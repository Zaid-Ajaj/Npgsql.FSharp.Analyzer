using FSharp.Compiler;
using FSharp.Compiler.Text;
using Npgsql.FSharp.Analyzers.Core;
using Microsoft.FSharp.Control;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Threading;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.FSharp.Core;

namespace NpgsqlFSharpVs
{
    ///<summary>
    /// Finds the linting errors in comments for a specific buffer.
    ///</summary>
    /// <remarks><para>The lifespan of this object is tied to the lifespan of the taggers on the view. On creation of the first tagger, the LintChecker starts doing
    /// work to find errors in the file. On the disposal of the last tagger, it shuts down.</para>
    /// </remarks>
    public class LintChecker : IDisposable
    {
        private readonly LintCheckerProvider _provider;
        private readonly ITextBuffer _buffer;

        private ITextSnapshot _currentSnapshot;
        private NormalizedSnapshotSpanCollection _dirtySpans;

        private readonly ITextDocument _document;
        private readonly List<LintTagger> _activeTaggers = new List<LintTagger>();
        private CancellationTokenSource _cts;

        public Task Linting { get; private set; }

        public LintProjectInfo Project { get; private set; }

        public int RefCount => _activeTaggers.Count;

        public LintingErrorsSnapshot LastErrorsSnapshot { get; private set; }

        public bool HasSnapshot => LastErrorsSnapshot != null;

        public event EventHandler Updated;

        public bool IsDisposed { get; private set; }

        public LintTableSnapshotFactory Factory { get; }

        public LintChecker(LintCheckerProvider provider, ITextView textView, ITextBuffer buffer)
        {
            _provider = provider;
            _buffer = buffer;
            _currentSnapshot = buffer.CurrentSnapshot;

            // Get the name of the underlying document buffer
            if (provider.TextDocumentFactoryService.TryGetTextDocument(textView.TextDataModel.DocumentBuffer, out ITextDocument document))
            {
                this._document = document;
            }

            this.Factory = new LintTableSnapshotFactory(new LintingErrorsSnapshot(document, version: 0));
        }

        public void AddTagger(LintTagger tagger)
        {
            if (RefCount == 0)
            {
                Initialize();
                RunLinter();
            }

            _activeTaggers.Add(tagger);
        }

        public void Initialize()
        {
            _buffer.ChangedLowPriority += this.OnBufferChange;

            _dirtySpans = new NormalizedSnapshotSpanCollection(new SnapshotSpan(_currentSnapshot, 0, _currentSnapshot.Length));

            _provider.AddLintChecker(this);
        }

        public void RemoveTagger(LintTagger tagger)
        {
            _activeTaggers.Remove(tagger);

            if (RefCount== 0)
            {
                Dispose();
            }
        }

        public void Dispose()
        {
            // Last tagger was disposed of. This is means there are no longer any open views on the buffer so we can safely shut down
            // lint checking for that buffer.
            _buffer.ChangedLowPriority -= this.OnBufferChange;

            _provider.RemoveLintChecker(this);

            IsDisposed = true;

            _buffer.Properties.RemoveProperty(typeof(LintChecker));
        }

        private void OnBufferChange(object sender, TextContentChangedEventArgs e)
        {
            _currentSnapshot = e.After;

            // Translate all of the old dirty spans to the new snapshot.
            NormalizedSnapshotSpanCollection newDirtySpans = _dirtySpans.CloneAndTrackTo(e.After, SpanTrackingMode.EdgeInclusive);

            // Dirty all the spans that changed.
            foreach (var change in e.Changes)
            {
                newDirtySpans = NormalizedSnapshotSpanCollection.Union(newDirtySpans, new NormalizedSnapshotSpanCollection(e.After, change.NewSpan));
            }

            // Translate all the linting errors to the new snapshot (and remove anything that is a dirty region since we will need to check that again).
            var oldErrors = this.Factory.CurrentSnapshot;
            var newErrors = new LintingErrorsSnapshot(oldErrors.Document, oldErrors.VersionNumber + 1);

            // Copy all of the old errors to the new errors unless the error was affected by the text change
            foreach (var error in oldErrors.Errors)
            {
                Debug.Assert(error.NextIndex == -1);

                var newError = LintError.CloneAndTranslateTo(error, e.After);

                if (newError != null)
                {
                    Debug.Assert(newError.Span.Length == error.Span.Length);

                    error.NextIndex = newErrors.Errors.Count;
                    newErrors.Errors.Add(newError);
                }
            }

            this.UpdateLintingErrors(newErrors);

            _dirtySpans = newDirtySpans;

            // Start processing the dirty spans (which no-ops if we're already doing it).
            if (_dirtySpans.Count != 0)
            {
                this.RunLinter();
            }
        }

        private void RunLinter()
        {
            // We're assuming we will only be called from the UI thread so there should be no issues with race conditions.
            _cts?.Cancel();
            _cts = new CancellationTokenSource();
            this.Linting = Task.Run(() => DoUpdateAsync());
        }

        public async Task DoUpdateAsync()
        {
            if (IsDisposed)
            {
                return;
            }

            var buffer = _currentSnapshot;
            var path = _document.FilePath;

            // replace with user token
            var token = _cts.Token;
            var instance = await FsLintVsPackage.Instance.WithCancellation(token);
            
            // this acts as a throttle 
            await Task.Delay(200, token);

            if (Project == null)
            {
                await instance.JoinableTaskFactory.SwitchToMainThreadAsync();
                var project = instance.Dte.Solution.FindProjectItem(path)?.ContainingProject;

                if (project == null)
                    return;

                if (instance.SolutionService.GetProjectOfUniqueName(project.UniqueName, out var vsHierarchy) != VSConstants.S_OK)
                    return;

                if (instance.SolutionService.GetGuidOfProject(vsHierarchy, out var guid) != VSConstants.S_OK)
                    return;

                Project = new LintProjectInfo(project.Name, guid, vsHierarchy);
            }

            await Task.Yield();

            var connectionString = SqlAnalyzer.tryFindConnectionString(path);

            if (string.IsNullOrWhiteSpace(connectionString))
            {
                return;
            }

            var loadedSchema = SqlAnalysis.databaseSchema(connectionString);

            if (loadedSchema.IsError)
            {
                return;
            }

            var source = _currentSnapshot.GetText();
            var sourceText = SourceText.ofString(source);

            var getProjectOptions = _provider.CheckerInstance.GetProjectOptionsFromScript(
                filename: path,
                sourceText: sourceText,
                assumeDotNetFramework: false,
                useSdkRefs: true,
                useFsiAuxLib: true,
                previewEnabled: true,
                otherFlags: new string[] { "--targetprofile:netstandard" },
                loadedTimeStamp: FSharpOption<DateTime>.None,
                extraProjectInfo: FSharpOption<object>.None,
                optionsStamp: FSharpOption<long>.None,
                userOpName: FSharpOption<string>.None
            );

            var (options, errors) = await FSharpAsync.StartAsTask(getProjectOptions, null, token);

            if (errors.Any())
            {
                return;
            }

            var performParseAndCheck = _provider.CheckerInstance.ParseAndCheckFileInProject(
                filename: path, 
                fileversion: 1,
                sourceText: sourceText,
                options: options, 
                textSnapshotInfo: FSharpOption<object>.None,
                userOpName: FSharpOption<string>.None
            );

            var (parseResults, checkAnswer) = await FSharpAsync.StartAsTask(performParseAndCheck, null, token);

            if (parseResults.ParseHadErrors || checkAnswer.IsAborted)
            {
                return;
            }

            var checkResults = SqlAnalyzer.checkAnswerResult(checkAnswer).Value;

            var context = new SqlAnalyzerContext(
                fileName: path,
                content: source.Split('\n'),
                parseTree: parseResults.ParseTree.Value,
                symbols: SqlAnalyzer.getSymbols(checkResults)
            );

            var databaseSchema = loadedSchema.ResultValue;

            var errorMessages =
               from operation in SyntacticAnalysis.findSqlOperations(context)
               from analysisOutput in SqlAnalysis.analyzeOperation(operation, connectionString, databaseSchema)
               select analysisOutput;

            var oldLintingErrors = this.Factory.CurrentSnapshot;
            var newLintErrors = new LintingErrorsSnapshot(_document, oldLintingErrors.VersionNumber + 1);

            foreach (var error in errorMessages)
            {
                var span = RangeToSpan(error.Range, buffer);
                newLintErrors.Errors.Add(new LintError(span, error, Project));
            }

            await instance.JoinableTaskFactory.SwitchToMainThreadAsync();
            if (token.IsCancellationRequested)
                return;

            UpdateLintingErrors(newLintErrors);
        }

        public static SnapshotSpan RangeToSpan(Range.range fsrange, ITextSnapshot textSnapshot)
        {
            var from = fsrange.StartLine - 1;
            ITextSnapshotLine anchor = textSnapshot.GetLineFromLineNumber(from);
            var start = anchor.Start.Position + fsrange.StartColumn;
            var to = fsrange.EndLine - 1;
            var end = textSnapshot.GetLineFromLineNumber(to).Start.Position + fsrange.EndColumn;
            return new SnapshotSpan(textSnapshot, new Span(start, end - start));
        }

        public static ITrackingSpan RangeToTrackingSpan(Range.range fsrange, ITextSnapshot textSnapshot)
        {
            var from = fsrange.StartLine - 1;
            ITextSnapshotLine anchor = textSnapshot.GetLineFromLineNumber(from);
            var start = anchor.Start.Position + fsrange.StartColumn;
            var to = fsrange.EndLine - 1;
            var end = textSnapshot.GetLineFromLineNumber(to).Start.Position + fsrange.EndColumn;
            return textSnapshot.CreateTrackingSpan(start, end - start, SpanTrackingMode.EdgeExclusive);
        }

        private void UpdateLintingErrors(LintingErrorsSnapshot lintSnapshot)
        {
            // Tell our factory to snap to a new snapshot.
            this.Factory.UpdateErrors(lintSnapshot);

            // Tell the provider to mark all the sinks dirty (so, as a side-effect, they will start an update pass that will get the new snapshot
            // from the factory).
            _provider.NotifyAllSinks();

            foreach (var tagger in _activeTaggers)
            {
                tagger.UpdateErrors(_currentSnapshot, lintSnapshot);
            }

            this.LastErrorsSnapshot = lintSnapshot;
            Updated?.Invoke(this, EventArgs.Empty);
        }

    }
}
