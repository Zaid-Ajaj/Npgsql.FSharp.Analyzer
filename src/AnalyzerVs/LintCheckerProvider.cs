using FSharp.Compiler.SourceCodeServices;
using FSharpLintVs;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Shell.TableControl;
using Microsoft.VisualStudio.Shell.TableManager;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Diagnostics;

namespace FSharpLintVs
{
    /// <summary>
    /// Factory for the <see cref="ITagger{T}"/> and <see cref="ITableDataSource"/>.
    /// There will be only one instance of this class created.
    /// </summary>
    [Export(typeof(IViewTaggerProvider))]
    [TagType(typeof(IErrorTag))]
    [ContentType(ContentTypeNames.FSharpContentType)]
    [TextViewRole(PredefinedTextViewRoles.Document)]
    [TextViewRole(PredefinedTextViewRoles.Analyzable)]
    public sealed class LintCheckerProvider : IViewTaggerProvider, ITableDataSource
    {
        public readonly ITableManager ErrorTableManager;
        public readonly ITextDocumentFactoryService TextDocumentFactoryService;

        public const string LintCheckerDataSource = "LintChecker";

        private readonly List<SubscriptionManager> _managers = new List<SubscriptionManager>();      // Also used for locks
        private readonly List<LintChecker> _lintCheckers = new List<LintChecker>();

        [ImportingConstructor]
        public LintCheckerProvider
        (
            [Import] ITableManagerProvider provider, 
            [Import] ITextDocumentFactoryService textDocumentFactoryService
        )
        {
            this.TextDocumentFactoryService = textDocumentFactoryService;
            this.ErrorTableManager = provider.GetTableManager(StandardTables.ErrorsTable);
            this.ErrorTableManager.AddSource(this, 
                StandardTableColumnDefinitions.DetailsExpander,
                StandardTableColumnDefinitions.ErrorSeverity,
                StandardTableColumnDefinitions.ErrorCode,
                StandardTableColumnDefinitions.ErrorSource,
                StandardTableColumnDefinitions.BuildTool,
                StandardTableColumnDefinitions.ErrorSource,
                StandardTableColumnDefinitions.ErrorCategory,
                StandardTableColumnDefinitions.Text,
                StandardTableColumnDefinitions.DocumentName,
                StandardTableColumnDefinitions.Line, 
                StandardTableColumnDefinitions.Column,
                StandardTableColumnDefinitions.ProjectName
            );
        }

        /// <summary>
        /// Create a tagger that does lint checking on the view/buffer combination.
        /// </summary>
        public ITagger<T> CreateTagger<T>(ITextView textView, ITextBuffer buffer) where T : ITag
        {
            ITagger<T> tagger = null;

            // Only attempt to lint check on the view's edit buffer (and multiple views could have that buffer open simultaneously so
            // only create one instance of the lint checker.
            if ((buffer == textView.TextBuffer) && (typeof(T) == typeof(IErrorTag)))
            {
                var lintChecker = buffer.Properties.GetOrCreateSingletonProperty(typeof(LintChecker), () => new LintChecker(this, textView, buffer));

                // This is a thin wrapper around the LintChecker that can be disposed of without shutting down the LintChecker
                // (unless it was the last tagger on the lint checker).
                tagger = new LintTagger(lintChecker) as ITagger<T>;
            }

            return tagger;
        }

        #region ITableDataSource members

        // This string should, in general, be localized since it is what would be displayed in any UI that lets the end user pick
        // which ITableDataSources should be subscribed to by an instance of the table control. It really isn't needed for the error
        // list however because it autosubscribes to all the ITableDataSources.
        public string DisplayName => "F# Lint";

        public string Identifier => LintCheckerDataSource;

        public string SourceTypeIdentifier => StandardTableDataSources.ErrorTableDataSource;

        // This is the observer pattern
        public IDisposable Subscribe(ITableDataSink sink)
        {
            // This method is called to each consumer interested in errors. In general, there will be only a single consumer (the error list tool window)
            // but it is always possible for 3rd parties to write code that will want to subscribe.
            return new SubscriptionManager(this, sink);
        }
        #endregion

        #region Checker

        private Lazy<FSharpChecker> _checker = new Lazy<FSharpChecker>(() =>
            FSharpChecker.Create(null, null, null, null, null, null, null, null)
        );

        public FSharpChecker CheckerInstance => _checker.Value;

        #endregion

        public void AddSinkManager(SubscriptionManager manager)
        {
            // This call can, in theory, happen from any thread so be appropriately thread safe.
            // In practice, it will probably be called only once from the UI thread (by the error list tool window).
            lock (_managers)
            {
                _managers.Add(manager);

                // Add the pre-existing lint checkers to the manager.
                foreach (var checker in _lintCheckers)
                {
                    manager.Add(checker);
                }
            }
        }

        public void RemoveSinkManager(SubscriptionManager manager)
        {
            // This call can, in theory, happen from any thread so be appropriately thread safe.
            // In practice, it will probably be called only once from the UI thread (by the error list tool window).
            lock (_managers)
            {
                _managers.Remove(manager);
            }
        }

        public void AddLintChecker(LintChecker lintChecker)
        {
            // This call will always happen on the UI thread (it is a side-effect of adding or removing the 1st/last tagger).
            lock (_managers)
            {
                _lintCheckers.Add(lintChecker);

                // Tell the preexisting managers about the new lint checker
                foreach (var manager in _managers)
                {
                    manager.Add(lintChecker);
                }
            }
        }

        public void RemoveLintChecker(LintChecker lintChecker)
        {
            // This call will always happen on the UI thread (it is a side-effect of adding or removing the 1st/last tagger).
            lock (_managers)
            {
                _lintCheckers.Remove(lintChecker);

                foreach (var manager in _managers)
                {
                    manager.Remove(lintChecker);
                }
            }
        }

        public void NotifyAllSinks()
        {
            lock (_managers)
            {
                foreach (var manager in _managers)
                {
                    manager.Notify();
                }
            }
        }

    }
}
