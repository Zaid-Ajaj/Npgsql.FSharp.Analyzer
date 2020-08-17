using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio.Threading;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Net.Http.Headers;
using System.Threading;
using System.Threading.Tasks;

namespace FSharpLintVs
{
    public class LintActionsSource : ISuggestedActionsSource
    {
        private readonly ITextBuffer _textBuffer;
        private readonly ITextView _textView;
        private LintChecker _lintChecker;

        public LintActionsSource(ITextView textView, ITextBuffer textBuffer)
        {
            _textBuffer = textBuffer;
            _textView = textView;
        }

#pragma warning disable 0067
        public event EventHandler<EventArgs> SuggestedActionsChanged;
        protected void OnSuggestedActionsChanged(object sender, EventArgs e)
        {
            SuggestedActionsChanged?.Invoke(sender, e);
        }
#pragma warning restore 0067

        public void Dispose()
        {
            if (_lintChecker != null)
                _lintChecker.Updated -= OnSuggestedActionsChanged;
        }

        public IEnumerable<SuggestedActionSet> GetSuggestedActions(ISuggestedActionCategorySet requestedActionCategories, SnapshotSpan range, CancellationToken cancellationToken)
        {
            yield return new SuggestedActionSet(
                categoryName: PredefinedSuggestedActionCategoryNames.CodeFix,
                actions: GetSuggestedActions(range),
                title: "FsLint",
                priority: SuggestedActionSetPriority.None,
                applicableToSpan: null
            );
        }

        public IEnumerable<ISuggestedAction> GetSuggestedActions(SnapshotSpan range)
        {
            if (!TryGetLintChecker(out var lintChecker))
                yield break;

            if (!lintChecker.HasSnapshot)
                yield break;

            foreach (var error in lintChecker.LastErrorsSnapshot.Errors)
            {
                if (range.IntersectsWith(error.Span))
                {
                    if (error.HasSuggestedFix)
                        yield return new LintFixAction(error);

                    yield return new LintSuppressAction(error);
                }
            }
        }

        public Task<bool> HasSuggestedActionsAsync(ISuggestedActionCategorySet requestedActionCategories, SnapshotSpan range, CancellationToken cancellationToken)
        {
            return Task.Run(async () =>
            {
                if (!TryGetLintChecker(out var lintChecker))
                    return false;

                // wait for linting to complete
                await lintChecker.Linting.WithCancellation(cancellationToken);

                if (!lintChecker.HasSnapshot)
                    return false;

                // we can't actually traverse the to see if the suggested action is a Some (fix) or None
                // because we'd have to evaluate the lazy 
                return lintChecker.LastErrorsSnapshot.Count > 0;

            }, cancellationToken);
        }

        private bool TryGetLintChecker(out LintChecker checker)
        {
            // return cached value
            if (_lintChecker != null)
            {
                checker = _lintChecker;
                return true;
            }

            if (!_textBuffer.Properties.TryGetProperty(typeof(LintChecker), out checker))
                return false;

            if (checker.IsDisposed || checker.RefCount == 0 || checker.Linting == null)
                return false;

            // cache value
            _lintChecker = checker;
            _lintChecker.Updated += OnSuggestedActionsChanged;
            return true;
        }


        public bool TryGetTelemetryId(out Guid telemetryId)
        {
            telemetryId = Guid.Empty;
            return false;
        }

    }
}
