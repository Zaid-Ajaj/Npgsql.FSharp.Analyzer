using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Adornments;
using Microsoft.VisualStudio.Text.Tagging;
using System;
using System.Collections.Generic;

namespace FSharpLintVs
{
    public class LintTagger : ITagger<IErrorTag>, IDisposable
    {
        private readonly LintChecker _lintChecker;
        private LintingErrorsSnapshot _snapshot;

        public LintTagger(LintChecker lintChecker)
        {
            _lintChecker = lintChecker;
            _snapshot = lintChecker.LastErrorsSnapshot;

            lintChecker.AddTagger(this);
        }

        public event EventHandler<SnapshotSpanEventArgs> TagsChanged;

        public void UpdateErrors(ITextSnapshot currentSnapshot, LintingErrorsSnapshot lintingErrors)
        {
            var oldLintingErrors = _snapshot;
            _snapshot = lintingErrors;


            // Raise a single tags changed event over the span that could have been affected by the change in the errors.
            var start = int.MaxValue;
            var end = int.MinValue;

            if (oldLintingErrors?.Errors.Count > 0)
            {
                start = oldLintingErrors.Errors[0].Span.Start.TranslateTo(currentSnapshot, PointTrackingMode.Negative);
                end = oldLintingErrors.Errors[oldLintingErrors.Errors.Count - 1].Span.End.TranslateTo(currentSnapshot, PointTrackingMode.Positive);
            }

            if (lintingErrors.Count > 0)
            {
                start = Math.Min(start, lintingErrors.Errors[0].Span.Start.Position);
                end = Math.Max(end, lintingErrors.Errors[lintingErrors.Errors.Count - 1].Span.End.Position);
            }

            if (start < end)
            {
                TagsChanged?.Invoke(this, new SnapshotSpanEventArgs(new SnapshotSpan(currentSnapshot, Span.FromBounds(start, end))));
            }
        }

        public IEnumerable<ITagSpan<IErrorTag>> GetTags(NormalizedSnapshotSpanCollection spans)
        {
            if (_snapshot != null)
            {
                foreach (var error in _snapshot.Errors)
                {
                    if (spans.IntersectsWith(error.Span))
                    {
                        yield return new TagSpan<IErrorTag>(error.Span, new ErrorTag(PredefinedErrorTypeNames.Warning, error.Tooltip) { });
                    }
                }
            }
        }

        public void Dispose()
        {
            // Called when the tagger is no longer needed (generally when the ITextView is closed).
            _lintChecker.RemoveTagger(this);
        }

    }
}
