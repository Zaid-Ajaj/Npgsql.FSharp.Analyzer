using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;
using System.Windows;
using Microsoft.VisualStudio.Imaging;
using Microsoft.VisualStudio.Imaging.Interop;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using Npgsql.FSharp.Analyzers.Core;

namespace FSharpLintVs
{
    public class LintFixAction : ISuggestedAction
    {
        private readonly LintError _lintError;
        private readonly Fix _fix;

        public LintFixAction(LintError lintError)
        {
            this._lintError = lintError;
            this._fix = _lintError.GetSuggestedFix();
        }

        public string DisplayText => $"Replace with '{_fix.ToText}'";

        public string IconAutomationText => null;

        ImageMoniker ISuggestedAction.IconMoniker => KnownMonikers.CodeWarningRule;

        public string InputGestureText => null;

        public bool HasActionSets => false;

#pragma warning disable RCS1210 
        public Task<IEnumerable<SuggestedActionSet>> GetActionSetsAsync(CancellationToken cancellationToken) => default;
#pragma warning restore RCS1210 

        public bool HasPreview => true;

        public Task<object> GetPreviewAsync(CancellationToken cancellationToken)
        {
            var textBlock = new SuggestionPreview(this._lintError) { MaxWidth = 400, MinHeight = 100 };
            return Task.FromResult<object>(textBlock);
        }

        public void Invoke(CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
                return;

            var span = LintChecker.RangeToSpan(_fix.FromRange, _lintError.Span.Snapshot);
            span.Snapshot.TextBuffer.Replace(span, _fix.ToText);
        }

        public bool TryGetTelemetryId(out Guid telemetryId)
        {
            telemetryId = Guid.Empty;
            return false;
        }

        public void Dispose()
        {
            //nothing to do
        }
    }
}
