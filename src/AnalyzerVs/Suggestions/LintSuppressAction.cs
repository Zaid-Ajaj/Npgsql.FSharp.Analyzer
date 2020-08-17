using Microsoft.VisualStudio.Imaging;
using Microsoft.VisualStudio.Imaging.Interop;
using Microsoft.VisualStudio.Language.Intellisense;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using System.Windows;

namespace FSharpLintVs
{
    public class LintSuppressAction : ISuggestedAction
    {
        private readonly LintError error;

        public LintSuppressAction(LintError error)
        {
            this.error = error;
        }

        public string DisplayText => $"Suppress {error.Identifier}";

        public ImageMoniker IconMoniker => KnownMonikers.CodeSuppressedRule;

        public string IconAutomationText => default;

        public string InputGestureText => default;

        public bool HasPreview => false;

        public bool HasActionSets => true;

        public Task<IEnumerable<SuggestedActionSet>> GetActionSetsAsync(CancellationToken cancellationToken)
        {
            return Task.FromResult(new SuggestedActionSet[]
            {
                new SuggestedActionSet(
                    categoryName: PredefinedSuggestedActionCategoryNames.CodeFix,
                    actions: GetSuggestedActions(),
                    title: "FsLint",
                    priority: SuggestedActionSetPriority.None,
                    applicableToSpan: null
                )
            }.AsEnumerable());
        }

        public IEnumerable<ISuggestedAction> GetSuggestedActions()
        {
            yield return new LintSuppressBy(error, LintSuppressBy.Method.Above);
            yield return new LintSuppressBy(error, LintSuppressBy.Method.Inline);
            yield return new LintSuppressBy(error, LintSuppressBy.Method.Section);
        }

        public Task<object> GetPreviewAsync(CancellationToken cancellationToken) => throw new NotImplementedException();

        public void Invoke(CancellationToken cancellationToken)
        {
            GetSuggestedActions().First().Invoke(cancellationToken);
        }

        public bool TryGetTelemetryId(out Guid telemetryId)
        {
            telemetryId = default;
            return false;
        }

        public void Dispose()
        {
            // nothing 
        }
    }


}