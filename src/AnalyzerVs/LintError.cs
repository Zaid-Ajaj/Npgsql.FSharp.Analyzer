using Npgsql.FSharp.Analyzers.Core;
using Microsoft.FSharp.Core;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Utilities;
using System.Collections.Generic;
using System.Text;
using System.Linq;

namespace FSharpLintVs
{
    public class LintError
    {
        public readonly SnapshotSpan Span;
        public readonly LintProjectInfo Project;
        private readonly Message LintWarning;

        public int NextIndex = -1;

        public string Tooltip => $"{LintWarning.Code}: {LintWarning.Message}";

        public string Identifier => LintWarning.Type;

        public string Name => LintWarning.Code;

        public string Message => LintWarning.Message;

        public string HelpUrl => $"https://fsprojects.github.io/FSharpLint/how-tos/rules/{Identifier}.html";

        public bool HasSuggestedFix => LintWarning.Fixes.Any();

        public Fix GetSuggestedFix() => LintWarning.Fixes.First();

        public int Line => LintWarning.Range.StartLine - 1;

        public int Column => LintWarning.Range.StartColumn;

        public string ErrorText => LintWarning.Message;

        public string ReplacementText
        {
            get
            {
                var fix = GetSuggestedFix();
                if (fix == null)
                    return "";

                var startColumn = fix.FromRange.StartColumn;
                return ErrorText.Remove(startColumn, fix.FromRange.EndColumn - startColumn).Insert(startColumn, fix.ToText);
            }
        }

        public LintError(SnapshotSpan span, Message lintWarning, LintProjectInfo project)
        {
            this.Span = span;
            this.LintWarning = lintWarning;
            this.Project = project;
        }

        public static LintError Clone(LintError error)
        {
            return new LintError(error.Span, error.LintWarning, error.Project);
        }

        public static LintError CloneAndTranslateTo(LintError error, ITextSnapshot newSnapshot)
        {
            var newSpan = error.Span.TranslateTo(newSnapshot, SpanTrackingMode.EdgeExclusive);

            // We want to only translate the error if the length of the error span did not change (if it did change, it would imply that
            // there was some text edit inside the error and, therefore, that the error is no longer valid).
            return (newSpan.Length == error.Span.Length)
                   ? new LintError(newSpan, error.LintWarning, error.Project)
                   : null;
        }

    }
}
