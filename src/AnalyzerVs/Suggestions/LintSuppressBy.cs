using Microsoft.VisualStudio.Imaging;
using Microsoft.VisualStudio.Imaging.Interop;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using System.Windows;

namespace FSharpLintVs
{
    public class LintSuppressBy : ISuggestedAction
    {
        public enum Method
        {
            Inline,
            Above,
            Section,
            Global
        }

        private readonly LintError _error;
        private readonly Method _suppressionMethod;

        public LintSuppressBy(LintError error, Method suppressionMethod)
        {
            this._error = error;
            this._suppressionMethod = suppressionMethod;
        }

        public static string ToText(Method suppressionMethod)
        {
            switch (suppressionMethod)
            {
                case Method.Inline:
                    return "inline";
                case Method.Above:
                    return "above";
                case Method.Section:
                    return "section";
                default:
                    return "...";
            }
        }

        public string DisplayText => $"Suppress {ToText(_suppressionMethod)}";

        public ImageMoniker IconMoniker => _suppressionMethod switch 
	    {
            Method.Above => KnownMonikers.GlyphUp,
            Method.Section => KnownMonikers.Inline,
            Method.Inline => KnownMonikers.GoToCurrentLine,
		    _ => default
	    };

        public string IconAutomationText => default;

        public string InputGestureText => default;

        public bool HasActionSets => false;

        public Task<IEnumerable<SuggestedActionSet>> GetActionSetsAsync(CancellationToken cancellationToken)
        {
            throw new NotSupportedException();
        }

        public bool HasPreview => false;

        public Task<object> GetPreviewAsync(CancellationToken cancellationToken) => throw new NotImplementedException();

        public void Invoke(CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
                return;

            var snapshot = _error.Span.Snapshot;

            switch (_suppressionMethod)
            {
                case Method.Inline:
                    {
                        var line = _error.Span.Start.GetContainingLine();
                        if (line != null)
                            snapshot.TextBuffer.Insert(line.End.Position, $" // fsharplint:disable-line {_error.Name}");
                        break;
                    }
                case Method.Above:
                    {
                        var line = snapshot.GetLineFromLineNumber(_error.Line - 1);
                        var indent = snapshot.GetLineFromLineNumber(_error.Line).GetText().TakeWhile(Char.IsWhiteSpace).Count();
                        if (line != null)
                            snapshot.TextBuffer.Insert(line.End.Position, $"{Environment.NewLine}{new String(' ', indent)}// fsharplint:disable-next-line {_error.Name}");
                        break;
                    }
                case Method.Section:
                    {
                        var containingLine = FindSection().FirstOrDefault();
                        if (containingLine == null)
                        {
                            snapshot = _error.Span.Snapshot.TextBuffer.Insert(0, $"// fsharplint:disable{Environment.NewLine}");
                            containingLine = snapshot.GetLineFromLineNumber(0);
                        }

                        snapshot.TextBuffer.Insert(containingLine.End, $" {_error.Name}");
                        break;
                    }
                default:
                    break;
            }
        }

        public IEnumerable<ITextSnapshotLine> FindSection()
        {
            var cursor = _error.Line;
            var snapshot = _error.Span.Snapshot;

            while (cursor-- >= 0)
            {
                var line = snapshot.GetLineFromLineNumber(cursor);
                if (line.GetText().StartsWith("// fsharplint:disable "))
                    yield return line;
            }
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