using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Utilities;
using System.ComponentModel.Composition;

namespace FSharpLintVs
{
    [Export(typeof(ISuggestedActionsSourceProvider))]
    [ContentType(ContentTypeNames.FSharpContentType)]
    [Name("F# Lint Suggested Actions")]
    public class LintSuggestionProvider : ISuggestedActionsSourceProvider
    {

        public ISuggestedActionsSource CreateSuggestedActionsSource(ITextView textView, ITextBuffer textBuffer)
        {
            return new LintActionsSource(textView, textBuffer);
        }
    }
}
