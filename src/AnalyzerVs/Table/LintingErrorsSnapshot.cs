using Microsoft.Internal.VisualStudio.Shell.TableControl;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Shell.TableControl;
using Microsoft.VisualStudio.Shell.TableManager;
using Microsoft.VisualStudio.Text;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.Windows;
using System.Windows.Documents;

namespace FSharpLintVs
{
    public class LintingErrorsSnapshot : WpfTableEntriesSnapshotBase
    {
        public ITextDocument Document { get; }

        public override int VersionNumber { get; }
        
        public override int Count => this.Errors.Count;

        public const string ErrorCategory = "Lint";
        public const string ToolName = "FSharpLint";

        // We're not using an immutable list here but we cannot modify the list in any way once we've published the snapshot.
        public List<LintError> Errors { get; }

        public LintingErrorsSnapshot NextSnapshot;

        public LintingErrorsSnapshot(ITextDocument document, int version)
        {
            Document = document;
            VersionNumber = version;
            Errors = new List<LintError>();
        }

        public override int IndexOf(int currentIndex, ITableEntriesSnapshot newerSnapshot)
        {
            // This and TranslateTo() are used to map errors from one snapshot to a different one (that way the error list can do things like maintain the selection on an error
            // even when the snapshot containing the error is replaced by a new one).
            //
            // You only need to implement Identity() or TranslateTo() and, of the two, TranslateTo() is more efficient for the error list to use.

            // Map currentIndex to the corresponding index in newerSnapshot (and keep doing it until either
            // we run out of snapshots, we reach newerSnapshot, or the index can no longer be mapped forward).
            var currentSnapshot = this;
            do
            {
                Debug.Assert(currentIndex >= 0);
                Debug.Assert(currentIndex < currentSnapshot.Count);

                currentIndex = currentSnapshot.Errors[currentIndex].NextIndex;

                currentSnapshot = currentSnapshot.NextSnapshot;
            }
            while ((currentSnapshot != null) && (currentSnapshot != newerSnapshot) && (currentIndex >= 0));

            return currentIndex;
        }

        public override bool TryGetValue(int index, string columnName, out object content)
        {
            if (index >= 0 && index < this.Errors.Count)
            {
                var err = this.Errors[index];

                switch (columnName)
                {
                    case StandardTableKeyNames.DocumentName:
                        {
                            // We return the full file path here. The UI handles displaying only the Path.GetFileName().
                            content = Document.FilePath;
                            return true;
                        }

                    case StandardTableKeyNames.ErrorCategory:
                        {
                            content = ErrorCategory;
                            return true;
                        }

                    case StandardTableKeyNames.ErrorSource:
                        {
                            content = ErrorSource.Other;
                            return true;
                        }

                    case StandardTableKeyNames.Line:
                        {
                            // Line and column numbers are 0-based (the UI that displays the line/column number will add one to the value returned here).
                            content = err.Line;
                            return true;
                        }

                    case StandardTableKeyNames.Column:
                        {
                            content = err.Column;
                            return true;
                        }

                    case StandardTableKeyNames.Text:
                        {
                            content = err.Message;
                            return true;
                        }

                    case StandardTableKeyNames2.TextInlines:
                        {
                            //content = "?";
                            // Do we have detailed inline text?
                            content = "";
                            return false;
                        }

                    case StandardTableKeyNames.ErrorSeverity:
                        {
                            content = __VSERRORCATEGORY.EC_WARNING;
                            return true;
                        }

                    case StandardTableKeyNames.BuildTool:
                        {
                            content = ToolName;
                            return true;
                        }

                    case StandardTableKeyNames.ErrorCode:
                        {
                            content = err.Identifier;
                            return true;
                        }

                    case StandardTableKeyNames.ErrorCodeToolTip:
                        {
                            content = err.Name;
                            return true;
                        }

                    case StandardTableKeyNames.HelpLink:
                        {
                            content = err.HelpUrl;
                            return true;
                        }

                    case StandardTableKeyNames.ProjectName:
                        {
                            content = err.Project.ProjectName;
                            return true;
                        }

                    case StandardTableKeyNames.ProjectGuid:
                        {
                            content = err.Project.ProjectGuid;
                            return true;
                        }

                        // TODO: add support for case StandardTableKeyNames.ProjectGuid:
                }

            }

            content = null;
            return false;
        }

        public override bool CanCreateDetailsContent(int index)
        {
            return false;
        }

        public override bool TryCreateDetailsStringContent(int index, out string content)
        {
            content = this.Errors[index].Tooltip;
            return content != null;
        }
    }
}
