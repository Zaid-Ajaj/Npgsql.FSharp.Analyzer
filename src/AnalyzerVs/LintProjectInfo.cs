using Microsoft.VisualStudio.Shell.Interop;
using System;

namespace FSharpLintVs
{
    public class LintProjectInfo
    {
        public string ProjectName { get; }


        // Performance will be improved if you "prebox" your System.Guid by, 
        // in your Microsoft.VisualStudio.Shell.TableManager.ITableEntry
        // or Microsoft.VisualStudio.Shell.TableManager.ITableEntriesSnapshot, having a
        // member variable:
        // private object boxedProjectGuid = projectGuid;
        // and returning boxedProjectGuid instead of projectGuid.
        public object ProjectGuid { get; }

        public IVsHierarchy Hierarchy { get; }

        public LintProjectInfo(string projectName, Guid projectGuid, IVsHierarchy hierarchy)
        {
            ProjectName = projectName;
            ProjectGuid = projectGuid;
            Hierarchy = hierarchy;
        }

    }
}
