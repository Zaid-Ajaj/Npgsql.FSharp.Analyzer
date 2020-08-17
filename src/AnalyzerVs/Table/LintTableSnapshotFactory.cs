using Microsoft.VisualStudio.Shell.TableManager;

namespace FSharpLintVs
{
    public class LintTableSnapshotFactory : TableEntriesSnapshotFactoryBase
    {

        public LintingErrorsSnapshot CurrentSnapshot { get; private set; }

        public LintTableSnapshotFactory(LintingErrorsSnapshot lintErrors)
        {
            this.CurrentSnapshot = lintErrors;
        }

        public void UpdateErrors(LintingErrorsSnapshot lintingErrors)
        {
            this.CurrentSnapshot.NextSnapshot = lintingErrors;
            this.CurrentSnapshot = lintingErrors;
        }

        #region ITableEntriesSnapshotFactory members
        
        public override int CurrentVersionNumber => this.CurrentSnapshot.VersionNumber;
        
        public override ITableEntriesSnapshot GetCurrentSnapshot() => this.CurrentSnapshot;

        public override void Dispose()
        {
        }

        public override ITableEntriesSnapshot GetSnapshot(int versionNumber)
        {
            // In theory the snapshot could change in the middle of the return statement so snap the snapshot just to be safe.
            var snapshot = this.CurrentSnapshot;
            return (versionNumber == snapshot.VersionNumber) ? snapshot : null;
        }

        #endregion
    }
}
