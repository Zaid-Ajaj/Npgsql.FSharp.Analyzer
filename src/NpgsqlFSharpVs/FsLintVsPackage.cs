using System;
using System.Diagnostics;
using System.Runtime.InteropServices;
using System.Threading;
using System.Threading.Tasks;
using EnvDTE80;
using Microsoft.VisualStudio.ComponentModelHost;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Task = System.Threading.Tasks.Task;

namespace NpgsqlFSharpVs
{
    // DO NOT REMOVE THIS MAGICAL INCANTATION NO MATTER HOW MUCH VS WARNS YOU OF DEPRECATION    
    // --------------------------------------------------------------------------------------
    [InstalledProductRegistration("F# Npgsql Analyzer", "Static SQL analyzer with Npgsql.FSharp", "0.1", IconResourceID = 400)]
    // --------------------------------------------------------------------------------------

    // Package registration attributes
    [PackageRegistration(UseManagedResourcesOnly = true, AllowsBackgroundLoading = true)]
    [Guid(FsLintVsPackage.PackageGuidString)]

    // Auto load only if a solution is open, this is important too
    [ProvideAutoLoad(UIContextGuids80.SolutionExists, PackageAutoLoadFlags.BackgroundLoad)]

    // Options page
    [ProvideOptionPage(typeof(FsLintOptionsPage), "F# Tools", "Linting", 0, 0, supportsAutomation: true)]

    public sealed partial class FsLintVsPackage : AsyncPackage
    {
        /// <summary>
        /// FsLintVsPackage GUID string.
        /// </summary>
        public const string PackageGuidString = "74927147-72e8-4b47-a80d-5568807d6879";

        private static readonly TaskCompletionSource<FsLintVsPackage> _instance = new TaskCompletionSource<FsLintVsPackage>();
        public static Task<FsLintVsPackage> Instance => _instance.Task;

        public FsLintOptionsPage Options => GetDialogPage(typeof(FsLintOptionsPage)) as FsLintOptionsPage ?? new FsLintOptionsPage();

        public IComponentModel MefHost { get; private set; }

        public IVsStatusbar Statusbar { get; private set; }

        public DTE2 Dte { get; private set; }
        
        public IVsSolution SolutionService { get; private set; }

        #region Package Members

        /// <summary>
        /// Initialization of the package; this method is called right after the package is sited, so this is the place
        /// where you can put all the initialization code that rely on services provided by VisualStudio.
        /// </summary>
        /// <param name="cancellationToken">A cancellation token to monitor for initialization cancellation, which can occur when VS is shutting down.</param>
        /// <param name="progress">A provider for progress updates.</param>
        /// <returns>A task representing the async work of package initialization, or an already completed task if there is none. Do not return null from this method.</returns>
        protected override async Task InitializeAsync(CancellationToken cancellationToken, IProgress<ServiceProgressData> progress)
        {
            Trace.WriteLine("F# Lint Vs Package Loaded");

            MefHost = await this.GetServiceAsync<SComponentModel, IComponentModel>();
            Statusbar = await this.GetServiceAsync<SVsStatusbar, IVsStatusbar>();
            Dte = await this.GetServiceAsync<SDTE, DTE2>();
            SolutionService = await this.GetServiceAsync<SVsSolution, IVsSolution>();
                        
            // When initialized asynchronously, the current thread may be a background thread at this point.
            // Do any initialization that requires the UI thread after switching to the UI thread.
            await JoinableTaskFactory.SwitchToMainThreadAsync(cancellationToken);

            // signal that package is ready
            _instance.SetResult(this);
        }

        #endregion
    }
}
