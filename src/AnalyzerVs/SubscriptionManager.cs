using Microsoft.VisualStudio.Shell.TableManager;
using System;

namespace FSharpLintVs
{
    /// <summary>
    /// Every consumer of data from an <see cref="ITableDataSource"/> provides an <see cref="ITableDataSink"/> to record the changes. We give the consumer
    /// an IDisposable (this object) that they hang on to as long as they are interested in our data (and they Dispose() of it when they are done).
    /// </summary>
    public class SubscriptionManager : IDisposable
    {
        private readonly LintCheckerProvider _lintCheckerProvider;
        private readonly ITableDataSink _sink;

        public SubscriptionManager(LintCheckerProvider lintCheckerProvider, ITableDataSink sink)
        {
            _lintCheckerProvider = lintCheckerProvider;
            _sink = sink;

            lintCheckerProvider.AddSinkManager(this);
        }

        public void Add(LintChecker lintChecker)
        {
            _sink.AddFactory(lintChecker.Factory);
        }

        public void Remove(LintChecker lintChecker)
        {
            _sink.RemoveFactory(lintChecker.Factory);
        }

        public void Notify()
        {
            _sink.FactorySnapshotChanged(null);
        }

        public void Dispose()
        {
            // Called when the person who subscribed to the data source disposes of the cookie 
            // (== this object) they were given.
            _lintCheckerProvider.RemoveSinkManager(this);
        }
    }
}
