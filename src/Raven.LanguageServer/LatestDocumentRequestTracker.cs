using System.Collections.Concurrent;

using OmniSharp.Extensions.LanguageServer.Protocol;

namespace Raven.LanguageServer;

internal sealed class LatestDocumentRequestTracker
{
    private readonly ConcurrentDictionary<string, LatestDocumentRequestState> _latestRequests = new();
    private long _requestSequence;

    public LatestDocumentRequestState Begin(DocumentUri uri, CancellationToken cancellationToken)
    {
        var key = uri.ToString();
        var sequence = Interlocked.Increment(ref _requestSequence);
        var state = new LatestDocumentRequestState(
            key,
            sequence,
            CancellationTokenSource.CreateLinkedTokenSource(cancellationToken));

        _latestRequests.AddOrUpdate(
            key,
            state,
            (_, previous) =>
            {
                previous.CancelAsSuperseded();
                return state;
            });

        return state;
    }

    public void Complete(LatestDocumentRequestState state)
    {
        if (_latestRequests.TryGetValue(state.Key, out var latest) &&
            ReferenceEquals(latest, state))
        {
            ((ICollection<KeyValuePair<string, LatestDocumentRequestState>>)_latestRequests)
                .Remove(new KeyValuePair<string, LatestDocumentRequestState>(state.Key, state));
        }

        state.Dispose();
    }
}

internal sealed class LatestDocumentRequestState : IDisposable
{
    private int _isSuperseded;
    private readonly CancellationTokenSource _cancellationTokenSource;

    public LatestDocumentRequestState(
        string key,
        long sequence,
        CancellationTokenSource cancellationTokenSource)
    {
        Key = key;
        Sequence = sequence;
        _cancellationTokenSource = cancellationTokenSource;
    }

    public string Key { get; }

    public long Sequence { get; }

    public CancellationToken Token => _cancellationTokenSource.Token;

    public bool IsSuperseded => Volatile.Read(ref _isSuperseded) != 0;

    public void CancelAsSuperseded()
    {
        if (Interlocked.Exchange(ref _isSuperseded, 1) == 0)
            _cancellationTokenSource.Cancel();
    }

    public void Dispose()
    {
        _cancellationTokenSource.Dispose();
    }
}
