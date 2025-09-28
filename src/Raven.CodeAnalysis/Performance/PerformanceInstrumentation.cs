using System.Threading;

namespace Raven.CodeAnalysis;

public sealed class PerformanceInstrumentation
{
    public static PerformanceInstrumentation Disabled { get; } = new PerformanceInstrumentation(isEnabled: false);

    public PerformanceInstrumentation()
        : this(true)
    {
    }

    private PerformanceInstrumentation(bool isEnabled)
    {
        LambdaReplay = new LambdaReplayInstrumentation(isEnabled);
    }

    public LambdaReplayInstrumentation LambdaReplay { get; }
}

public sealed class LambdaReplayInstrumentation
{
    private readonly bool _isEnabled;
    private long _replayAttempts;
    private long _replaySuccesses;
    private long _replayFailures;
    private long _cacheHits;
    private long _cacheMisses;
    private long _bindingInvocations;
    private long _bindingSuccesses;
    private long _bindingFailures;

    internal LambdaReplayInstrumentation(bool isEnabled)
    {
        _isEnabled = isEnabled;
    }

    public long ReplayAttempts => Volatile.Read(ref _replayAttempts);

    public long ReplaySuccesses => Volatile.Read(ref _replaySuccesses);

    public long ReplayFailures => Volatile.Read(ref _replayFailures);

    public long CacheHits => Volatile.Read(ref _cacheHits);

    public long CacheMisses => Volatile.Read(ref _cacheMisses);

    public long BindingInvocations => Volatile.Read(ref _bindingInvocations);

    public long BindingSuccesses => Volatile.Read(ref _bindingSuccesses);

    public long BindingFailures => Volatile.Read(ref _bindingFailures);

    internal void RecordReplayAttempt()
    {
        if (!_isEnabled)
            return;

        Interlocked.Increment(ref _replayAttempts);
    }

    internal void RecordReplaySuccess()
    {
        if (!_isEnabled)
            return;

        Interlocked.Increment(ref _replaySuccesses);
    }

    internal void RecordReplayFailure()
    {
        if (!_isEnabled)
            return;

        Interlocked.Increment(ref _replayFailures);
    }

    internal void RecordCacheHit()
    {
        if (!_isEnabled)
            return;

        Interlocked.Increment(ref _cacheHits);
    }

    internal void RecordCacheMiss()
    {
        if (!_isEnabled)
            return;

        Interlocked.Increment(ref _cacheMisses);
    }

    internal void RecordBindingInvocation()
    {
        if (!_isEnabled)
            return;

        Interlocked.Increment(ref _bindingInvocations);
    }

    internal void RecordBindingSuccess()
    {
        if (!_isEnabled)
            return;

        Interlocked.Increment(ref _bindingSuccesses);
    }

    internal void RecordBindingFailure()
    {
        if (!_isEnabled)
            return;

        Interlocked.Increment(ref _bindingFailures);
    }
}
