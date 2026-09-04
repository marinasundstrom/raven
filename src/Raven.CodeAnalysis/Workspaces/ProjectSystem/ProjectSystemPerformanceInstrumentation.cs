using System.Diagnostics;
using System.Threading;

namespace Raven.CodeAnalysis;

public sealed class ProjectSystemPerformanceInstrumentation
{
    private long _evaluationRequests;
    private long _evaluations;
    private long _evaluationCacheHits;
    private long _evaluationCacheInvalidations;
    private long _evaluationFailures;
    private long _evaluationElapsedTicks;

    public ProjectSystemPerformanceSnapshot CaptureSnapshot()
        => new(
            Interlocked.Read(ref _evaluationRequests),
            Interlocked.Read(ref _evaluations),
            Interlocked.Read(ref _evaluationCacheHits),
            Interlocked.Read(ref _evaluationCacheInvalidations),
            Interlocked.Read(ref _evaluationFailures),
            TimeSpan.FromSeconds((double)Interlocked.Read(ref _evaluationElapsedTicks) / Stopwatch.Frequency));

    internal void RecordEvaluationRequest() => Interlocked.Increment(ref _evaluationRequests);

    internal void RecordEvaluation(long elapsedTicks)
    {
        Interlocked.Increment(ref _evaluations);
        Interlocked.Add(ref _evaluationElapsedTicks, elapsedTicks);
    }

    internal void RecordEvaluationCacheHit() => Interlocked.Increment(ref _evaluationCacheHits);

    internal void RecordEvaluationCacheInvalidation() => Interlocked.Increment(ref _evaluationCacheInvalidations);

    internal void RecordEvaluationFailure() => Interlocked.Increment(ref _evaluationFailures);
}

public readonly record struct ProjectSystemPerformanceSnapshot(
    long EvaluationRequests,
    long Evaluations,
    long EvaluationCacheHits,
    long EvaluationCacheInvalidations,
    long EvaluationFailures,
    TimeSpan EvaluationElapsed);
