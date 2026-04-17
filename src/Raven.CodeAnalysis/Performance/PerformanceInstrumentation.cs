using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading;

using Raven.CodeAnalysis.Syntax;

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
        BinderReentry = new BinderReentryInstrumentation(isEnabled);
        Macros = new MacroInstrumentation(isEnabled);
        Setup = new CompilerSetupInstrumentation(isEnabled);
    }

    public LambdaReplayInstrumentation LambdaReplay { get; }

    public BinderReentryInstrumentation BinderReentry { get; }

    public MacroInstrumentation Macros { get; }

    public CompilerSetupInstrumentation Setup { get; }
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

    [Conditional("RAVEN_INSTRUMENTATION")]
    internal void RecordReplayAttempt()
    {
        if (!_isEnabled)
            return;

        Interlocked.Increment(ref _replayAttempts);
    }

    [Conditional("RAVEN_INSTRUMENTATION")]
    internal void RecordReplaySuccess()
    {
        if (!_isEnabled)
            return;

        Interlocked.Increment(ref _replaySuccesses);
    }

    [Conditional("RAVEN_INSTRUMENTATION")]
    internal void RecordReplayFailure()
    {
        if (!_isEnabled)
            return;

        Interlocked.Increment(ref _replayFailures);
    }

    [Conditional("RAVEN_INSTRUMENTATION")]
    internal void RecordCacheHit()
    {
        if (!_isEnabled)
            return;

        Interlocked.Increment(ref _cacheHits);
    }

    [Conditional("RAVEN_INSTRUMENTATION")]
    internal void RecordCacheMiss()
    {
        if (!_isEnabled)
            return;

        Interlocked.Increment(ref _cacheMisses);
    }

    [Conditional("RAVEN_INSTRUMENTATION")]
    internal void RecordBindingInvocation()
    {
        if (!_isEnabled)
            return;

        Interlocked.Increment(ref _bindingInvocations);
    }

    [Conditional("RAVEN_INSTRUMENTATION")]
    internal void RecordBindingSuccess()
    {
        if (!_isEnabled)
            return;

        Interlocked.Increment(ref _bindingSuccesses);
    }

    [Conditional("RAVEN_INSTRUMENTATION")]
    internal void RecordBindingFailure()
    {
        if (!_isEnabled)
            return;

        Interlocked.Increment(ref _bindingFailures);
    }
}

public sealed class BinderReentryInstrumentation
{
    private readonly bool _isEnabled;
    private long _totalInvocations;
    private long _totalRepeatedNodeInvocations;
    private long _totalCacheHits;
    private long _totalBindExecutions;
    private readonly ConcurrentDictionary<SyntaxNode, int> _nodeInvocationCounts = new();
    private readonly ConcurrentDictionary<SyntaxNode, int> _nodeBindExecutionCounts = new();
    private readonly ConcurrentDictionary<SyntaxKind, long> _invocationsByKind = new();
    private readonly ConcurrentDictionary<SyntaxKind, long> _cacheHitsByKind = new();
    private readonly ConcurrentDictionary<SyntaxKind, long> _bindExecutionsByKind = new();

    internal BinderReentryInstrumentation(bool isEnabled)
    {
        _isEnabled = isEnabled;
    }

    public long TotalInvocations => Volatile.Read(ref _totalInvocations);

    public long TotalRepeatedNodeInvocations => Volatile.Read(ref _totalRepeatedNodeInvocations);

    public long TotalCacheHits => Volatile.Read(ref _totalCacheHits);

    public long TotalBindExecutions => Volatile.Read(ref _totalBindExecutions);

    public int GetBindExecutionCount(SyntaxNode node)
        => _nodeBindExecutionCounts.TryGetValue(node, out var count) ? count : 0;

    [Conditional("RAVEN_INSTRUMENTATION")]
    public void Reset()
    {
        if (!_isEnabled)
            return;

        Interlocked.Exchange(ref _totalInvocations, 0);
        Interlocked.Exchange(ref _totalRepeatedNodeInvocations, 0);
        Interlocked.Exchange(ref _totalCacheHits, 0);
        Interlocked.Exchange(ref _totalBindExecutions, 0);
        _nodeInvocationCounts.Clear();
        _nodeBindExecutionCounts.Clear();
        _invocationsByKind.Clear();
        _cacheHitsByKind.Clear();
        _bindExecutionsByKind.Clear();
    }

    [Conditional("RAVEN_INSTRUMENTATION")]
    internal void RecordInvocation(SyntaxNode node)
    {
        if (!_isEnabled)
            return;

        Interlocked.Increment(ref _totalInvocations);
        _invocationsByKind.AddOrUpdate(node.Kind, 1, static (_, current) => current + 1);

        var count = _nodeInvocationCounts.AddOrUpdate(node, 1, static (_, current) => current + 1);
        if (count > 1)
            Interlocked.Increment(ref _totalRepeatedNodeInvocations);
    }

    [Conditional("RAVEN_INSTRUMENTATION")]
    internal void RecordCacheHit(SyntaxNode node)
    {
        if (!_isEnabled)
            return;

        Interlocked.Increment(ref _totalCacheHits);
        _cacheHitsByKind.AddOrUpdate(node.Kind, 1, static (_, current) => current + 1);
    }

    [Conditional("RAVEN_INSTRUMENTATION")]
    internal void RecordBindExecution(SyntaxNode node)
    {
        if (!_isEnabled)
            return;

        Interlocked.Increment(ref _totalBindExecutions);
        _nodeBindExecutionCounts.AddOrUpdate(node, 1, static (_, current) => current + 1);
        _bindExecutionsByKind.AddOrUpdate(node.Kind, 1, static (_, current) => current + 1);
    }

    public string GetSummary(int topNodeCount = 20, int topKindCount = 20)
    {
        var builder = new StringBuilder();
        builder.AppendLine("# Binder re-entry");
        builder.AppendLine($"Total invocations: {TotalInvocations}");
        builder.AppendLine($"Repeated-node invocations: {TotalRepeatedNodeInvocations}");
        builder.AppendLine($"Cache hits: {TotalCacheHits}");
        builder.AppendLine($"Bind executions: {TotalBindExecutions}");
        builder.AppendLine();

        builder.AppendLine("Top syntax kinds:");
        foreach (var entry in _invocationsByKind
                     .OrderByDescending(static pair => pair.Value)
                     .ThenBy(static pair => pair.Key.ToString(), StringComparer.Ordinal)
                     .Take(topKindCount))
        {
            var cacheHits = _cacheHitsByKind.TryGetValue(entry.Key, out var kindCacheHits) ? kindCacheHits : 0;
            var bindExecutions = _bindExecutionsByKind.TryGetValue(entry.Key, out var kindBindExecutions) ? kindBindExecutions : 0;
            builder.AppendLine($"  - {entry.Key}: invocations={entry.Value}, cacheHits={cacheHits}, bindExecutions={bindExecutions}");
        }

        builder.AppendLine();
        builder.AppendLine("Top repeated syntax nodes:");
        foreach (var entry in _nodeInvocationCounts
                     .Where(static pair => pair.Value > 1)
                     .OrderByDescending(static pair => pair.Value)
                     .ThenBy(static pair => pair.Key.Kind.ToString(), StringComparer.Ordinal)
                     .Take(topNodeCount))
        {
            var node = entry.Key;
            var span = node.GetLocation().GetLineSpan();
            builder.AppendLine(
                $"  - {node.Kind}: count={entry.Value} @ {span.Path}({span.StartLinePosition.Line + 1},{span.StartLinePosition.Character + 1})");
        }

        builder.AppendLine();
        builder.AppendLine("Top bind-executed syntax kinds:");
        foreach (var entry in _bindExecutionsByKind
                     .OrderByDescending(static pair => pair.Value)
                     .ThenBy(static pair => pair.Key.ToString(), StringComparer.Ordinal)
                     .Take(topKindCount))
        {
            builder.AppendLine($"  - {entry.Key}: bindExecutions={entry.Value}");
        }

        builder.AppendLine();
        builder.AppendLine("Top repeatedly bind-executed syntax nodes:");
        foreach (var entry in _nodeBindExecutionCounts
                     .Where(static pair => pair.Value > 1)
                     .OrderByDescending(static pair => pair.Value)
                     .ThenBy(static pair => pair.Key.Kind.ToString(), StringComparer.Ordinal)
                     .Take(topNodeCount))
        {
            var node = entry.Key;
            var span = node.GetLocation().GetLineSpan();
            builder.AppendLine(
                $"  - {node.Kind}: bindExecutions={entry.Value} @ {span.Path}({span.StartLinePosition.Line + 1},{span.StartLinePosition.Character + 1})");
        }

        return builder.ToString();
    }
}

public sealed class MacroInstrumentation
{
    private readonly bool _isEnabled;
    private long _attachedExpansionInvocations;
    private long _freestandingExpansionInvocations;
    private long _shadowOutputCacheHits;
    private long _shadowOutputCacheMisses;
    private long _consumerRefreshRuns;
    private long _consumerRefreshProjectUpdates;

    internal MacroInstrumentation(bool isEnabled)
    {
        _isEnabled = isEnabled;
    }

    public long AttachedExpansionInvocations => Volatile.Read(ref _attachedExpansionInvocations);

    public long FreestandingExpansionInvocations => Volatile.Read(ref _freestandingExpansionInvocations);

    public long ShadowOutputCacheHits => Volatile.Read(ref _shadowOutputCacheHits);

    public long ShadowOutputCacheMisses => Volatile.Read(ref _shadowOutputCacheMisses);

    public long ConsumerRefreshRuns => Volatile.Read(ref _consumerRefreshRuns);

    public long ConsumerRefreshProjectUpdates => Volatile.Read(ref _consumerRefreshProjectUpdates);

    [Conditional("RAVEN_INSTRUMENTATION")]
    public void Reset()
    {
        if (!_isEnabled)
            return;

        Interlocked.Exchange(ref _attachedExpansionInvocations, 0);
        Interlocked.Exchange(ref _freestandingExpansionInvocations, 0);
        Interlocked.Exchange(ref _shadowOutputCacheHits, 0);
        Interlocked.Exchange(ref _shadowOutputCacheMisses, 0);
        Interlocked.Exchange(ref _consumerRefreshRuns, 0);
        Interlocked.Exchange(ref _consumerRefreshProjectUpdates, 0);
    }

    [Conditional("RAVEN_INSTRUMENTATION")]
    internal void RecordAttachedExpansionInvocation()
    {
        if (!_isEnabled)
            return;

        Interlocked.Increment(ref _attachedExpansionInvocations);
    }

    [Conditional("RAVEN_INSTRUMENTATION")]
    internal void RecordFreestandingExpansionInvocation()
    {
        if (!_isEnabled)
            return;

        Interlocked.Increment(ref _freestandingExpansionInvocations);
    }

    [Conditional("RAVEN_INSTRUMENTATION")]
    internal void RecordShadowOutputCacheHit()
    {
        if (!_isEnabled)
            return;

        Interlocked.Increment(ref _shadowOutputCacheHits);
    }

    [Conditional("RAVEN_INSTRUMENTATION")]
    internal void RecordShadowOutputCacheMiss()
    {
        if (!_isEnabled)
            return;

        Interlocked.Increment(ref _shadowOutputCacheMisses);
    }

    [Conditional("RAVEN_INSTRUMENTATION")]
    internal void RecordConsumerRefreshRun(int updatedProjects)
    {
        if (!_isEnabled)
            return;

        Interlocked.Increment(ref _consumerRefreshRuns);
        if (updatedProjects > 0)
            Interlocked.Add(ref _consumerRefreshProjectUpdates, updatedProjects);
    }

    public string GetSummary()
    {
        var builder = new StringBuilder();
        builder.AppendLine("# Macro instrumentation");
        builder.AppendLine($"Attached expansion invocations: {AttachedExpansionInvocations}");
        builder.AppendLine($"Freestanding expansion invocations: {FreestandingExpansionInvocations}");
        builder.AppendLine($"Shadow output cache hits: {ShadowOutputCacheHits}");
        builder.AppendLine($"Shadow output cache misses: {ShadowOutputCacheMisses}");
        builder.AppendLine($"Consumer refresh runs: {ConsumerRefreshRuns}");
        builder.AppendLine($"Consumer refresh project updates: {ConsumerRefreshProjectUpdates}");
        return builder.ToString();
    }
}

public sealed class CompilerSetupInstrumentation
{
    public readonly record struct Snapshot(
        long EnsureSemanticModelsCreatedCalls,
        long SemanticModelsCreated,
        long EnsureSourceDeclarationsCompleteCalls,
        long EnsureDeclarationsCalls,
        long DeclarationPasses,
        long EnsureRootBinderCreatedCalls,
        long RootBinderCreations);

    private readonly bool _isEnabled;
    private long _ensureSemanticModelsCreatedCalls;
    private long _semanticModelsCreated;
    private long _ensureSourceDeclarationsCompleteCalls;
    private long _ensureDeclarationsCalls;
    private long _declarationPasses;
    private long _ensureRootBinderCreatedCalls;
    private long _rootBinderCreations;

    internal CompilerSetupInstrumentation(bool isEnabled)
    {
        _isEnabled = isEnabled;
    }

    public long EnsureSemanticModelsCreatedCalls => Volatile.Read(ref _ensureSemanticModelsCreatedCalls);
    public long SemanticModelsCreated => Volatile.Read(ref _semanticModelsCreated);
    public long EnsureSourceDeclarationsCompleteCalls => Volatile.Read(ref _ensureSourceDeclarationsCompleteCalls);
    public long EnsureDeclarationsCalls => Volatile.Read(ref _ensureDeclarationsCalls);
    public long DeclarationPasses => Volatile.Read(ref _declarationPasses);
    public long EnsureRootBinderCreatedCalls => Volatile.Read(ref _ensureRootBinderCreatedCalls);
    public long RootBinderCreations => Volatile.Read(ref _rootBinderCreations);

    public Snapshot CaptureSnapshot()
        => new(
            EnsureSemanticModelsCreatedCalls,
            SemanticModelsCreated,
            EnsureSourceDeclarationsCompleteCalls,
            EnsureDeclarationsCalls,
            DeclarationPasses,
            EnsureRootBinderCreatedCalls,
            RootBinderCreations);

    public static Snapshot Subtract(Snapshot end, Snapshot start)
        => new(
            end.EnsureSemanticModelsCreatedCalls - start.EnsureSemanticModelsCreatedCalls,
            end.SemanticModelsCreated - start.SemanticModelsCreated,
            end.EnsureSourceDeclarationsCompleteCalls - start.EnsureSourceDeclarationsCompleteCalls,
            end.EnsureDeclarationsCalls - start.EnsureDeclarationsCalls,
            end.DeclarationPasses - start.DeclarationPasses,
            end.EnsureRootBinderCreatedCalls - start.EnsureRootBinderCreatedCalls,
            end.RootBinderCreations - start.RootBinderCreations);

    public static string FormatDelta(Snapshot delta)
        => $"ensureModels={delta.EnsureSemanticModelsCreatedCalls}, " +
           $"modelsCreated={delta.SemanticModelsCreated}, " +
           $"ensureSourceDecls={delta.EnsureSourceDeclarationsCompleteCalls}, " +
           $"ensureDecls={delta.EnsureDeclarationsCalls}, " +
           $"declarationPasses={delta.DeclarationPasses}, " +
           $"ensureRootBinders={delta.EnsureRootBinderCreatedCalls}, " +
           $"rootBindersCreated={delta.RootBinderCreations}";

    [Conditional("RAVEN_INSTRUMENTATION")]
    public void Reset()
    {
        if (!_isEnabled)
            return;

        Interlocked.Exchange(ref _ensureSemanticModelsCreatedCalls, 0);
        Interlocked.Exchange(ref _semanticModelsCreated, 0);
        Interlocked.Exchange(ref _ensureSourceDeclarationsCompleteCalls, 0);
        Interlocked.Exchange(ref _ensureDeclarationsCalls, 0);
        Interlocked.Exchange(ref _declarationPasses, 0);
        Interlocked.Exchange(ref _ensureRootBinderCreatedCalls, 0);
        Interlocked.Exchange(ref _rootBinderCreations, 0);
    }

    [Conditional("RAVEN_INSTRUMENTATION")]
    internal void RecordEnsureSemanticModelsCreatedCall()
    {
        if (!_isEnabled)
            return;

        Interlocked.Increment(ref _ensureSemanticModelsCreatedCalls);
    }

    [Conditional("RAVEN_INSTRUMENTATION")]
    internal void RecordSemanticModelCreated()
    {
        if (!_isEnabled)
            return;

        Interlocked.Increment(ref _semanticModelsCreated);
    }

    [Conditional("RAVEN_INSTRUMENTATION")]
    internal void RecordEnsureSourceDeclarationsCompleteCall()
    {
        if (!_isEnabled)
            return;

        Interlocked.Increment(ref _ensureSourceDeclarationsCompleteCalls);
    }

    [Conditional("RAVEN_INSTRUMENTATION")]
    internal void RecordEnsureDeclarationsCall()
    {
        if (!_isEnabled)
            return;

        Interlocked.Increment(ref _ensureDeclarationsCalls);
    }

    [Conditional("RAVEN_INSTRUMENTATION")]
    internal void RecordDeclarationPass()
    {
        if (!_isEnabled)
            return;

        Interlocked.Increment(ref _declarationPasses);
    }

    [Conditional("RAVEN_INSTRUMENTATION")]
    internal void RecordEnsureRootBinderCreatedCall()
    {
        if (!_isEnabled)
            return;

        Interlocked.Increment(ref _ensureRootBinderCreatedCalls);
    }

    [Conditional("RAVEN_INSTRUMENTATION")]
    internal void RecordRootBinderCreated()
    {
        if (!_isEnabled)
            return;

        Interlocked.Increment(ref _rootBinderCreations);
    }

    public string GetSummary()
    {
        var builder = new StringBuilder();
        builder.AppendLine("# Compiler setup instrumentation");
        builder.AppendLine($"EnsureSemanticModelsCreated calls: {EnsureSemanticModelsCreatedCalls}");
        builder.AppendLine($"Semantic models created: {SemanticModelsCreated}");
        builder.AppendLine($"EnsureSourceDeclarationsComplete calls: {EnsureSourceDeclarationsCompleteCalls}");
        builder.AppendLine($"EnsureDeclarations calls: {EnsureDeclarationsCalls}");
        builder.AppendLine($"Declaration passes: {DeclarationPasses}");
        builder.AppendLine($"EnsureRootBinderCreated calls: {EnsureRootBinderCreatedCalls}");
        builder.AppendLine($"Root binders created: {RootBinderCreations}");
        return builder.ToString();
    }
}
