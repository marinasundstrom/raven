using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

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
        SemanticQuery = new SemanticQueryInstrumentation(isEnabled);
        DiagnosticBinding = new DiagnosticBindingInstrumentation(isEnabled);
        Macros = new MacroInstrumentation(isEnabled);
        Setup = new CompilerSetupInstrumentation(isEnabled);
    }

    public LambdaReplayInstrumentation LambdaReplay { get; }

    public BinderReentryInstrumentation BinderReentry { get; }

    public SemanticQueryInstrumentation SemanticQuery { get; }

    public DiagnosticBindingInstrumentation DiagnosticBinding { get; }

    public MacroInstrumentation Macros { get; }

    public CompilerSetupInstrumentation Setup { get; }
}

public sealed class DiagnosticBindingInstrumentation
{
    public readonly record struct Snapshot(
        long Calls,
        long DocumentCalls,
        long CompleteCalls,
        long TransferredDocumentHits,
        long IncrementalPasses,
        long FullPasses,
        long DeclarationTicks,
        long BinderSelectionTicks,
        long ClearTicks,
        long TraverseTicks,
        long DocumentationTicks,
        long CollectTicks,
        long MaterializeTicks,
        long StoreDescriptorTicks);

    private readonly bool _isEnabled;
    private long _calls;
    private long _documentCalls;
    private long _completeCalls;
    private long _transferredDocumentHits;
    private long _incrementalPasses;
    private long _fullPasses;
    private long _declarationTicks;
    private long _binderSelectionTicks;
    private long _clearTicks;
    private long _traverseTicks;
    private long _documentationTicks;
    private long _collectTicks;
    private long _materializeTicks;
    private long _storeDescriptorTicks;

    internal DiagnosticBindingInstrumentation(bool isEnabled)
    {
        _isEnabled = isEnabled;
    }

    public Snapshot CaptureSnapshot()
        => new(
            Volatile.Read(ref _calls),
            Volatile.Read(ref _documentCalls),
            Volatile.Read(ref _completeCalls),
            Volatile.Read(ref _transferredDocumentHits),
            Volatile.Read(ref _incrementalPasses),
            Volatile.Read(ref _fullPasses),
            Volatile.Read(ref _declarationTicks),
            Volatile.Read(ref _binderSelectionTicks),
            Volatile.Read(ref _clearTicks),
            Volatile.Read(ref _traverseTicks),
            Volatile.Read(ref _documentationTicks),
            Volatile.Read(ref _collectTicks),
            Volatile.Read(ref _materializeTicks),
            Volatile.Read(ref _storeDescriptorTicks));

    public static Snapshot Subtract(Snapshot end, Snapshot start)
        => new(
            end.Calls - start.Calls,
            end.DocumentCalls - start.DocumentCalls,
            end.CompleteCalls - start.CompleteCalls,
            end.TransferredDocumentHits - start.TransferredDocumentHits,
            end.IncrementalPasses - start.IncrementalPasses,
            end.FullPasses - start.FullPasses,
            end.DeclarationTicks - start.DeclarationTicks,
            end.BinderSelectionTicks - start.BinderSelectionTicks,
            end.ClearTicks - start.ClearTicks,
            end.TraverseTicks - start.TraverseTicks,
            end.DocumentationTicks - start.DocumentationTicks,
            end.CollectTicks - start.CollectTicks,
            end.MaterializeTicks - start.MaterializeTicks,
            end.StoreDescriptorTicks - start.StoreDescriptorTicks);

    public static string FormatDelta(Snapshot delta)
        => $"calls={delta.Calls}, " +
           $"documentCalls={delta.DocumentCalls}, " +
           $"completeCalls={delta.CompleteCalls}, " +
           $"transferredDocumentHits={delta.TransferredDocumentHits}, " +
           $"incrementalPasses={delta.IncrementalPasses}, " +
           $"fullPasses={delta.FullPasses}, " +
           $"declarationMs={TicksToMilliseconds(delta.DeclarationTicks):F1}, " +
           $"binderSelectionMs={TicksToMilliseconds(delta.BinderSelectionTicks):F1}, " +
           $"clearMs={TicksToMilliseconds(delta.ClearTicks):F1}, " +
           $"traverseMs={TicksToMilliseconds(delta.TraverseTicks):F1}, " +
           $"documentationMs={TicksToMilliseconds(delta.DocumentationTicks):F1}, " +
           $"collectMs={TicksToMilliseconds(delta.CollectTicks):F1}, " +
           $"materializeMs={TicksToMilliseconds(delta.MaterializeTicks):F1}, " +
           $"storeDescriptorsMs={TicksToMilliseconds(delta.StoreDescriptorTicks):F1}";

    [Conditional("RAVEN_INSTRUMENTATION")]
    internal void RecordCall(bool requireCompleteDeclarations)
    {
        if (!_isEnabled)
            return;

        Interlocked.Increment(ref _calls);
        if (requireCompleteDeclarations)
            Interlocked.Increment(ref _completeCalls);
        else
            Interlocked.Increment(ref _documentCalls);
    }

    [Conditional("RAVEN_INSTRUMENTATION")]
    internal void RecordTransferredDocumentHit()
    {
        if (_isEnabled)
            Interlocked.Increment(ref _transferredDocumentHits);
    }

    [Conditional("RAVEN_INSTRUMENTATION")]
    internal void RecordIncrementalPass()
    {
        if (_isEnabled)
            Interlocked.Increment(ref _incrementalPasses);
    }

    [Conditional("RAVEN_INSTRUMENTATION")]
    internal void RecordFullPass()
    {
        if (_isEnabled)
            Interlocked.Increment(ref _fullPasses);
    }

    [Conditional("RAVEN_INSTRUMENTATION")]
    internal void RecordDeclarationTicks(long ticks)
    {
        AddTicks(ref _declarationTicks, ticks);
    }

    [Conditional("RAVEN_INSTRUMENTATION")]
    internal void RecordBinderSelectionTicks(long ticks)
    {
        AddTicks(ref _binderSelectionTicks, ticks);
    }

    [Conditional("RAVEN_INSTRUMENTATION")]
    internal void RecordClearTicks(long ticks)
    {
        AddTicks(ref _clearTicks, ticks);
    }

    [Conditional("RAVEN_INSTRUMENTATION")]
    internal void RecordTraverseTicks(long ticks)
    {
        AddTicks(ref _traverseTicks, ticks);
    }

    [Conditional("RAVEN_INSTRUMENTATION")]
    internal void RecordDocumentationTicks(long ticks)
    {
        AddTicks(ref _documentationTicks, ticks);
    }

    [Conditional("RAVEN_INSTRUMENTATION")]
    internal void RecordCollectTicks(long ticks)
    {
        AddTicks(ref _collectTicks, ticks);
    }

    [Conditional("RAVEN_INSTRUMENTATION")]
    internal void RecordMaterializeTicks(long ticks)
    {
        AddTicks(ref _materializeTicks, ticks);
    }

    [Conditional("RAVEN_INSTRUMENTATION")]
    internal void RecordStoreDescriptorTicks(long ticks)
    {
        AddTicks(ref _storeDescriptorTicks, ticks);
    }

    private void AddTicks(ref long target, long ticks)
    {
        if (!_isEnabled)
            return;

        Interlocked.Add(ref target, ticks);
    }

    private static double TicksToMilliseconds(long ticks)
        => ticks * 1000d / Stopwatch.Frequency;
}

public sealed class SemanticQueryInstrumentation
{
    public readonly record struct Snapshot(
        long SymbolInfoQueries,
        long SymbolInfoCacheHits,
        long SymbolInfoBoundCacheHits,
        long SymbolInfoBinderFallbacks,
        long SymbolInfoOperationFallbacks,
        long TypeInfoQueries,
        long TypeInfoSymbolHits,
        long TypeInfoBoundFallbacks,
        long TypeInfoDiagnosticFallbacks,
        long BoundNodeQueries,
        long BoundNodeCacheHits,
        long BoundNodeContextualCacheHits,
        long BoundNodeBindFallbacks,
        long BoundNodeLoweredCacheHits,
        long BoundNodeLoweredFallbacks);

    private readonly bool _isEnabled;
    private long _symbolInfoQueries;
    private long _symbolInfoCacheHits;
    private long _symbolInfoBoundCacheHits;
    private long _symbolInfoBinderFallbacks;
    private long _symbolInfoOperationFallbacks;
    private long _typeInfoQueries;
    private long _typeInfoSymbolHits;
    private long _typeInfoBoundFallbacks;
    private long _typeInfoDiagnosticFallbacks;
    private long _boundNodeQueries;
    private long _boundNodeCacheHits;
    private long _boundNodeContextualCacheHits;
    private long _boundNodeBindFallbacks;
    private long _boundNodeLoweredCacheHits;
    private long _boundNodeLoweredFallbacks;

    internal SemanticQueryInstrumentation(bool isEnabled)
    {
        _isEnabled = isEnabled;
    }

    public Snapshot CaptureSnapshot()
        => new(
            Volatile.Read(ref _symbolInfoQueries),
            Volatile.Read(ref _symbolInfoCacheHits),
            Volatile.Read(ref _symbolInfoBoundCacheHits),
            Volatile.Read(ref _symbolInfoBinderFallbacks),
            Volatile.Read(ref _symbolInfoOperationFallbacks),
            Volatile.Read(ref _typeInfoQueries),
            Volatile.Read(ref _typeInfoSymbolHits),
            Volatile.Read(ref _typeInfoBoundFallbacks),
            Volatile.Read(ref _typeInfoDiagnosticFallbacks),
            Volatile.Read(ref _boundNodeQueries),
            Volatile.Read(ref _boundNodeCacheHits),
            Volatile.Read(ref _boundNodeContextualCacheHits),
            Volatile.Read(ref _boundNodeBindFallbacks),
            Volatile.Read(ref _boundNodeLoweredCacheHits),
            Volatile.Read(ref _boundNodeLoweredFallbacks));

    public static Snapshot Subtract(Snapshot end, Snapshot start)
        => new(
            end.SymbolInfoQueries - start.SymbolInfoQueries,
            end.SymbolInfoCacheHits - start.SymbolInfoCacheHits,
            end.SymbolInfoBoundCacheHits - start.SymbolInfoBoundCacheHits,
            end.SymbolInfoBinderFallbacks - start.SymbolInfoBinderFallbacks,
            end.SymbolInfoOperationFallbacks - start.SymbolInfoOperationFallbacks,
            end.TypeInfoQueries - start.TypeInfoQueries,
            end.TypeInfoSymbolHits - start.TypeInfoSymbolHits,
            end.TypeInfoBoundFallbacks - start.TypeInfoBoundFallbacks,
            end.TypeInfoDiagnosticFallbacks - start.TypeInfoDiagnosticFallbacks,
            end.BoundNodeQueries - start.BoundNodeQueries,
            end.BoundNodeCacheHits - start.BoundNodeCacheHits,
            end.BoundNodeContextualCacheHits - start.BoundNodeContextualCacheHits,
            end.BoundNodeBindFallbacks - start.BoundNodeBindFallbacks,
            end.BoundNodeLoweredCacheHits - start.BoundNodeLoweredCacheHits,
            end.BoundNodeLoweredFallbacks - start.BoundNodeLoweredFallbacks);

    public static string FormatDelta(Snapshot delta)
        => $"symbolInfo={delta.SymbolInfoQueries}, " +
           $"symbolCacheHits={delta.SymbolInfoCacheHits}, " +
           $"symbolBoundHits={delta.SymbolInfoBoundCacheHits}, " +
           $"symbolBinderFallbacks={delta.SymbolInfoBinderFallbacks}, " +
           $"symbolOperationFallbacks={delta.SymbolInfoOperationFallbacks}, " +
           $"typeInfo={delta.TypeInfoQueries}, " +
           $"typeSymbolHits={delta.TypeInfoSymbolHits}, " +
           $"typeBoundFallbacks={delta.TypeInfoBoundFallbacks}, " +
           $"typeDiagnosticFallbacks={delta.TypeInfoDiagnosticFallbacks}, " +
           $"boundNode={delta.BoundNodeQueries}, " +
           $"boundCacheHits={delta.BoundNodeCacheHits}, " +
           $"boundContextHits={delta.BoundNodeContextualCacheHits}, " +
           $"boundBindFallbacks={delta.BoundNodeBindFallbacks}, " +
           $"loweredCacheHits={delta.BoundNodeLoweredCacheHits}, " +
           $"loweredFallbacks={delta.BoundNodeLoweredFallbacks}";

    internal void RecordSymbolInfoQuery()
    {
        if (_isEnabled)
            Interlocked.Increment(ref _symbolInfoQueries);
    }

    internal void RecordSymbolInfoCacheHit()
    {
        if (_isEnabled)
            Interlocked.Increment(ref _symbolInfoCacheHits);
    }

    internal void RecordSymbolInfoBoundCacheHit()
    {
        if (_isEnabled)
            Interlocked.Increment(ref _symbolInfoBoundCacheHits);
    }

    internal void RecordSymbolInfoBinderFallback()
    {
        if (_isEnabled)
            Interlocked.Increment(ref _symbolInfoBinderFallbacks);
    }

    internal void RecordSymbolInfoOperationFallback()
    {
        if (_isEnabled)
            Interlocked.Increment(ref _symbolInfoOperationFallbacks);
    }

    internal void RecordTypeInfoQuery()
    {
        if (_isEnabled)
            Interlocked.Increment(ref _typeInfoQueries);
    }

    internal void RecordTypeInfoSymbolHit()
    {
        if (_isEnabled)
            Interlocked.Increment(ref _typeInfoSymbolHits);
    }

    internal void RecordTypeInfoBoundFallback()
    {
        if (_isEnabled)
            Interlocked.Increment(ref _typeInfoBoundFallbacks);
    }

    internal void RecordTypeInfoDiagnosticFallback()
    {
        if (_isEnabled)
            Interlocked.Increment(ref _typeInfoDiagnosticFallbacks);
    }

    internal void RecordBoundNodeQuery()
    {
        if (_isEnabled)
            Interlocked.Increment(ref _boundNodeQueries);
    }

    internal void RecordBoundNodeCacheHit()
    {
        if (_isEnabled)
            Interlocked.Increment(ref _boundNodeCacheHits);
    }

    internal void RecordBoundNodeContextualCacheHit()
    {
        if (_isEnabled)
            Interlocked.Increment(ref _boundNodeContextualCacheHits);
    }

    internal void RecordBoundNodeBindFallback()
    {
        if (_isEnabled)
            Interlocked.Increment(ref _boundNodeBindFallbacks);
    }

    internal void RecordBoundNodeLoweredCacheHit()
    {
        if (_isEnabled)
            Interlocked.Increment(ref _boundNodeLoweredCacheHits);
    }

    internal void RecordBoundNodeLoweredFallback()
    {
        if (_isEnabled)
            Interlocked.Increment(ref _boundNodeLoweredFallbacks);
    }
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
    public readonly record struct Snapshot(
        long TotalInvocations,
        long TotalRepeatedNodeInvocations,
        long TotalCacheHits,
        long TotalBindExecutions);

    private readonly bool _isEnabled;
    private long _totalInvocations;
    private long _totalRepeatedNodeInvocations;
    private long _totalCacheHits;
    private long _totalBindExecutions;
    private readonly ConcurrentDictionary<SyntaxNodeInstrumentationKey, int> _nodeInvocationCounts = new();
    private readonly ConcurrentDictionary<SyntaxNodeInstrumentationKey, int> _nodeBindExecutionCounts = new();
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

    public Snapshot CaptureSnapshot()
        => new(
            TotalInvocations,
            TotalRepeatedNodeInvocations,
            TotalCacheHits,
            TotalBindExecutions);

    public static Snapshot Subtract(Snapshot end, Snapshot start)
        => new(
            end.TotalInvocations - start.TotalInvocations,
            end.TotalRepeatedNodeInvocations - start.TotalRepeatedNodeInvocations,
            end.TotalCacheHits - start.TotalCacheHits,
            end.TotalBindExecutions - start.TotalBindExecutions);

    public static string FormatDelta(Snapshot delta)
        => $"invocations={delta.TotalInvocations}, " +
           $"repeated={delta.TotalRepeatedNodeInvocations}, " +
           $"cacheHits={delta.TotalCacheHits}, " +
           $"bindExecutions={delta.TotalBindExecutions}";

    public int GetBindExecutionCount(SyntaxNode node)
        => _nodeBindExecutionCounts.TryGetValue(SyntaxNodeInstrumentationKey.Create(node), out var count) ? count : 0;

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

        var key = SyntaxNodeInstrumentationKey.Create(node);
        var count = _nodeInvocationCounts.AddOrUpdate(key, 1, static (_, current) => current + 1);
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
        _nodeBindExecutionCounts.AddOrUpdate(SyntaxNodeInstrumentationKey.Create(node), 1, static (_, current) => current + 1);
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
            var key = entry.Key;
            builder.AppendLine(
                $"  - {key.Kind}: count={entry.Value} @ {key.FilePath}[{key.Span.Start}..{key.Span.End})");
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
            var key = entry.Key;
            builder.AppendLine(
                $"  - {key.Kind}: bindExecutions={entry.Value} @ {key.FilePath}[{key.Span.Start}..{key.Span.End})");
        }

        return builder.ToString();
    }

    private readonly record struct SyntaxNodeInstrumentationKey(
        SyntaxKind Kind,
        string FilePath,
        TextSpan Span,
        TextSpan FullSpan)
    {
        public static SyntaxNodeInstrumentationKey Create(SyntaxNode node)
            => new(
                node.Kind,
                node.SyntaxTree?.FilePath ?? string.Empty,
                node.Span,
                node.FullSpan);
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
        long EnsureSourceDeclarationsDeclaredCalls,
        long EnsureSourceDeclarationsCompleteCalls,
        long EnsureDeclarationsCalls,
        long DeclarationPasses,
        long EnsureRootBinderCreatedCalls,
        long RootBinderCreations);

    private readonly bool _isEnabled;
    private long _ensureSemanticModelsCreatedCalls;
    private long _semanticModelsCreated;
    private long _ensureSourceDeclarationsDeclaredCalls;
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
    public long EnsureSourceDeclarationsDeclaredCalls => Volatile.Read(ref _ensureSourceDeclarationsDeclaredCalls);
    public long EnsureSourceDeclarationsCompleteCalls => Volatile.Read(ref _ensureSourceDeclarationsCompleteCalls);
    public long EnsureDeclarationsCalls => Volatile.Read(ref _ensureDeclarationsCalls);
    public long DeclarationPasses => Volatile.Read(ref _declarationPasses);
    public long EnsureRootBinderCreatedCalls => Volatile.Read(ref _ensureRootBinderCreatedCalls);
    public long RootBinderCreations => Volatile.Read(ref _rootBinderCreations);

    public Snapshot CaptureSnapshot()
        => new(
            EnsureSemanticModelsCreatedCalls,
            SemanticModelsCreated,
            EnsureSourceDeclarationsDeclaredCalls,
            EnsureSourceDeclarationsCompleteCalls,
            EnsureDeclarationsCalls,
            DeclarationPasses,
            EnsureRootBinderCreatedCalls,
            RootBinderCreations);

    public static Snapshot Subtract(Snapshot end, Snapshot start)
        => new(
            end.EnsureSemanticModelsCreatedCalls - start.EnsureSemanticModelsCreatedCalls,
            end.SemanticModelsCreated - start.SemanticModelsCreated,
            end.EnsureSourceDeclarationsDeclaredCalls - start.EnsureSourceDeclarationsDeclaredCalls,
            end.EnsureSourceDeclarationsCompleteCalls - start.EnsureSourceDeclarationsCompleteCalls,
            end.EnsureDeclarationsCalls - start.EnsureDeclarationsCalls,
            end.DeclarationPasses - start.DeclarationPasses,
            end.EnsureRootBinderCreatedCalls - start.EnsureRootBinderCreatedCalls,
            end.RootBinderCreations - start.RootBinderCreations);

    public static string FormatDelta(Snapshot delta)
        => $"ensureModels={delta.EnsureSemanticModelsCreatedCalls}, " +
           $"modelsCreated={delta.SemanticModelsCreated}, " +
           $"ensureSourceDeclsDeclared={delta.EnsureSourceDeclarationsDeclaredCalls}, " +
           $"ensureSourceDeclsComplete={delta.EnsureSourceDeclarationsCompleteCalls}, " +
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
        Interlocked.Exchange(ref _ensureSourceDeclarationsDeclaredCalls, 0);
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
    internal void RecordEnsureSourceDeclarationsDeclaredCall()
    {
        if (!_isEnabled)
            return;

        Interlocked.Increment(ref _ensureSourceDeclarationsDeclaredCalls);
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
        builder.AppendLine($"EnsureSourceDeclarationsDeclared calls: {EnsureSourceDeclarationsDeclaredCalls}");
        builder.AppendLine($"EnsureSourceDeclarationsComplete calls: {EnsureSourceDeclarationsCompleteCalls}");
        builder.AppendLine($"EnsureDeclarations calls: {EnsureDeclarationsCalls}");
        builder.AppendLine($"Declaration passes: {DeclarationPasses}");
        builder.AppendLine($"EnsureRootBinderCreated calls: {EnsureRootBinderCreatedCalls}");
        builder.AppendLine($"Root binders created: {RootBinderCreations}");
        return builder.ToString();
    }
}
