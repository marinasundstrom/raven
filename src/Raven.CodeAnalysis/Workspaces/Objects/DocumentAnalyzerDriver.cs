using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal sealed class DocumentAnalyzerDriver
{
    private readonly Project _project;
    private readonly SyntaxTree _syntaxTree;
    private readonly Compilation _compilation;
    private readonly CompilationWithAnalyzersOptions? _analyzerOptions;
    private readonly IWorkspaceEventSink? _eventSink;
    private readonly CancellationToken _cancellationToken;
    private readonly bool _semanticAccessAlreadyHeld;
    private readonly List<Diagnostic> _diagnostics = [];
    private readonly HashSet<Diagnostic> _diagnosticSet = [];
    private long _syntaxTraversalTicks;
    private int _syntaxTraversalNodeVisits;
    private int _syntaxTraversalActionInvocations;
    private int _syntaxTraversalKindCount;

    private DocumentAnalyzerDriver(
        Project project,
        SyntaxTree syntaxTree,
        Compilation compilation,
        CompilationWithAnalyzersOptions? analyzerOptions,
        IWorkspaceEventSink? eventSink,
        bool semanticAccessAlreadyHeld,
        CancellationToken cancellationToken)
    {
        _project = project;
        _syntaxTree = syntaxTree;
        _compilation = compilation;
        _analyzerOptions = analyzerOptions;
        _eventSink = eventSink;
        _semanticAccessAlreadyHeld = semanticAccessAlreadyHeld;
        _cancellationToken = cancellationToken;
    }

    public static ImmutableArray<Diagnostic> Run(
        Project project,
        SyntaxTree syntaxTree,
        Compilation compilation,
        CompilationWithAnalyzersOptions? analyzerOptions,
        IWorkspaceEventSink? eventSink,
        CancellationToken cancellationToken,
        bool semanticAccessAlreadyHeld = false)
    {
        var driver = new DocumentAnalyzerDriver(
            project,
            syntaxTree,
            compilation,
            analyzerOptions,
            eventSink,
            semanticAccessAlreadyHeld,
            cancellationToken);

        return driver.RunCore();
    }

    private ImmutableArray<Diagnostic> RunCore()
    {
        var totalTimestamp = Stopwatch.GetTimestamp();

        try
        {
            var executions = CollectAnalyzerExecutions();
            ReportActionPlan(executions);
            RunAnalyzerExecutions(executions);
            MergeDiagnostics(executions);
            ReportSyntaxTraversalStats(executions);
            ReportAnalyzerStats(executions);

            ReportWorkspaceEvent(
                "documentAnalyzer.total",
                Stopwatch.GetElapsedTime(totalTimestamp).TotalMilliseconds,
                $"analyzers={executions.Count}, diagnostics={_diagnostics.Count}, outcome=completed");

            return _diagnostics.OrderBy(static diagnostic => diagnostic, DiagnosticComparer.Instance).ToImmutableArray();
        }
        catch (OperationCanceledException)
        {
            ReportWorkspaceEvent(
                "documentAnalyzer.total",
                Stopwatch.GetElapsedTime(totalTimestamp).TotalMilliseconds,
                $"diagnostics={_diagnostics.Count}, outcome=canceled");
            throw;
        }
    }

    private List<AnalyzerExecution> CollectAnalyzerExecutions()
    {
        var executions = new List<AnalyzerExecution>();

        foreach (var analyzer in GetEnabledAnalyzers())
        {
            _cancellationToken.ThrowIfCancellationRequested();
            executions.Add(CreateAnalyzerExecution(analyzer));
        }

        return executions;
    }

    private AnalyzerExecution CreateAnalyzerExecution(DiagnosticAnalyzer analyzer)
    {
        var stats = new DocumentAnalyzerStats(analyzer.GetType().FullName ?? analyzer.GetType().Name);
        var execution = new AnalyzerExecution(analyzer, stats);
        var isInternalAnalyzer = AnalyzerDiagnosticIdValidator.IsInternalAnalyzer(analyzer);
        var initializationTimestamp = Stopwatch.GetTimestamp();
        if (!analyzer.TryEnsureInitialized())
        {
            stats.InitializationTicks += Stopwatch.GetTimestamp() - initializationTimestamp;
            return execution;
        }

        stats.InitializationTicks += Stopwatch.GetTimestamp() - initializationTimestamp;
        stats.ConcurrentExecutionEnabled = analyzer.ConcurrentExecutionEnabled;

        void ReportDiagnostic(Diagnostic diagnostic)
        {
            AnalyzerDiagnosticIdValidator.Validate(analyzer, diagnostic, isInternalAnalyzer);

            var mapped = _compilation.ApplyCompilationOptions(
                diagnostic,
                _analyzerOptions?.ReportSuppressedDiagnostics ?? false);
            if (mapped is not null)
            {
                if (execution.DiagnosticSet.Add(mapped))
                {
                    execution.Diagnostics.Add(mapped);
                    stats.Diagnostics++;
                }
            }
        }

        CollectCompilationActions(analyzer, execution, ReportDiagnostic, stats);
        CollectSyntaxTreeActions(analyzer, execution, ReportDiagnostic, stats);
        CollectSyntaxNodeActions(analyzer, execution, ReportDiagnostic, stats);

        return execution;
    }

    private void CollectCompilationActions(
        DiagnosticAnalyzer analyzer,
        AnalyzerExecution execution,
        Action<Diagnostic> reportDiagnostic,
        DocumentAnalyzerStats stats)
    {
        foreach (var action in analyzer.CompilationActions)
        {
            _cancellationToken.ThrowIfCancellationRequested();
            execution.CompilationActions.Add(new DocumentCompilationAnalyzerAction(action, reportDiagnostic, stats));
        }
    }

    private void RunCompilationActions(AnalyzerExecution execution)
    {
        foreach (var action in execution.CompilationActions)
        {
            _cancellationToken.ThrowIfCancellationRequested();

            var context = new CompilationAnalysisContext(
                _compilation,
                _syntaxTree,
                action.ReportDiagnostic,
                _cancellationToken);

            try
            {
                var actionTimestamp = Stopwatch.GetTimestamp();
                action.Action(context);
                action.Stats.CompilationActionTicks += Stopwatch.GetTimestamp() - actionTimestamp;
                action.Stats.CompilationActionCount++;
            }
            catch (OperationCanceledException)
            {
                throw;
            }
            catch
            {
                // Analyzer failures should not stop normal compilation diagnostics.
                action.Stats.CompilationActionCount++;
            }
        }
    }

    private void CollectSyntaxTreeActions(
        DiagnosticAnalyzer analyzer,
        AnalyzerExecution execution,
        Action<Diagnostic> reportDiagnostic,
        DocumentAnalyzerStats stats)
    {
        foreach (var action in analyzer.SyntaxTreeActions)
        {
            _cancellationToken.ThrowIfCancellationRequested();
            execution.SyntaxTreeActions.Add(new DocumentSyntaxTreeAnalyzerAction(action, reportDiagnostic, stats));
        }
    }

    private void RunSyntaxTreeActions(AnalyzerExecution execution)
    {
        foreach (var action in execution.SyntaxTreeActions)
        {
            _cancellationToken.ThrowIfCancellationRequested();

            var syntaxTreeContext = new SyntaxTreeAnalysisContext(
                _syntaxTree,
                _compilation,
                action.ReportDiagnostic,
                _cancellationToken);

            try
            {
                var actionTimestamp = Stopwatch.GetTimestamp();
                action.Action(syntaxTreeContext);
                action.Stats.SyntaxTreeActionTicks += Stopwatch.GetTimestamp() - actionTimestamp;
                action.Stats.SyntaxTreeActionCount++;
            }
            catch (OperationCanceledException)
            {
                throw;
            }
            catch
            {
                // Analyzer failures should not stop normal compilation diagnostics.
                action.Stats.SyntaxTreeActionCount++;
            }
        }
    }

    private void CollectSyntaxNodeActions(
        DiagnosticAnalyzer analyzer,
        AnalyzerExecution execution,
        Action<Diagnostic> reportDiagnostic,
        DocumentAnalyzerStats stats)
    {
        foreach (var registration in analyzer.SyntaxNodeActions)
        {
            _cancellationToken.ThrowIfCancellationRequested();

            var action = new DocumentSyntaxNodeAnalyzerAction(registration.Action, registration.Scope, reportDiagnostic, stats);
            foreach (var kind in registration.Kinds)
            {
                if (!execution.SyntaxNodeActionsByKind.TryGetValue(kind, out var actions))
                {
                    actions = [];
                    execution.SyntaxNodeActionsByKind[kind] = actions;
                }

                actions.Add(action);
            }
        }
    }

    private void RunAnalyzerExecutions(IReadOnlyList<AnalyzerExecution> executions)
    {
        if (executions.Count == 0)
            return;

        var sequentialExecutions = executions
            .Where(static execution => !execution.Analyzer.ConcurrentExecutionEnabled)
            .ToArray();
        var concurrentExecutions = executions
            .Where(static execution => execution.Analyzer.ConcurrentExecutionEnabled)
            .ToArray();

        foreach (var execution in sequentialExecutions)
        {
            _cancellationToken.ThrowIfCancellationRequested();
            RunAnalyzerExecutionWithoutSyntaxNodeActions(execution);
        }

        if (concurrentExecutions.Length > 0)
        {
            var parallelOptions = new ParallelOptions
            {
                CancellationToken = _cancellationToken,
                MaxDegreeOfParallelism = GetAnalyzerMaxDegreeOfParallelism(concurrentExecutions.Length)
            };

            Parallel.ForEach(concurrentExecutions, parallelOptions, RunAnalyzerExecutionWithoutSyntaxNodeActions);
        }

        RunSyntaxNodeActions(executions);
    }

    private void RunAnalyzerExecutionWithoutSyntaxNodeActions(AnalyzerExecution execution)
    {
        RunCompilationActions(execution);
        RunSyntaxTreeActions(execution);
    }

    private void RunSyntaxNodeActions(IReadOnlyList<AnalyzerExecution> executions)
    {
        var actionsByKind = CollectSyntaxNodeActionsByKind(executions);
        if (actionsByKind.Count == 0)
            return;

        var traversalTimestamp = Stopwatch.GetTimestamp();
        var nodeVisits = 0;
        var actionInvocations = 0;
        var root = _syntaxTree.GetRoot(_cancellationToken);
        var semanticModel = _compilation.GetSemanticModel(_syntaxTree);

        using var semanticLease = _semanticAccessAlreadyHeld
            ? null
            : semanticModel.EnterSemanticAccess(_cancellationToken);

        foreach (var node in root.DescendantNodesAndSelf())
        {
            _cancellationToken.ThrowIfCancellationRequested();
            nodeVisits++;

            if (!actionsByKind.TryGetValue(node.Kind, out var actions))
                continue;

            foreach (var action in actions)
            {
                _cancellationToken.ThrowIfCancellationRequested();
                actionInvocations++;
                RunSyntaxNodeAction(node, semanticModel, action);
            }
        }

        _syntaxTraversalTicks += Stopwatch.GetTimestamp() - traversalTimestamp;
        _syntaxTraversalNodeVisits += nodeVisits;
        _syntaxTraversalActionInvocations += actionInvocations;
        _syntaxTraversalKindCount = actionsByKind.Count;
    }

    private static Dictionary<SyntaxKind, List<DocumentSyntaxNodeAnalyzerAction>> CollectSyntaxNodeActionsByKind(
        IReadOnlyList<AnalyzerExecution> executions)
    {
        var actionsByKind = new Dictionary<SyntaxKind, List<DocumentSyntaxNodeAnalyzerAction>>();
        foreach (var execution in executions.OrderBy(static execution => execution.Stats.AnalyzerName, StringComparer.Ordinal))
        {
            foreach (var (kind, actions) in execution.SyntaxNodeActionsByKind.OrderBy(static pair => pair.Key))
            {
                if (!actionsByKind.TryGetValue(kind, out var mergedActions))
                {
                    mergedActions = [];
                    actionsByKind[kind] = mergedActions;
                }

                mergedActions.AddRange(actions);
            }
        }

        return actionsByKind;
    }

    private void MergeDiagnostics(IReadOnlyList<AnalyzerExecution> executions)
    {
        foreach (var execution in executions.OrderBy(static execution => execution.Stats.AnalyzerName, StringComparer.Ordinal))
        {
            foreach (var diagnostic in execution.Diagnostics.OrderBy(static diagnostic => diagnostic, DiagnosticComparer.Instance))
            {
                if (_diagnosticSet.Add(diagnostic))
                    _diagnostics.Add(diagnostic);
            }
        }
    }

    private void ReportSyntaxTraversalStats(IReadOnlyList<AnalyzerExecution> executions)
    {
        if (_syntaxTraversalTicks == 0)
            return;

        ReportWorkspaceEvent(
            "documentAnalyzer.syntaxTraversal",
            TicksToMilliseconds(_syntaxTraversalTicks),
            $"nodes={_syntaxTraversalNodeVisits}, actionInvocations={_syntaxTraversalActionInvocations}, kinds={_syntaxTraversalKindCount}");
    }

    private static int GetAnalyzerMaxDegreeOfParallelism(int analyzerCount)
        => Math.Clamp(Environment.ProcessorCount - 1, 1, Math.Max(1, analyzerCount));

    private void ReportActionPlan(IReadOnlyList<AnalyzerExecution> executions)
    {
        var compilationActionCount = executions.Sum(static execution => execution.CompilationActions.Count);
        var syntaxTreeActionCount = executions.Sum(static execution => execution.SyntaxTreeActions.Count);
        var syntaxNodeActions = executions
            .SelectMany(static execution => execution.SyntaxNodeActionsByKind.Values)
            .SelectMany(static actions => actions)
            .ToArray();
        var syntaxNodeActionCount = syntaxNodeActions.Length;
        var documentScopedSyntaxNodeActionCount = syntaxNodeActions.Count(static action => action.Scope == SyntaxNodeAnalysisScope.Document);
        var nodeScopedSyntaxNodeActionCount = syntaxNodeActions.Count(static action => action.Scope == SyntaxNodeAnalysisScope.Node);
        var syntaxNodeKindCount = executions
            .SelectMany(static execution => execution.SyntaxNodeActionsByKind.Keys)
            .Distinct()
            .Count();
        var concurrentAnalyzerCount = executions.Count(static execution => execution.Analyzer.ConcurrentExecutionEnabled);

        ReportWorkspaceEvent(
            "documentAnalyzer.actionPlan",
            elapsedMilliseconds: 0,
            $"analyzers={executions.Count}, concurrentAnalyzers={concurrentAnalyzerCount}, compilationActions={compilationActionCount}, syntaxTreeActions={syntaxTreeActionCount}, syntaxNodeActions={syntaxNodeActionCount}, documentScopedSyntaxNodeActions={documentScopedSyntaxNodeActionCount}, nodeScopedSyntaxNodeActions={nodeScopedSyntaxNodeActionCount}, syntaxNodeKinds={syntaxNodeKindCount}, maxDegreeOfParallelism={GetAnalyzerMaxDegreeOfParallelism(concurrentAnalyzerCount)}");
    }

    private void RunSyntaxNodeAction(
        SyntaxNode node,
        SemanticModel semanticModel,
        DocumentSyntaxNodeAnalyzerAction action)
    {
        var context = new SyntaxNodeAnalysisContext(
            node,
            semanticModel,
            _compilation,
            action.ReportDiagnostic,
            _cancellationToken);

        try
        {
            var actionTimestamp = Stopwatch.GetTimestamp();
            action.Action(context);
            action.Stats.SyntaxNodeActionTicks += Stopwatch.GetTimestamp() - actionTimestamp;
            action.Stats.SyntaxNodeActionCount++;
        }
        catch (OperationCanceledException)
        {
            throw;
        }
        catch
        {
            // Analyzer failures should not stop normal compilation diagnostics.
            action.Stats.SyntaxNodeActionCount++;
        }
    }

    private void ReportAnalyzerStats(IReadOnlyList<AnalyzerExecution> executions)
    {
        foreach (var stats in executions.Select(static execution => execution.Stats))
        {
            ReportWorkspaceEvent(
                "documentAnalyzer.analyzer",
                stats.TotalMilliseconds,
                $"{stats.AnalyzerName}: concurrent={stats.ConcurrentExecutionEnabled}, initMs={TicksToMilliseconds(stats.InitializationTicks):F1}, compilationActions={stats.CompilationActionCount}, compilationMs={TicksToMilliseconds(stats.CompilationActionTicks):F1}, syntaxTreeActions={stats.SyntaxTreeActionCount}, syntaxTreeMs={TicksToMilliseconds(stats.SyntaxTreeActionTicks):F1}, syntaxNodeActions={stats.SyntaxNodeActionCount}, syntaxNodeMs={TicksToMilliseconds(stats.SyntaxNodeActionTicks):F1}, diagnostics={stats.Diagnostics}");
        }
    }

    private void ReportWorkspaceEvent(
        string operation,
        double elapsedMilliseconds,
        string detail)
        => _eventSink?.Report(new WorkspaceEvent(
            operation,
            _project.Name,
            _syntaxTree.FilePath,
            elapsedMilliseconds,
            detail));

    private static bool ShouldRunAnalyzer(
        DiagnosticAnalyzer analyzer,
        CompilationOptions? compilationOptions,
        CompilationWithAnalyzersOptions? analyzerOptions)
    {
        _ = analyzerOptions;

        var options = compilationOptions ?? new CompilationOptions();
        return !AnalyzerOptionUtilities.IsAnalyzerDisabled(analyzer.GetType(), options.DisabledAnalyzers) &&
            (analyzer is not ICompilationOptionsAwareAnalyzer awareAnalyzer ||
             awareAnalyzer.ShouldAnalyze(options));
    }

    private IEnumerable<DiagnosticAnalyzer> GetEnabledAnalyzers()
        => _project.AnalyzerReferences
            .SelectMany(static reference => reference.GetAnalyzers())
            .Where(analyzer => ShouldRunAnalyzer(analyzer, _project.CompilationOptions, _analyzerOptions));

    private static double TicksToMilliseconds(long ticks)
        => ticks * 1000.0 / Stopwatch.Frequency;

    private sealed class AnalyzerExecution
    {
        public AnalyzerExecution(DiagnosticAnalyzer analyzer, DocumentAnalyzerStats stats)
        {
            Analyzer = analyzer;
            Stats = stats;
        }

        public DiagnosticAnalyzer Analyzer { get; }
        public DocumentAnalyzerStats Stats { get; }
        public List<Diagnostic> Diagnostics { get; } = [];
        public HashSet<Diagnostic> DiagnosticSet { get; } = [];
        public List<DocumentCompilationAnalyzerAction> CompilationActions { get; } = [];
        public List<DocumentSyntaxTreeAnalyzerAction> SyntaxTreeActions { get; } = [];
        public Dictionary<SyntaxKind, List<DocumentSyntaxNodeAnalyzerAction>> SyntaxNodeActionsByKind { get; } = [];
    }

    private readonly record struct DocumentCompilationAnalyzerAction(
        Action<CompilationAnalysisContext> Action,
        Action<Diagnostic> ReportDiagnostic,
        DocumentAnalyzerStats Stats);

    private readonly record struct DocumentSyntaxTreeAnalyzerAction(
        Action<SyntaxTreeAnalysisContext> Action,
        Action<Diagnostic> ReportDiagnostic,
        DocumentAnalyzerStats Stats);

    private readonly record struct DocumentSyntaxNodeAnalyzerAction(
        Action<SyntaxNodeAnalysisContext> Action,
        SyntaxNodeAnalysisScope Scope,
        Action<Diagnostic> ReportDiagnostic,
        DocumentAnalyzerStats Stats);

    private sealed class DocumentAnalyzerStats
    {
        public DocumentAnalyzerStats(string analyzerName)
        {
            AnalyzerName = analyzerName;
        }

        public string AnalyzerName { get; }
        public long InitializationTicks;
        public long CompilationActionTicks;
        public long SyntaxTreeActionTicks;
        public long SyntaxNodeActionTicks;
        public int CompilationActionCount;
        public int SyntaxTreeActionCount;
        public int SyntaxNodeActionCount;
        public int Diagnostics;
        public bool ConcurrentExecutionEnabled;

        public double TotalMilliseconds => TicksToMilliseconds(
            InitializationTicks +
            CompilationActionTicks +
            SyntaxTreeActionTicks +
            SyntaxNodeActionTicks);
    }
}
