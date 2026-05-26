using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using System.Threading;

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
    private readonly HashSet<Diagnostic> _diagnostics = [];
    private readonly List<DocumentCompilationAnalyzerAction> _compilationActions = [];
    private readonly List<DocumentSyntaxTreeAnalyzerAction> _syntaxTreeActions = [];
    private readonly Dictionary<SyntaxKind, List<DocumentSyntaxNodeAnalyzerAction>> _syntaxNodeActionsByKind = [];
    private readonly List<DocumentAnalyzerStats> _analyzerStats = [];

    private DocumentAnalyzerDriver(
        Project project,
        SyntaxTree syntaxTree,
        Compilation compilation,
        CompilationWithAnalyzersOptions? analyzerOptions,
        IWorkspaceEventSink? eventSink,
        CancellationToken cancellationToken)
    {
        _project = project;
        _syntaxTree = syntaxTree;
        _compilation = compilation;
        _analyzerOptions = analyzerOptions;
        _eventSink = eventSink;
        _cancellationToken = cancellationToken;
    }

    public static ImmutableArray<Diagnostic> Run(
        Project project,
        SyntaxTree syntaxTree,
        Compilation compilation,
        CompilationWithAnalyzersOptions? analyzerOptions,
        IWorkspaceEventSink? eventSink,
        CancellationToken cancellationToken)
    {
        var driver = new DocumentAnalyzerDriver(
            project,
            syntaxTree,
            compilation,
            analyzerOptions,
            eventSink,
            cancellationToken);

        return driver.RunCore();
    }

    private ImmutableArray<Diagnostic> RunCore()
    {
        var totalTimestamp = Stopwatch.GetTimestamp();

        CollectActions();
        ReportActionPlan();
        RunCompilationActions();
        RunSyntaxTreeActions();
        RunSyntaxNodeActions();
        ReportAnalyzerStats();

        ReportWorkspaceEvent(
            "documentAnalyzer.total",
            Stopwatch.GetElapsedTime(totalTimestamp).TotalMilliseconds,
            $"analyzers={_analyzerStats.Count}, diagnostics={_diagnostics.Count}");

        return _diagnostics.OrderBy(d => d.Location).ToImmutableArray();
    }

    private void CollectActions()
    {
        foreach (var reference in _project.AnalyzerReferences)
        {
            _cancellationToken.ThrowIfCancellationRequested();

            foreach (var analyzer in reference.GetAnalyzers())
            {
                _cancellationToken.ThrowIfCancellationRequested();

                if (!ShouldRunAnalyzer(analyzer, _project.CompilationOptions, _analyzerOptions))
                    continue;

                CollectAnalyzerActions(analyzer);
            }
        }
    }

    private void CollectAnalyzerActions(DiagnosticAnalyzer analyzer)
    {
        var stats = new DocumentAnalyzerStats(analyzer.GetType().FullName ?? analyzer.GetType().Name);
        _analyzerStats.Add(stats);
        var isInternalAnalyzer = AnalyzerDiagnosticIdValidator.IsInternalAnalyzer(analyzer);
        var initializationTimestamp = Stopwatch.GetTimestamp();
        if (!analyzer.TryEnsureInitialized())
        {
            stats.InitializationTicks += Stopwatch.GetTimestamp() - initializationTimestamp;
            return;
        }

        stats.InitializationTicks += Stopwatch.GetTimestamp() - initializationTimestamp;

        void ReportDiagnostic(Diagnostic diagnostic)
        {
            AnalyzerDiagnosticIdValidator.Validate(analyzer, diagnostic, isInternalAnalyzer);

            var mapped = _compilation.ApplyCompilationOptions(
                diagnostic,
                _analyzerOptions?.ReportSuppressedDiagnostics ?? false);
            if (mapped is not null)
            {
                _diagnostics.Add(mapped);
                stats.Diagnostics++;
            }
        }

        CollectCompilationActions(analyzer, ReportDiagnostic, stats);
        CollectSyntaxTreeActions(analyzer, ReportDiagnostic, stats);
        CollectSyntaxNodeActions(analyzer, ReportDiagnostic, stats);
    }

    private void CollectCompilationActions(
        DiagnosticAnalyzer analyzer,
        Action<Diagnostic> reportDiagnostic,
        DocumentAnalyzerStats stats)
    {
        foreach (var action in analyzer.CompilationActions)
        {
            _cancellationToken.ThrowIfCancellationRequested();
            _compilationActions.Add(new DocumentCompilationAnalyzerAction(action, reportDiagnostic, stats));
        }
    }

    private void RunCompilationActions()
    {
        foreach (var action in _compilationActions)
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
        Action<Diagnostic> reportDiagnostic,
        DocumentAnalyzerStats stats)
    {
        foreach (var action in analyzer.SyntaxTreeActions)
        {
            _cancellationToken.ThrowIfCancellationRequested();
            _syntaxTreeActions.Add(new DocumentSyntaxTreeAnalyzerAction(action, reportDiagnostic, stats));
        }
    }

    private void RunSyntaxTreeActions()
    {
        foreach (var action in _syntaxTreeActions)
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
        Action<Diagnostic> reportDiagnostic,
        DocumentAnalyzerStats stats)
    {
        foreach (var registration in analyzer.SyntaxNodeActions)
        {
            _cancellationToken.ThrowIfCancellationRequested();

            var action = new DocumentSyntaxNodeAnalyzerAction(registration.Action, registration.Scope, reportDiagnostic, stats);
            foreach (var kind in registration.Kinds)
            {
                if (!_syntaxNodeActionsByKind.TryGetValue(kind, out var actions))
                {
                    actions = [];
                    _syntaxNodeActionsByKind[kind] = actions;
                }

                actions.Add(action);
            }
        }
    }

    private void RunSyntaxNodeActions()
    {
        if (_syntaxNodeActionsByKind.Count == 0)
            return;

        var traversalTimestamp = Stopwatch.GetTimestamp();
        var nodeVisits = 0;
        var actionInvocations = 0;
        var root = _syntaxTree.GetRoot(_cancellationToken);
        var semanticModel = _compilation.GetSemanticModel(_syntaxTree);

        foreach (var node in root.DescendantNodesAndSelf())
        {
            _cancellationToken.ThrowIfCancellationRequested();
            nodeVisits++;

            if (!_syntaxNodeActionsByKind.TryGetValue(node.Kind, out var actions))
                continue;

            foreach (var action in actions)
            {
                _cancellationToken.ThrowIfCancellationRequested();
                actionInvocations++;
                RunSyntaxNodeAction(node, semanticModel, action);
            }
        }

        ReportWorkspaceEvent(
            "documentAnalyzer.syntaxTraversal",
            Stopwatch.GetElapsedTime(traversalTimestamp).TotalMilliseconds,
            $"nodes={nodeVisits}, actionInvocations={actionInvocations}, kinds={_syntaxNodeActionsByKind.Count}");
    }

    private void ReportActionPlan()
    {
        var syntaxNodeActions = _syntaxNodeActionsByKind.Values.SelectMany(static actions => actions).ToArray();
        var syntaxNodeActionCount = syntaxNodeActions.Length;
        var documentScopedSyntaxNodeActionCount = syntaxNodeActions.Count(static action => action.Scope == SyntaxNodeAnalysisScope.Document);
        var nodeScopedSyntaxNodeActionCount = syntaxNodeActions.Count(static action => action.Scope == SyntaxNodeAnalysisScope.Node);
        ReportWorkspaceEvent(
            "documentAnalyzer.actionPlan",
            elapsedMilliseconds: 0,
            $"analyzers={_analyzerStats.Count}, compilationActions={_compilationActions.Count}, syntaxTreeActions={_syntaxTreeActions.Count}, syntaxNodeActions={syntaxNodeActionCount}, documentScopedSyntaxNodeActions={documentScopedSyntaxNodeActionCount}, nodeScopedSyntaxNodeActions={nodeScopedSyntaxNodeActionCount}, syntaxNodeKinds={_syntaxNodeActionsByKind.Count}");
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

    private void ReportAnalyzerStats()
    {
        foreach (var stats in _analyzerStats)
        {
            ReportWorkspaceEvent(
                "documentAnalyzer.analyzer",
                stats.TotalMilliseconds,
                $"{stats.AnalyzerName}: initMs={TicksToMilliseconds(stats.InitializationTicks):F1}, compilationActions={stats.CompilationActionCount}, compilationMs={TicksToMilliseconds(stats.CompilationActionTicks):F1}, syntaxTreeActions={stats.SyntaxTreeActionCount}, syntaxTreeMs={TicksToMilliseconds(stats.SyntaxTreeActionTicks):F1}, syntaxNodeActions={stats.SyntaxNodeActionCount}, syntaxNodeMs={TicksToMilliseconds(stats.SyntaxNodeActionTicks):F1}, diagnostics={stats.Diagnostics}");
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

    private static double TicksToMilliseconds(long ticks)
        => ticks * 1000.0 / Stopwatch.Frequency;

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

        public double TotalMilliseconds => TicksToMilliseconds(
            InitializationTicks +
            CompilationActionTicks +
            SyntaxTreeActionTicks +
            SyntaxNodeActionTicks);
    }
}
