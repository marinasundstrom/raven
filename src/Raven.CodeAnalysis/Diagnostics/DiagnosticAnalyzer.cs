using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

/// <summary>Base class for analyzers that can produce diagnostics for a compilation.</summary>
public abstract class DiagnosticAnalyzer
{
    private bool _initialized;
    private readonly List<Action<SyntaxTreeAnalysisContext>> _syntaxTreeActions = new();
    private readonly List<SyntaxNodeActionRegistration> _syntaxNodeActions = new();

    /// <summary>Implement to register analysis actions.</summary>
    public abstract void Initialize(AnalysisContext context);

    /// <summary>Runs the analyzer for the specified compilation.</summary>
    public IEnumerable<Diagnostic> Analyze(Compilation compilation, CancellationToken cancellationToken = default)
    {
        if (!_initialized)
        {
            Initialize(new AnalysisContext(_syntaxTreeActions, _syntaxNodeActions));
            _initialized = true;
        }

        var diagnostics = new List<Diagnostic>();
        foreach (var tree in compilation.SyntaxTrees)
        {
            var treeContext = new SyntaxTreeAnalysisContext(tree, compilation, diagnostics.Add, cancellationToken);
            foreach (var action in _syntaxTreeActions)
                action(treeContext);

            if (_syntaxNodeActions.Count == 0)
                continue;

            var root = tree.GetRoot(cancellationToken);
            var semanticModel = compilation.GetSemanticModel(tree);

            foreach (var node in root.DescendantNodesAndSelf())
            {
                for (var i = 0; i < _syntaxNodeActions.Count; i++)
                {
                    var registration = _syntaxNodeActions[i];
                    if (!registration.Kinds.Contains(node.Kind))
                        continue;

                    var nodeContext = new SyntaxNodeAnalysisContext(
                        node,
                        semanticModel,
                        compilation,
                        diagnostics.Add,
                        cancellationToken);

                    registration.Action(nodeContext);
                }
            }
        }

        return diagnostics;
    }
}

/// <summary>Context used to register analysis actions.</summary>
public sealed class AnalysisContext
{
    private readonly List<Action<SyntaxTreeAnalysisContext>> _syntaxTreeActions;
    private readonly List<SyntaxNodeActionRegistration> _syntaxNodeActions;

    internal AnalysisContext(
        List<Action<SyntaxTreeAnalysisContext>> syntaxTreeActions,
        List<SyntaxNodeActionRegistration> syntaxNodeActions)
    {
        _syntaxTreeActions = syntaxTreeActions;
        _syntaxNodeActions = syntaxNodeActions;
    }

    /// <summary>Registers an action executed for each syntax tree in a compilation.</summary>
    public void RegisterSyntaxTreeAction(Action<SyntaxTreeAnalysisContext> action)
    {
        if (action is null)
            throw new ArgumentNullException(nameof(action));

        _syntaxTreeActions.Add(action);
    }

    /// <summary>
    /// Registers an action executed for syntax nodes whose <see cref="SyntaxNode.Kind"/> matches
    /// one of the provided <paramref name="syntaxKinds"/>.
    /// </summary>
    public void RegisterSyntaxNodeAction(Action<SyntaxNodeAnalysisContext> action, params SyntaxKind[] syntaxKinds)
    {
        if (action is null)
            throw new ArgumentNullException(nameof(action));

        if (syntaxKinds is null || syntaxKinds.Length == 0)
            throw new ArgumentException("At least one syntax kind must be provided.", nameof(syntaxKinds));

        _syntaxNodeActions.Add(new SyntaxNodeActionRegistration(
            action,
            syntaxKinds.ToHashSet()));
    }
}

/// <summary>Context for analyzing a single syntax tree.</summary>
public readonly struct SyntaxTreeAnalysisContext
{
    private readonly Action<Diagnostic> _reportDiagnostic;

    internal SyntaxTreeAnalysisContext(
        SyntaxTree syntaxTree,
        Compilation compilation,
        Action<Diagnostic> reportDiagnostic,
        CancellationToken cancellationToken)
    {
        SyntaxTree = syntaxTree;
        Compilation = compilation;
        _reportDiagnostic = reportDiagnostic;
        CancellationToken = cancellationToken;
    }

    /// <summary>The syntax tree being analyzed.</summary>
    public SyntaxTree SyntaxTree { get; }

    /// <summary>The compilation containing the syntax tree.</summary>
    public Compilation Compilation { get; }

    /// <summary>Cancellation token for the analysis.</summary>
    public CancellationToken CancellationToken { get; }

    /// <summary>Reports a diagnostic.</summary>
    public void ReportDiagnostic(Diagnostic diagnostic)
    {
        _reportDiagnostic(diagnostic);
    }
}

/// <summary>Context for analyzing a specific syntax node.</summary>
public readonly struct SyntaxNodeAnalysisContext
{
    private readonly Action<Diagnostic> _reportDiagnostic;

    internal SyntaxNodeAnalysisContext(
        SyntaxNode node,
        SemanticModel semanticModel,
        Compilation compilation,
        Action<Diagnostic> reportDiagnostic,
        CancellationToken cancellationToken)
    {
        Node = node;
        SemanticModel = semanticModel;
        Compilation = compilation;
        _reportDiagnostic = reportDiagnostic;
        CancellationToken = cancellationToken;
    }

    /// <summary>The syntax node being analyzed.</summary>
    public SyntaxNode Node { get; }

    /// <summary>The semantic model for the current syntax tree.</summary>
    public SemanticModel SemanticModel { get; }

    /// <summary>The compilation containing the syntax tree.</summary>
    public Compilation Compilation { get; }

    /// <summary>Cancellation token for the analysis.</summary>
    public CancellationToken CancellationToken { get; }

    /// <summary>Reports a diagnostic.</summary>
    public void ReportDiagnostic(Diagnostic diagnostic)
    {
        _reportDiagnostic(diagnostic);
    }
}

internal readonly record struct SyntaxNodeActionRegistration(
    Action<SyntaxNodeAnalysisContext> Action,
    HashSet<SyntaxKind> Kinds);
