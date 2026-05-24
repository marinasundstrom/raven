using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

/// <summary>Base class for analyzers that can produce diagnostics for a compilation.</summary>
public abstract class DiagnosticAnalyzer
{
    private bool _initialized;
    private readonly List<Action<CompilationAnalysisContext>> _compilationActions = new();
    private readonly List<Action<SyntaxTreeAnalysisContext>> _syntaxTreeActions = new();
    private readonly List<SyntaxNodeActionRegistration> _syntaxNodeActions = new();

    /// <summary>Implement to register analysis actions.</summary>
    public abstract void Initialize(AnalysisContext context);

    public virtual ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => [];

    /// <summary>Runs the analyzer for the specified compilation.</summary>
    public IEnumerable<Diagnostic> Analyze(Compilation compilation, CancellationToken cancellationToken = default)
        => Analyze(compilation, syntaxTree: null, cancellationToken);

    /// <summary>Runs the analyzer for the specified syntax tree in the compilation.</summary>
    public IEnumerable<Diagnostic> Analyze(Compilation compilation, SyntaxTree? syntaxTree, CancellationToken cancellationToken = default)
    {
        if (!_initialized)
        {
            try
            {
                Initialize(new AnalysisContext(_compilationActions, _syntaxTreeActions, _syntaxNodeActions));
                _initialized = true;
            }
            catch (OperationCanceledException)
            {
                throw;
            }
            catch
            {
                return [];
            }
        }

        var diagnostics = new List<Diagnostic>();
        var syntaxTrees = syntaxTree is null
            ? compilation.SyntaxTrees
            : [syntaxTree];

        foreach (var action in _compilationActions)
        {
            var compilationContext = new CompilationAnalysisContext(
                compilation,
                syntaxTree,
                diagnostics.Add,
                cancellationToken);

            try
            {
                action(compilationContext);
            }
            catch (OperationCanceledException)
            {
                throw;
            }
            catch
            {
                // Analyzer failures should not stop compilation.
            }
        }

        foreach (var tree in syntaxTrees)
        {
            var treeContext = new SyntaxTreeAnalysisContext(tree, compilation, diagnostics.Add, cancellationToken);
            foreach (var action in _syntaxTreeActions)
            {
                try
                {
                    action(treeContext);
                }
                catch (OperationCanceledException)
                {
                    throw;
                }
                catch
                {
                    // Analyzer failures should not stop compilation.
                }
            }

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

                    try
                    {
                        registration.Action(nodeContext);
                    }
                    catch (OperationCanceledException)
                    {
                        throw;
                    }
                    catch
                    {
                        // Analyzer failures should not stop compilation.
                    }
                }
            }
        }

        return diagnostics;
    }
}

public interface ICompilationOptionsAwareAnalyzer
{
    bool ShouldAnalyze(CompilationOptions options);
}

/// <summary>Context used to register analysis actions.</summary>
public sealed class AnalysisContext
{
    private readonly List<Action<CompilationAnalysisContext>> _compilationActions;
    private readonly List<Action<SyntaxTreeAnalysisContext>> _syntaxTreeActions;
    private readonly List<SyntaxNodeActionRegistration> _syntaxNodeActions;

    internal AnalysisContext(
        List<Action<CompilationAnalysisContext>> compilationActions,
        List<Action<SyntaxTreeAnalysisContext>> syntaxTreeActions,
        List<SyntaxNodeActionRegistration> syntaxNodeActions)
    {
        _compilationActions = compilationActions;
        _syntaxTreeActions = syntaxTreeActions;
        _syntaxNodeActions = syntaxNodeActions;
    }

    /// <summary>Registers an action executed once for the compilation being analyzed.</summary>
    public void RegisterCompilationAction(Action<CompilationAnalysisContext> action)
    {
        if (action is null)
            throw new ArgumentNullException(nameof(action));

        _compilationActions.Add(action);
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

/// <summary>Context for analyzing a compilation.</summary>
public readonly struct CompilationAnalysisContext
{
    private readonly Action<Diagnostic> _reportDiagnostic;

    internal CompilationAnalysisContext(
        Compilation compilation,
        SyntaxTree? syntaxTree,
        Action<Diagnostic> reportDiagnostic,
        CancellationToken cancellationToken)
    {
        Compilation = compilation;
        SyntaxTree = syntaxTree;
        _reportDiagnostic = reportDiagnostic;
        CancellationToken = cancellationToken;
    }

    /// <summary>The compilation being analyzed.</summary>
    public Compilation Compilation { get; }

    /// <summary>
    /// The syntax tree being analyzed, or <c>null</c> when the analyzer is running for the whole compilation.
    /// </summary>
    public SyntaxTree? SyntaxTree { get; }

    /// <summary>Cancellation token for the analysis.</summary>
    public CancellationToken CancellationToken { get; }

    /// <summary>Reports a diagnostic.</summary>
    public void ReportDiagnostic(Diagnostic diagnostic)
    {
        _reportDiagnostic(diagnostic);
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
