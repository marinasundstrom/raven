using System;
using System.Collections.Generic;
using System.Threading;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

/// <summary>Base class for analyzers that can produce diagnostics for a compilation.</summary>
public abstract class DiagnosticAnalyzer
{
    private bool _initialized;
    private readonly List<Action<SyntaxTreeAnalysisContext>> _syntaxTreeActions = new();

    /// <summary>Implement to register analysis actions.</summary>
    public abstract void Initialize(AnalysisContext context);

    /// <summary>Runs the analyzer for the specified compilation.</summary>
    public IEnumerable<Diagnostic> Analyze(Compilation compilation, CancellationToken cancellationToken = default)
    {
        if (!_initialized)
        {
            Initialize(new AnalysisContext(_syntaxTreeActions));
            _initialized = true;
        }

        var diagnostics = new List<Diagnostic>();
        foreach (var tree in compilation.SyntaxTrees)
        {
            var treeContext = new SyntaxTreeAnalysisContext(tree, compilation, diagnostics.Add, cancellationToken);
            foreach (var action in _syntaxTreeActions)
                action(treeContext);
        }

        return diagnostics;
    }
}

/// <summary>Context used to register analysis actions.</summary>
public sealed class AnalysisContext
{
    private readonly List<Action<SyntaxTreeAnalysisContext>> _syntaxTreeActions;

    internal AnalysisContext(List<Action<SyntaxTreeAnalysisContext>> syntaxTreeActions)
    {
        _syntaxTreeActions = syntaxTreeActions;
    }

    /// <summary>Registers an action executed for each syntax tree in a compilation.</summary>
    public void RegisterSyntaxTreeAction(Action<SyntaxTreeAnalysisContext> action)
    {
        if (action is null)
            throw new ArgumentNullException(nameof(action));

        _syntaxTreeActions.Add(action);
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
