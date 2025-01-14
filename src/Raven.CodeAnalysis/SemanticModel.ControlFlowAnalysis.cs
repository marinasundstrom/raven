using System;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class SemanticModel
{
    public static ControlFlowAnalysis AnalyzeControlFlow(ExpressionSyntax expression) 
    {
        
        return new ControlFlowAnalysis();
    }
}

public sealed class ControlFlowAnalysis 
{
    /// <summary>
    /// ndicates whether a region completes normally. Return true if and only if the end of the last statement in a region is reachable or the region contains no statements.
    /// </summary>
    public bool EndPointIsReachable { get; init; }

    /// <summary>
    /// The set of statements inside the region what are the destination of branches outside the region.
    /// </summary>
    public ImmutableArray<SyntaxNode>  EntryPoints { get; init; }

    /// <summary>
    /// The set of statements inside a region that jump to locations outside the region.
    /// </summary>
    public ImmutableArray<SyntaxNode>  ExitPoints { get; init; }

    /// <summary>
    /// The set of return statements found within a region.
    /// </summary>
    public ImmutableArray<SyntaxNode> ReturnStatements { get; init; }

    public bool StartPointIsReachable { get; init; }

    /// <summary>
    /// Returns true if and only if analysis was successful. Analysis can fail if the region does not properly span a single expression, a single statement, or a contiguous series of statements within the enclosing block
    /// </summary>
    public bool Succeeded { get; init; }
}