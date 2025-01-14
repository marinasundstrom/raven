using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class SemanticModel
{
    public static DataFlowAnalysis AnalyzeDataFlow(ExpressionSyntax expression) 
    {

        return new DataFlowAnalysis();
    }

    public static DataFlowAnalysis AnalyzeDataFlow(StatementSyntax statement) 
    {

        return new DataFlowAnalysis();
    }

    public static DataFlowAnalysis AnalyzeDataFlow(StatementSyntax firstStatement, StatementSyntax lastStatement) 
    {

        return new DataFlowAnalysis();
    }
}

public sealed class DataFlowAnalysis
{
    public DataFlowAnalysis()
    {
    }

    /// <summary>
    /// The set of local variables for which a value is always assigned inside a region.
    /// </summary>
    public ImmutableArray<ISymbol> AlwaysAssigned { get; init; }

    /// <summary>
    /// The set of the local variables that have been referenced in anonymous functions and therefore must be moved to a field of a frame class.
    /// </summary>
    public ImmutableArray<ISymbol> Captured { get; init; }

    /// <summary>
    /// The set of variables that are captured inside a region.
    /// </summary>
    public ImmutableArray<ISymbol> CapturedInside { get; init; }

    /// <summary>
    /// The set of variables that are captured outside a region.
    /// </summary>
    public ImmutableArray<ISymbol> CapturedOutside { get; init; }

    /// <summary>
    /// The set of local variables which are assigned a value outside a region that may be used inside the region.
    /// </summary>
    public ImmutableArray<ISymbol> DataFlowsIn { get; init; }

    /// <summary>
    /// The set of local variables which are assigned a value inside a region that may be used outside the region.
    /// </summary>
    public ImmutableArray<ISymbol> DataFlowsOut { get; init; }

    /// <summary>
    /// The set of local variables which are definitely assigned a value when a region is entered.
    /// </summary>
    public ImmutableArray<ISymbol> DefinitelyAssignedOnEntry { get; init; }

    /// <summary>
    /// The set of local variables which are definitely assigned a value when a region is exited.
    /// </summary>
    public ImmutableArray<ISymbol> DefinitelyAssignedOnExit { get; init; }

    /// <summary>
    /// The set of local variables that are read inside a region.
    /// </summary>
    public ImmutableArray<ISymbol> ReadInside { get; init; }

    /// <summary>
    /// The set of the local variables that are read outside a region.
    /// </summary>
    public ImmutableArray<ISymbol> ReadOutside { get; init; }

    /// <summary>
    /// Returns true if and only if analysis was successful. Analysis can fail if the region does not properly span a single expression, a single statement, or a contiguous series of statements within the enclosing block.
    /// </summary>
    public bool Succeeded { get; init; }

    /// <summary>
    /// The set of local variables that are declared within a region. Note that the region must be bounded by a method's body or a field's initializer, so parameter symbols are never included in the result.
    /// </summary>
    public ImmutableArray<ISymbol> VariablesDeclared { get; init; }

    /// <summary>
    /// The set of local variables that are written inside a region.
    /// </summary>
    public ImmutableArray<ISymbol> WrittenInside { get; init; }

    /// <summary>
    /// The set of local variables that are written outside a region.
    /// </summary>
    public ImmutableArray<ISymbol> WritteOutside { get; init; }
}