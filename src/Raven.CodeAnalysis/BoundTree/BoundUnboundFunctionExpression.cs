using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal sealed class BoundUnboundFunctionExpression
{
    public SourceLambdaSymbol LambdaSymbol { get; }
    public ExpressionSyntax Syntax { get; }
    public ImmutableArray<ParameterSymbol> Parameters { get; }
    public ImmutableArray<NamedTypeSymbol> CandidateDelegates { get; }
    public ImmutableArray<SuppressedLambdaDiagnostic> SuppressedDiagnostics { get; }

    internal BoundUnboundFunctionExpression(
        SourceLambdaSymbol lambdaSymbol,
        ExpressionSyntax syntax,
        ImmutableArray<ParameterSymbol> parameters,
        ImmutableArray<NamedTypeSymbol> candidateDelegates,
        ImmutableArray<SuppressedLambdaDiagnostic> suppressedDiagnostics)
    {
        LambdaSymbol = lambdaSymbol;
        Syntax = syntax;
        Parameters = parameters;
        CandidateDelegates = candidateDelegates.IsDefault
            ? ImmutableArray<NamedTypeSymbol>.Empty
            : candidateDelegates;
        SuppressedDiagnostics = suppressedDiagnostics.IsDefault
            ? ImmutableArray<SuppressedLambdaDiagnostic>.Empty
            : suppressedDiagnostics;
    }

    internal void ReportSuppressedDiagnostics(DiagnosticBag diagnostics)
    {
        foreach (var suppression in SuppressedDiagnostics)
            diagnostics.ReportLambdaParameterTypeCannotBeInferred(suppression.ParameterName, suppression.Location);
    }
}

internal readonly struct SuppressedLambdaDiagnostic
{
    public string ParameterName { get; }
    public Location Location { get; }

    public SuppressedLambdaDiagnostic(string parameterName, Location location)
    {
        ParameterName = parameterName;
        Location = location;
    }
}
