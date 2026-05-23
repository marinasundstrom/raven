using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal sealed class BoundUnboundFunctionExpression
{
    public SourceLambdaSymbol LambdaSymbol { get; }
    public ExpressionSyntax Syntax { get; }
    public ImmutableArray<IParameterSymbol> Parameters { get; }
    public ImmutableArray<INamedTypeSymbol> CandidateDelegates { get; }
    public ImmutableArray<SuppressedLambdaDiagnostic> SuppressedDiagnostics { get; }
    public ImplicitReceiverKind ImplicitReceiverKind { get; }
    public ITypeSymbol? ImplicitReceiverLookupType { get; }
    public ISymbol? ImplicitReceiverSymbol { get; }
    public bool HasImplicitReceiverParameter => ImplicitReceiverKind == ImplicitReceiverKind.Parameter;

    internal BoundUnboundFunctionExpression(
        SourceLambdaSymbol lambdaSymbol,
        ExpressionSyntax syntax,
        ImmutableArray<IParameterSymbol> parameters,
        ImmutableArray<INamedTypeSymbol> candidateDelegates,
        ImmutableArray<SuppressedLambdaDiagnostic> suppressedDiagnostics,
        ImplicitReceiverKind implicitReceiverKind = ImplicitReceiverKind.None,
        ITypeSymbol? implicitReceiverLookupType = null,
        ISymbol? implicitReceiverSymbol = null)
    {
        LambdaSymbol = lambdaSymbol;
        Syntax = syntax;
        Parameters = parameters;
        CandidateDelegates = candidateDelegates.IsDefault
            ? ImmutableArray<INamedTypeSymbol>.Empty
            : candidateDelegates;
        SuppressedDiagnostics = suppressedDiagnostics.IsDefault
            ? ImmutableArray<SuppressedLambdaDiagnostic>.Empty
            : suppressedDiagnostics;
        ImplicitReceiverKind = implicitReceiverKind;
        ImplicitReceiverLookupType = implicitReceiverLookupType;
        ImplicitReceiverSymbol = implicitReceiverSymbol;
    }

    internal BoundUnboundFunctionExpression WithImplicitReceiver(ISymbol? symbol, ITypeSymbol? lookupType, ImplicitReceiverKind kind)
    {
        return new BoundUnboundFunctionExpression(
            LambdaSymbol,
            Syntax,
            Parameters,
            CandidateDelegates,
            SuppressedDiagnostics,
            kind,
            lookupType,
            symbol);
    }

    internal void ReportSuppressedDiagnostics(DiagnosticBag diagnostics)
    {
        foreach (var suppression in SuppressedDiagnostics)
            diagnostics.ReportLambdaParameterTypeCannotBeInferred(suppression.ParameterName, suppression.Location);
    }
}

internal enum ImplicitReceiverKind
{
    None,
    Parameter,
    Local
}

internal readonly struct ImplicitReceiverSymbol
{
    public ImplicitReceiverSymbol(ISymbol symbol, ITypeSymbol lookupType)
    {
        Symbol = symbol;
        LookupType = lookupType;
    }

    public ISymbol Symbol { get; }
    public ITypeSymbol LookupType { get; }
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
