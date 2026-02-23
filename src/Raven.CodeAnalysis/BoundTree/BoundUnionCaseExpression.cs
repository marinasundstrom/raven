using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

/// <summary>
/// Represents a union case value in expression position.
///
/// The <see cref="BoundExpression.Type"/> is the <em>union root</em> type (e.g. <c>Err</c>),
/// not the individual case type (e.g. <c>Err_MissingName</c>). This is critical for correct
/// generic type inference: when this expression is passed to a generic method such as
/// <c>Error&lt;T,E&gt;(E error)</c> the inferred type argument for <c>E</c> will be the union
/// root rather than the case type, so the resulting <c>Result&lt;T,E&gt;</c> matches what the
/// caller expects.
///
/// The node is lowered by <see cref="Lowerer"/> to a
/// <see cref="BoundConversionExpression"/> whose operand is a
/// <see cref="BoundObjectCreationExpression"/> that constructs the case instance; the DU
/// conversion then sets the union's tag and payload fields.
/// </summary>
internal sealed partial class BoundUnionCaseExpression : BoundExpression
{
    /// <summary>The concrete case type (e.g. <c>Err_MissingName</c>).</summary>
    public INamedTypeSymbol CaseType { get; }

    /// <summary>
    /// The constructor used to create the case instance.
    /// May be <see langword="null"/> for unit-payload cases that have not yet resolved a
    /// constructor (handled during lowering).
    /// </summary>
    public IMethodSymbol? CaseConstructor { get; }

    /// <summary>Arguments to the case constructor (empty for zero-arg cases).</summary>
    public ImmutableArray<BoundExpression> Arguments { get; }

    /// <summary>The union root type — same as <see cref="BoundExpression.Type"/> but strongly typed.</summary>
    public INamedTypeSymbol UnionType => (INamedTypeSymbol)Type;

    public BoundUnionCaseExpression(
        INamedTypeSymbol unionType,
        INamedTypeSymbol caseType,
        IMethodSymbol? caseConstructor,
        ImmutableArray<BoundExpression> arguments)
        : base(unionType, caseConstructor, BoundExpressionReason.None)
    {
        CaseType = caseType;
        CaseConstructor = caseConstructor;
        Arguments = arguments;
    }

    public override string ToString()
        => $"new {CaseType.Name}({string.Join(", ", Arguments)}) as {Type.Name}";
}
