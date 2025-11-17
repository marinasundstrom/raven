using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal partial class BoundTypeExpression : BoundExpression
{
    public ITypeSymbol TypeSymbol { get; }

    /// <summary>
    /// When this type expression comes from a member access off another type (for example, Result&lt;int&gt;.Ok), this
    /// property keeps track of the constructed receiver so that later binding stages can substitute any outer type
    /// arguments without needing to construct a new nested type symbol up front.
    /// </summary>
    public ConstructedNamedTypeSymbol? ConstructedReceiver { get; }

    public BoundTypeExpression(ITypeSymbol typeSymbol, ConstructedNamedTypeSymbol? constructedReceiver = null)
        : base(typeSymbol, typeSymbol)
    {
        TypeSymbol = typeSymbol;
        ConstructedReceiver = constructedReceiver;
    }
}
