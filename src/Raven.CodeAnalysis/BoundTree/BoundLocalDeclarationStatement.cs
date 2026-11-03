using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

sealed partial class BoundLocalDeclarationStatement : BoundStatement
{
    public IEnumerable<BoundVariableDeclarator> Declarators { get; }
    public bool IsUsing { get; }

    public BoundLocalDeclarationStatement(IEnumerable<BoundVariableDeclarator> declarators, bool isUsing = false)
    {
        Declarators = declarators.ToImmutableArray();
        IsUsing = isUsing;
    }

    public override Symbol Symbol => Declarators.First().Local;
}

sealed partial class BoundVariableDeclarator : BoundNode
{
    public LocalSymbol Local { get; }
    public BoundExpression? Initializer { get; }
    public BoundAddressOfExpression? FixedAddressInitializer { get; }
    public LocalSymbol? FixedPinnedLocal { get; }

    public BoundVariableDeclarator(
        LocalSymbol local,
        BoundExpression? initializer,
        BoundAddressOfExpression? fixedAddressInitializer = null,
        LocalSymbol? fixedPinnedLocal = null)
    {
        Local = local;
        Initializer = initializer;
        FixedAddressInitializer = fixedAddressInitializer;
        FixedPinnedLocal = fixedPinnedLocal;
    }

    public Symbol Symbol => Local;
    public TypeSymbol Type => Local.Type;
}
