namespace Raven.CodeAnalysis;

sealed partial class BoundLocalDeclarationStatement : BoundStatement
{
    public IReadOnlyList<BoundVariableDeclarator> Declarators { get; }

    public BoundLocalDeclarationStatement(IReadOnlyList<BoundVariableDeclarator> declarators)
    {
        Declarators = declarators;
    }

    public override ISymbol Symbol => Declarators.First().Local;
    public override ITypeSymbol Type => Declarators.First().Type;
}

sealed partial class BoundVariableDeclarator : BoundNode
{
    public ILocalSymbol Local { get; }
    public BoundExpression? Initializer { get; }

    public BoundVariableDeclarator(ILocalSymbol symbol, BoundExpression? initializer)
    {
        Local = symbol;
        Initializer = initializer;
    }

    public ISymbol Symbol => Local;
    public ITypeSymbol Type => Local.Type;
}