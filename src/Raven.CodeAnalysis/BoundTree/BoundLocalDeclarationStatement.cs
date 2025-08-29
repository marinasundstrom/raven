namespace Raven.CodeAnalysis;

sealed partial class BoundLocalDeclarationStatement : BoundStatement
{
    public IEnumerable<BoundVariableDeclarator> Declarators { get; }

    public BoundLocalDeclarationStatement(IEnumerable<BoundVariableDeclarator> declarators)
    {
        Declarators = declarators;
    }

    public override ISymbol Symbol => Declarators.First().Local;
}

sealed partial class BoundVariableDeclarator : BoundNode
{
    public ILocalSymbol Local { get; }
    public BoundExpression? Initializer { get; }

    public BoundVariableDeclarator(ILocalSymbol local, BoundExpression? initializer)
    {
        Local = local;
        Initializer = initializer;
    }

    public ISymbol Symbol => Local;
    public ITypeSymbol Type => Local.Type;
}