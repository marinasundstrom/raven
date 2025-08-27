using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal partial class BoundReturnStatement : BoundStatement
{
    public BoundReturnStatement(BoundExpression? expression)
    {
        Expression = expression;
        Type = expression?.Type;
    }

    public BoundExpression? Expression { get; }

    public override ITypeSymbol? Type { get; }
}