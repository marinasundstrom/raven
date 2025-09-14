
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal partial class BoundIfExpression : BoundExpression
{
    public BoundExpression Condition { get; }
    public BoundExpression ThenBranch { get; }
    public BoundExpression? ElseBranch { get; }

    public BoundIfExpression(BoundExpression condition, BoundExpression thenBranch, BoundExpression? elseBranch = null)
        : base(Handle(thenBranch, elseBranch), null, BoundExpressionReason.None)
    {
        Condition = condition;
        ThenBranch = thenBranch;
        ElseBranch = elseBranch;
    }

    private static ITypeSymbol Handle(BoundExpression thenBranch, BoundExpression? elseBranch)
    {
        var thenType = thenBranch.Type;
        var elseType = elseBranch?.Type;

        if (elseType is not null && !SymbolEqualityComparer.Default.Equals(elseType, thenType))
            return new UnionTypeSymbol([thenType, elseType], null, null, null, []);

        return thenType!;
    }
}
