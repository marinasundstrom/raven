
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal class BoundIfExpression : BoundExpression
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
        if (!elseBranch?.Type?.Equals(thenBranch.Type, SymbolEqualityComparer.Default) ?? false)
        {
            return new UnionTypeSymbol([thenBranch.Type, elseBranch.Type], null, null, null, []);
        }

        return thenBranch.Type;
    }
}