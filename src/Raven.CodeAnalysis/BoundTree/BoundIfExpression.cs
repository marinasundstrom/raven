using System;
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

        if (thenType is null)
            return elseType ?? throw new InvalidOperationException("If expression must have a type.");

        if (elseType is null || SymbolEqualityComparer.Default.Equals(elseType, thenType))
            return thenType;

        return TypeSymbolNormalization.NormalizeUnion(new[] { thenType, elseType });
    }
}
