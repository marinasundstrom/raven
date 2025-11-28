using System;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class BoundNodeFactory
{
    public BoundErrorExpression ErrorExpression(
        ITypeSymbol? type = null,
        ISymbol? symbol = null,
        BoundExpressionReason reason = BoundExpressionReason.None,
        ImmutableArray<ISymbol> candidates = default)
        => CreateErrorExpression(type ?? Compilation.ErrorTypeSymbol, symbol, reason, candidates);

    public BoundUnitExpression UnitExpression(BoundExpressionReason reason = BoundExpressionReason.None)
        => CreateUnitExpression(reason);

    public BoundLiteralExpression NullLiteral(ITypeSymbol? convertedType = null)
        => CreateLiteralExpression(BoundLiteralExpressionKind.NullLiteral, null!, Compilation.NullTypeSymbol, convertedType);

    public BoundMethodGroupExpression MethodGroupExpression(
        BoundExpression? receiver,
        ImmutableArray<IMethodSymbol> methods,
        Func<ITypeSymbol?>? delegateTypeFactory = null,
        IMethodSymbol? selectedMethod = null,
        BoundExpressionReason reason = BoundExpressionReason.None)
        => CreateMethodGroupExpression(receiver, methods, Compilation.ErrorTypeSymbol, delegateTypeFactory, selectedMethod, reason);
}
