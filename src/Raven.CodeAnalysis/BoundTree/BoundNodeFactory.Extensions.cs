using System;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class BoundNodeFactory
{
    public BoundErrorExpression ErrorExpression(
        TypeSymbol? type = null,
        Symbol? symbol = null,
        BoundExpressionReason reason = BoundExpressionReason.None,
        ImmutableArray<Symbol> candidates = default)
        => CreateErrorExpression(type ?? Compilation.ErrorTypeSymbol, symbol, reason, candidates);

    public BoundUnitExpression UnitExpression(BoundExpressionReason reason = BoundExpressionReason.None)
        => CreateUnitExpression(reason);

    public BoundLiteralExpression NullLiteral(TypeSymbol? convertedType = null)
        => CreateLiteralExpression(BoundLiteralExpressionKind.NullLiteral, null!, Compilation.NullTypeSymbol, convertedType);

    public BoundMethodGroupExpression MethodGroupExpression(
        BoundExpression? receiver,
        ImmutableArray<MethodSymbol> methods,
        Func<TypeSymbol?>? delegateTypeFactory = null,
        MethodSymbol? selectedMethod = null,
        BoundExpressionReason reason = BoundExpressionReason.None)
        => CreateMethodGroupExpression(receiver, methods, Compilation.ErrorTypeSymbol, delegateTypeFactory, selectedMethod, reason);
}
