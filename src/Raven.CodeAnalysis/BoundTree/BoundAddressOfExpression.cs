using System;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class BoundAddressOfExpression : BoundExpression
{
    public BoundAddressOfExpression(ISymbol symbol, ITypeSymbol valueType, BoundExpression? receiver = null)
        : base(new ByRefTypeSymbol(valueType), symbol)
    {
        ValueType = valueType ?? throw new ArgumentNullException(nameof(valueType));
        Receiver = receiver;
    }

    public BoundExpression? Receiver { get; }

    public ITypeSymbol ValueType { get; }

    public ITypeSymbol ReferencedType => ((ByRefTypeSymbol)Type).ElementType;
}
