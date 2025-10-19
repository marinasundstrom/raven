using System;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class BoundAddressOfExpression : BoundExpression
{
    public BoundAddressOfExpression(ISymbol? symbol, ITypeSymbol valueType, BoundExpression? receiver = null, BoundExpression? storage = null)
        : base(new AddressTypeSymbol(valueType ?? throw new ArgumentNullException(nameof(valueType))), symbol)
    {
        ValueType = valueType ?? throw new ArgumentNullException(nameof(valueType));
        Receiver = receiver;
        Storage = storage;
    }

    public BoundAddressOfExpression(BoundExpression storage)
        : this(
            storage?.Symbol,
            storage?.Type ?? throw new ArgumentException("Storage expression must have a type.", nameof(storage)),
            GetReceiver(storage),
            storage)
    {
    }

    public BoundExpression? Receiver { get; }

    public BoundExpression? Storage { get; }

    public ITypeSymbol ValueType { get; }

    public ITypeSymbol ReferencedType => ValueType;

    private static BoundExpression? GetReceiver(BoundExpression storage)
    {
        return storage switch
        {
            BoundMemberAccessExpression member when member.Member is IFieldSymbol => member.Receiver,
            BoundSelfExpression self => self,
            _ => null,
        };
    }
}
