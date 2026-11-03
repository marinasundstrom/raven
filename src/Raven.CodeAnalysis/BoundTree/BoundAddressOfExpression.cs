using System;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class BoundAddressOfExpression : BoundExpression
{
    public BoundAddressOfExpression(Symbol? symbol, TypeSymbol valueType, BoundExpression? receiver = null, BoundExpression? storage = null, bool isOutDiscard = false, bool isUntypedOutDeclaration = false)
        : base(new AddressTypeSymbolImplementation(valueType ?? throw new ArgumentNullException(nameof(valueType))), symbol)
    {
        ValueType = valueType ?? throw new ArgumentNullException(nameof(valueType));
        Receiver = receiver;
        Storage = storage;
        IsOutDiscard = isOutDiscard;
        IsUntypedOutDeclaration = isUntypedOutDeclaration;
    }

    public BoundAddressOfExpression(BoundExpression storage, bool isUntypedOutDeclaration = false)
        : this(
            storage?.Symbol,
            storage?.Type ?? throw new ArgumentException("Storage expression must have a type.", nameof(storage)),
            GetReceiver(storage),
            storage,
            isUntypedOutDeclaration: isUntypedOutDeclaration)
    {
    }

    // Binding-only placeholder; selected out parameters supply concrete storage.
    public bool IsOutDiscard { get; }

    // The selected overload supplies the new local's type before emission.
    public bool IsUntypedOutDeclaration { get; }

    public bool IsTypeInferredOutArgument => IsOutDiscard || IsUntypedOutDeclaration;

    public BoundExpression? Receiver { get; }

    public BoundExpression? Storage { get; }

    public TypeSymbol ValueType { get; }

    public TypeSymbol ReferencedType => ValueType;

    private static BoundExpression? GetReceiver(BoundExpression storage)
    {
        return storage switch
        {
            BoundMemberAccessExpression member when member.Member is FieldSymbol => member.Receiver,
            BoundSelfExpression self => self,
            _ => null,
        };
    }
}
