using System;
using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

internal sealed partial class BoundDelegateCreationExpression : BoundExpression
{
    public BoundDelegateCreationExpression(BoundMethodGroupExpression methodGroup, TypeSymbol delegateType)
        : base(delegateType, (methodGroup ?? throw new ArgumentNullException(nameof(methodGroup))).SelectedMethod, methodGroup.Reason)
    {
        MethodGroup = methodGroup;
    }

    public BoundMethodGroupExpression MethodGroup { get; }

    public BoundExpression? Receiver => MethodGroup.Receiver;

    public ImmutableArray<MethodSymbol> Methods => MethodGroup.Methods;

    public MethodSymbol? Method => MethodGroup.SelectedMethod;

    public TypeSymbol DelegateType => Type ?? throw new InvalidOperationException("A bound delegate creation must have a type.");

    public override TypeSymbol? GetConvertedType() => MethodGroup.DelegateType ?? DelegateType;
}
