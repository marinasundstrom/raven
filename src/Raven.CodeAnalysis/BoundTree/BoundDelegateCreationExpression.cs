using System;
using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

internal sealed partial class BoundDelegateCreationExpression : BoundExpression
{
    public BoundDelegateCreationExpression(BoundMethodGroupExpression methodGroup, TypeSymbol delegateType)
        : base(delegateType, methodGroup.SelectedMethod, methodGroup.Reason)
    {
        MethodGroup = methodGroup ?? throw new ArgumentNullException(nameof(methodGroup));
    }

    public BoundMethodGroupExpression MethodGroup { get; }

    public BoundExpression? Receiver => MethodGroup.Receiver;

    public ImmutableArray<MethodSymbol> Methods => MethodGroup.Methods;

    public MethodSymbol? Method => MethodGroup.SelectedMethod;

    public TypeSymbol DelegateType => Type;

    public override TypeSymbol? GetConvertedType() => MethodGroup.DelegateType ?? DelegateType;
}
