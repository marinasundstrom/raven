using System;
using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

internal sealed partial class BoundDelegateCreationExpression : BoundExpression
{
    public BoundDelegateCreationExpression(BoundMethodGroupExpression methodGroup, ITypeSymbol delegateType)
        : base(delegateType, methodGroup.SelectedMethod, methodGroup.Reason)
    {
        MethodGroup = methodGroup ?? throw new ArgumentNullException(nameof(methodGroup));
    }

    public BoundMethodGroupExpression MethodGroup { get; }

    public BoundExpression? Receiver => MethodGroup.Receiver;

    public ImmutableArray<IMethodSymbol> Methods => MethodGroup.Methods;

    public IMethodSymbol? Method => MethodGroup.SelectedMethod;

    public ITypeSymbol DelegateType => Type;

    public override ITypeSymbol? GetConvertedType() => MethodGroup.DelegateType ?? DelegateType;
}
