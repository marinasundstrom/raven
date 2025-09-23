using System;
using System.Collections.Immutable;
using System.Diagnostics;

namespace Raven.CodeAnalysis;

internal sealed partial class BoundMethodGroupExpression : BoundExpression
{
    private readonly Lazy<ITypeSymbol?>? _lazyDelegateType;

    public BoundMethodGroupExpression(
        BoundExpression? receiver,
        ImmutableArray<IMethodSymbol> methods,
        ITypeSymbol methodGroupType,
        Func<ITypeSymbol?>? delegateTypeFactory = null,
        IMethodSymbol? selectedMethod = null,
        BoundExpressionReason reason = BoundExpressionReason.None)
        : base(methodGroupType, selectedMethod ?? SelectMethod(methods), reason)
    {
        if (methods.IsDefaultOrEmpty)
            throw new ArgumentException("Method group must include at least one candidate.", nameof(methods));

        if (selectedMethod is not null)
            Debug.Assert(methods.Contains(selectedMethod, SymbolEqualityComparer.Default), "Selected method must belong to the candidate set.");

        Receiver = receiver;
        Methods = methods;
        MethodGroupType = methodGroupType;
        DelegateTypeFactory = delegateTypeFactory;
        _lazyDelegateType = delegateTypeFactory is null
            ? null
            : new Lazy<ITypeSymbol?>(delegateTypeFactory, isThreadSafe: false);
    }

    public BoundExpression? Receiver { get; }

    public ImmutableArray<IMethodSymbol> Methods { get; }

    public ITypeSymbol MethodGroupType { get; }

    public Func<ITypeSymbol?>? DelegateTypeFactory { get; }

    public IMethodSymbol? SelectedMethod => Symbol as IMethodSymbol;

    public ITypeSymbol? DelegateType => _lazyDelegateType?.Value;

    public override ITypeSymbol? GetConvertedType() => DelegateType;

    private static IMethodSymbol? SelectMethod(ImmutableArray<IMethodSymbol> methods)
    {
        return methods.Length == 1 ? methods[0] : null;
    }
}
