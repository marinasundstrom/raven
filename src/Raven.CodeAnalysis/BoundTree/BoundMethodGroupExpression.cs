using System;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;

namespace Raven.CodeAnalysis;

internal sealed partial class BoundMethodGroupExpression : BoundExpression
{
    private readonly Lazy<TypeSymbol?>? _lazyDelegateType;

    public BoundMethodGroupExpression(
        BoundExpression? receiver,
        ImmutableArray<MethodSymbol> methods,
        TypeSymbol methodGroupType,
        Func<TypeSymbol?>? delegateTypeFactory = null,
        MethodSymbol? selectedMethod = null,
        BoundExpressionReason reason = BoundExpressionReason.None)
        : base(methodGroupType, selectedMethod ?? SelectMethod(methods), reason)
    {
        if (methods.IsDefaultOrEmpty)
            throw new ArgumentException("Method group must include at least one candidate.", nameof(methods));

        if (selectedMethod is not null)
        {
            Debug.Assert(
                methods.Any(method =>
                    SymbolEqualityComparer.Default.Equals(method, selectedMethod) ||
                    SymbolEqualityComparer.Default.Equals(
                        method.OriginalDefinition ?? method,
                        selectedMethod.OriginalDefinition ?? selectedMethod)),
                "Selected method must be constructed from the candidate set.");
        }

        Receiver = receiver;
        Methods = methods;
        MethodGroupType = methodGroupType;
        DelegateTypeFactory = delegateTypeFactory;
        _lazyDelegateType = delegateTypeFactory is null
            ? null
            : new Lazy<TypeSymbol?>(delegateTypeFactory, isThreadSafe: false);
    }

    public BoundExpression? Receiver { get; }

    public ImmutableArray<MethodSymbol> Methods { get; }

    public TypeSymbol MethodGroupType { get; }

    public Func<TypeSymbol?>? DelegateTypeFactory { get; }

    public MethodSymbol? SelectedMethod => Symbol as MethodSymbol;

    public TypeSymbol? DelegateType => _lazyDelegateType?.Value;

    public override TypeSymbol? GetConvertedType() => DelegateType;

    private static MethodSymbol? SelectMethod(ImmutableArray<MethodSymbol> methods)
    {
        return methods.Length == 1 ? methods[0] : null;
    }
}
