using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal enum BoundCarrierKind
{
    Nullable,
    Option,
    Result
}

internal partial class BoundCarrierConditionalAccessExpression : BoundExpression
{
    public BoundExpression Receiver { get; }                 // the carrier expression
    public BoundCarrierKind CarrierKind { get; }             // Nullable/Option/Result
    public ITypeSymbol PayloadType { get; }                  // T
    public BoundExpression WhenPresent { get; }              // expression bound against a payload local
    public ITypeSymbol ResultType { get; }                   // carrier(U) or U?

    // This local is the key: lowering introduces it when extracting payload.
    public ILocalSymbol PayloadLocal { get; }

    // Resolved carrier API (preferred over reflection in codegen).
    // These are populated by binding for Option/Result carriers.
    public INamedTypeSymbol? CarrierType { get; }

    // Result<T,E>
    public INamedTypeSymbol? ResultOkCaseType { get; }
    public INamedTypeSymbol? ResultErrorCaseType { get; }
    public IMethodSymbol? ResultTryGetOkMethod { get; }
    public IMethodSymbol? ResultTryGetErrorMethod { get; }
    public IMethodSymbol? ResultOkValueGetter { get; }
    public IMethodSymbol? ResultErrorDataGetter { get; }
    public IMethodSymbol? ResultOkCtor { get; }
    public IMethodSymbol? ResultErrorCtor { get; }
    public IMethodSymbol? ResultImplicitFromOk { get; }
    public IMethodSymbol? ResultImplicitFromError { get; }

    // Receiver Result<TPayload, E> API (used to extract payload/error from the *receiver* carrier)
    public INamedTypeSymbol? ReceiverResultOkCaseType { get; }
    public INamedTypeSymbol? ReceiverResultErrorCaseType { get; }
    public IMethodSymbol? ReceiverResultOkValueGetter { get; }
    public IMethodSymbol? ReceiverResultErrorDataGetter { get; }

    // Option<T>
    public INamedTypeSymbol? OptionSomeCaseType { get; }
    public INamedTypeSymbol? OptionNoneCaseType { get; }
    public IMethodSymbol? OptionTryGetSomeMethod { get; }
    public IMethodSymbol? OptionSomeValueGetter { get; }
    public IMethodSymbol? OptionSomeCtor { get; }
    public IMethodSymbol? OptionNoneCtorOrFactory { get; }
    public IMethodSymbol? OptionImplicitFromSome { get; }
    public IMethodSymbol? OptionImplicitFromNone { get; }


    public override ITypeSymbol Type => ResultType;

    public BoundCarrierConditionalAccessExpression(
        BoundExpression receiver,
        BoundCarrierKind carrierKind,
        ITypeSymbol payloadType,
        ILocalSymbol payloadLocal,
        BoundExpression whenPresent,
        ITypeSymbol resultType,
        INamedTypeSymbol? carrierType = null,

        INamedTypeSymbol? receiverResultOkCaseType = null,
        INamedTypeSymbol? receiverResultErrorCaseType = null,
        IMethodSymbol? receiverResultOkValueGetter = null,
        IMethodSymbol? receiverResultErrorDataGetter = null,

        INamedTypeSymbol? resultOkCaseType = null,
        INamedTypeSymbol? resultErrorCaseType = null,
        IMethodSymbol? resultTryGetOkMethod = null,
        IMethodSymbol? resultTryGetErrorMethod = null,
        IMethodSymbol? resultOkValueGetter = null,
        IMethodSymbol? resultErrorDataGetter = null,
        IMethodSymbol? resultOkCtor = null,
        IMethodSymbol? resultErrorCtor = null,
        IMethodSymbol? resultImplicitFromOk = null,
        IMethodSymbol? resultImplicitFromError = null,

        INamedTypeSymbol? optionSomeCaseType = null,
        INamedTypeSymbol? optionNoneCaseType = null,
        IMethodSymbol? optionTryGetSomeMethod = null,
        IMethodSymbol? optionSomeValueGetter = null,
        IMethodSymbol? optionSomeCtor = null,
        IMethodSymbol? optionNoneCtorOrFactory = null,
        IMethodSymbol? optionImplicitFromSome = null,
        IMethodSymbol? optionImplicitFromNone = null)
        : base(resultType, symbol: null, BoundExpressionReason.None)
    {
        Receiver = receiver;
        CarrierKind = carrierKind;
        PayloadType = payloadType;
        PayloadLocal = payloadLocal;
        WhenPresent = whenPresent;
        ResultType = resultType;

        CarrierType = carrierType;
        ReceiverResultOkCaseType = receiverResultOkCaseType;
        ReceiverResultErrorCaseType = receiverResultErrorCaseType;
        ReceiverResultOkValueGetter = receiverResultOkValueGetter;
        ReceiverResultErrorDataGetter = receiverResultErrorDataGetter;

        ResultOkCaseType = resultOkCaseType;
        ResultErrorCaseType = resultErrorCaseType;
        ResultTryGetOkMethod = resultTryGetOkMethod;
        ResultTryGetErrorMethod = resultTryGetErrorMethod;
        ResultOkValueGetter = resultOkValueGetter;
        ResultErrorDataGetter = resultErrorDataGetter;
        ResultOkCtor = resultOkCtor;
        ResultErrorCtor = resultErrorCtor;
        ResultImplicitFromOk = resultImplicitFromOk;
        ResultImplicitFromError = resultImplicitFromError;

        OptionSomeCaseType = optionSomeCaseType;
        OptionNoneCaseType = optionNoneCaseType;
        OptionTryGetSomeMethod = optionTryGetSomeMethod;
        OptionSomeValueGetter = optionSomeValueGetter;
        OptionSomeCtor = optionSomeCtor;
        OptionNoneCtorOrFactory = optionNoneCtorOrFactory;
        OptionImplicitFromSome = optionImplicitFromSome;
        OptionImplicitFromNone = optionImplicitFromNone;
    }
}
