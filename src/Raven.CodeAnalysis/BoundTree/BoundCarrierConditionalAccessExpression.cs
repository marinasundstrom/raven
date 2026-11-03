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
    Optional,
    Result
}

internal partial class BoundCarrierConditionalAccessExpression : BoundExpression
{
    public BoundExpression Receiver { get; }                 // the carrier expression
    public BoundCarrierKind CarrierKind { get; }             // Nullable/Optional/Result
    public TypeSymbol PayloadType { get; }                  // T
    public BoundExpression WhenPresent { get; }              // expression bound against a payload local
    public TypeSymbol ResultType { get; }                   // carrier(U) or U?

    // This local is the key: lowering introduces it when extracting payload.
    public LocalSymbol PayloadLocal { get; }

    // Resolved carrier API (preferred over reflection in codegen).
    // These are populated by binding for Optional/Result carriers.
    public NamedTypeSymbol? CarrierType { get; }

    // Result<T,E>
    public NamedTypeSymbol? ResultOkCaseType { get; }
    public NamedTypeSymbol? ResultErrorCaseType { get; }
    public MethodSymbol? ResultTryGetValueForOkCaseMethod { get; }
    public MethodSymbol? ResultTryGetValueForErrorCaseMethod { get; }
    public MethodSymbol? ResultOkValueGetter { get; }
    public MethodSymbol? ResultErrorDataGetter { get; }
    public MethodSymbol? ResultOkCtor { get; }
    public MethodSymbol? ResultErrorCtor { get; }
    public MethodSymbol? ResultImplicitFromOk { get; }
    public MethodSymbol? ResultImplicitFromError { get; }

    // Receiver Result<TPayload, E> API (used to extract payload/error from the *receiver* carrier)
    public NamedTypeSymbol? ReceiverResultOkCaseType { get; }
    public NamedTypeSymbol? ReceiverResultErrorCaseType { get; }
    public MethodSymbol? ReceiverResultOkValueGetter { get; }
    public MethodSymbol? ReceiverResultErrorDataGetter { get; }

    // Optional<T>
    public NamedTypeSymbol? OptionSomeCaseType { get; }
    public NamedTypeSymbol? OptionNoneCaseType { get; }
    public MethodSymbol? OptionTryGetValueMethod { get; }
    public MethodSymbol? OptionSomeValueGetter { get; }
    public MethodSymbol? OptionSomeCtor { get; }
    public MethodSymbol? OptionNoneCtorOrFactory { get; }
    public MethodSymbol? OptionImplicitFromSome { get; }
    public MethodSymbol? OptionImplicitFromNone { get; }


    public override TypeSymbol Type => ResultType;

    public BoundCarrierConditionalAccessExpression(
        BoundExpression receiver,
        BoundCarrierKind carrierKind,
        TypeSymbol payloadType,
        LocalSymbol payloadLocal,
        BoundExpression whenPresent,
        TypeSymbol resultType,
        NamedTypeSymbol? carrierType = null,

        NamedTypeSymbol? receiverResultOkCaseType = null,
        NamedTypeSymbol? receiverResultErrorCaseType = null,
        MethodSymbol? receiverResultOkValueGetter = null,
        MethodSymbol? receiverResultErrorDataGetter = null,

        NamedTypeSymbol? resultOkCaseType = null,
        NamedTypeSymbol? resultErrorCaseType = null,
        MethodSymbol? resultTryGetValueForOkCaseMethod = null,
        MethodSymbol? resultTryGetValueForErrorCaseMethod = null,
        MethodSymbol? resultOkValueGetter = null,
        MethodSymbol? resultErrorDataGetter = null,
        MethodSymbol? resultOkCtor = null,
        MethodSymbol? resultErrorCtor = null,
        MethodSymbol? resultImplicitFromOk = null,
        MethodSymbol? resultImplicitFromError = null,

        NamedTypeSymbol? optionSomeCaseType = null,
        NamedTypeSymbol? optionNoneCaseType = null,
        MethodSymbol? optionTryGetValueMethod = null,
        MethodSymbol? optionSomeValueGetter = null,
        MethodSymbol? optionSomeCtor = null,
        MethodSymbol? optionNoneCtorOrFactory = null,
        MethodSymbol? optionImplicitFromSome = null,
        MethodSymbol? optionImplicitFromNone = null)
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
        ResultTryGetValueForOkCaseMethod = resultTryGetValueForOkCaseMethod;
        ResultTryGetValueForErrorCaseMethod = resultTryGetValueForErrorCaseMethod;
        ResultOkValueGetter = resultOkValueGetter;
        ResultErrorDataGetter = resultErrorDataGetter;
        ResultOkCtor = resultOkCtor;
        ResultErrorCtor = resultErrorCtor;
        ResultImplicitFromOk = resultImplicitFromOk;
        ResultImplicitFromError = resultImplicitFromError;

        OptionSomeCaseType = optionSomeCaseType;
        OptionNoneCaseType = optionNoneCaseType;
        OptionTryGetValueMethod = optionTryGetValueMethod;
        OptionSomeValueGetter = optionSomeValueGetter;
        OptionSomeCtor = optionSomeCtor;
        OptionNoneCtorOrFactory = optionNoneCtorOrFactory;
        OptionImplicitFromSome = optionImplicitFromSome;
        OptionImplicitFromNone = optionImplicitFromNone;
    }
}
