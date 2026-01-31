using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

partial class BlockBinder
{
    private BoundErrorExpression InvocationError(
            BoundExpression? receiver,
            string methodName,
            BoundExpressionReason reason)
    {
        ImmutableArray<IMethodSymbol> candidates = default;
        ITypeSymbol? resultType = null;
        ISymbol? symbol = null;

        if (receiver is BoundNamespaceExpression nsReceiver)
        {
            var typeInNamespace = nsReceiver.Namespace
                .GetMembers(methodName)
                .OfType<INamedTypeSymbol>()
                .FirstOrDefault();

            if (typeInNamespace is not null)
            {
                candidates = typeInNamespace.Constructors
                    .Where(static ctor => !ctor.IsStatic)
                    .ToImmutableArray();
                symbol = candidates.IsDefaultOrEmpty ? typeInNamespace : candidates[0];
                resultType = typeInNamespace;
            }
        }
        else if (receiver is BoundTypeExpression typeReceiver)
        {
            candidates = new SymbolQuery(methodName, typeReceiver.Type, IsStatic: true)
                .LookupMethods(this)
                .ToImmutableArray();

            if (!candidates.IsDefaultOrEmpty)
            {
                symbol = candidates[0];
                resultType = candidates[0].ReturnType;
            }
            else
            {
                symbol = typeReceiver.Type;
                resultType = typeReceiver.Type;
            }
        }
        else if (receiver is not null)
        {
            candidates = new SymbolQuery(methodName, receiver.Type, IsStatic: false)
                .LookupMethods(this)
                .ToImmutableArray();

            if (!candidates.IsDefaultOrEmpty)
            {
                symbol = candidates[0];
                resultType = candidates[0].ReturnType;
            }
            else
            {
                resultType = receiver.Type;
            }
        }
        else
        {
            candidates = new SymbolQuery(methodName)
                .LookupMethods(this)
                .ToImmutableArray();

            if (!candidates.IsDefaultOrEmpty)
            {
                symbol = candidates[0];
                resultType = candidates[0].ReturnType;
            }
        }

        return ErrorExpression(
            resultType ?? Compilation.ErrorTypeSymbol,
            symbol,
            reason,
            AsSymbolCandidates(candidates));
    }

    private BoundExpression BindInvocationOnMethodGroup(BoundMethodGroupExpression methodGroup, InvocationExpressionSyntax syntax)
    {
        var candidatesForArgumentBinding = FilterInvocationCandidatesForArgumentBinding(methodGroup.Methods, syntax.ArgumentList.Arguments);
        var boundArguments = BindInvocationArgumentsWithCandidateTargetTypes(candidatesForArgumentBinding, syntax.ArgumentList.Arguments, out var hasErrors);

        if (hasErrors || methodGroup.Receiver is { } receiver && IsErrorExpression(receiver))
        {
            var selectedForError = methodGroup.SelectedMethod;
            var symbol = selectedForError ?? methodGroup.Methods.FirstOrDefault();
            var returnType = symbol?.ReturnType ?? Compilation.ErrorTypeSymbol;

            return ErrorExpression(
                returnType,
                symbol,
                BoundExpressionReason.ArgumentBindingFailed,
                AsSymbolCandidates(methodGroup.Methods));
        }

        var methodName = methodGroup.Methods[0].Name;
        var selected = methodGroup.SelectedMethod;
        var extensionReceiver = IsExtensionReceiver(methodGroup.Receiver) ? methodGroup.Receiver : null;
        var receiverSyntax = GetInvocationReceiverSyntax(syntax) ?? syntax.Expression;

        if (selected is not null)
        {
            var inferred = OverloadResolver.ApplyTypeArgumentInference(selected, extensionReceiver, boundArguments, Compilation);
            if (inferred is not null)
            {
                selected = inferred;

                if (AreArgumentsCompatibleWithMethod(selected, boundArguments.Length, extensionReceiver, boundArguments))
                {
                    var converted = ConvertInvocationArguments(
                        selected,
                        boundArguments,
                        extensionReceiver,
                        receiverSyntax,
                        out var convertedExtensionReceiver);
                    return new BoundInvocationExpression(selected, converted, methodGroup.Receiver, convertedExtensionReceiver);
                }
            }
        }

        var resolution = OverloadResolver.ResolveOverload(
            methodGroup.Methods,
            boundArguments,
            Compilation,
            extensionReceiver,
            EnsureLambdaCompatible,
            callSyntax: syntax);

        if (resolution.Success)
        {
            var method = resolution.Method!;
            var convertedArgs = ConvertInvocationArguments(
                method,
                boundArguments,
                extensionReceiver,
                receiverSyntax,
                out var convertedExtensionReceiver);
            return new BoundInvocationExpression(method, convertedArgs, methodGroup.Receiver, convertedExtensionReceiver);
        }

        if (resolution.IsAmbiguous)
        {
            _diagnostics.ReportCallIsAmbiguous(methodName, resolution.AmbiguousCandidates, syntax.GetLocation());
            return ErrorExpression(
                reason: BoundExpressionReason.Ambiguous,
                candidates: AsSymbolCandidates(resolution.AmbiguousCandidates));
        }

        if (LookupType(methodName) is INamedTypeSymbol typeFallback)
        {
            // Rebind arguments against ctor parameter types so target-typed member bindings (e.g. `.Ok`, `.Human`)
            // can be resolved even before overload resolution.
            return BindConstructorInvocation(typeFallback, syntax, receiverSyntax: syntax.Expression, receiver: null);
        }

        ReportSuppressedLambdaDiagnostics(boundArguments);
        _diagnostics.ReportNoOverloadForMethod("method", methodName, boundArguments.Length, syntax.GetLocation());
        return ErrorExpression(reason: BoundExpressionReason.OverloadResolutionFailed);
    }

    private static ImmutableArray<IMethodSymbol> FilterInvocationCandidatesForArgumentBinding(
        ImmutableArray<IMethodSymbol> methods,
        SeparatedSyntaxList<ArgumentSyntax> arguments)
    {
        if (methods.IsDefaultOrEmpty)
            return methods;

        var argCount = arguments.Count;

        // Best-effort filtering: drop candidates that cannot accept the given argument count.
        // This is important for target-typed member bindings inside arguments (e.g. `.Public | .Static`)
        // when overload sets contain both parameterless and parameterful candidates.
        var builder = ImmutableArray.CreateBuilder<IMethodSymbol>(methods.Length);

        foreach (var method in methods)
        {
            if (method is null)
                continue;

            var parameters = method.Parameters;

            // For extension methods, the receiver occupies parameter 0.
            var start = method.IsExtensionMethod ? 1 : 0;
            var effectiveCount = parameters.Length - start;

            // Determine required parameter count (excluding optional parameters).
            int required = 0;
            for (int i = start; i < parameters.Length; i++)
            {
                if (!parameters[i].IsOptional)
                    required++;
            }

            var hasParamsArray = effectiveCount > 0 && parameters[^1].IsParams;

            bool accepts;
            if (hasParamsArray)
            {
                // Params: any count >= required-1 (because the params array itself can take 0+ arguments)
                // but still must satisfy required non-optional parameters.
                accepts = argCount >= required - 1;
            }
            else
            {
                accepts = argCount >= required && argCount <= effectiveCount;
            }

            if (accepts)
                builder.Add(method);
        }

        // If filtering removed everything, keep original to avoid hiding diagnostics.
        return builder.Count > 0 ? builder.ToImmutable() : methods;
    }

    private BoundArgument[] BindInvocationArgumentsWithCandidateTargetTypes(
        ImmutableArray<IMethodSymbol> methods,
        SeparatedSyntaxList<ArgumentSyntax> arguments,
        out bool hasErrors)
    {
        // Bind invocation arguments while supplying a best-effort target type.
        // This is important for target-typed member bindings inside argument position,
        // e.g. `format(.Ok(42))`, where `.Ok(42)` needs the parameter type to resolve.

        hasErrors = false;

        var boundArguments = new BoundArgument[arguments.Count];

        for (int i = 0; i < arguments.Count; i++)
        {
            var arg = arguments[i];

            // Bind invocation arguments while supplying a best-effort target type.
            // Positional args use the (common) parameter type at the given index.
            // Named args use the (common) parameter type for that name.
            ITypeSymbol? targetType = null;

            if (arg.NameColon is null)
            {
                targetType = TryGetCommonPositionalParameterType(methods, i);
            }
            else
            {
                var argName = arg.NameColon.Name.Identifier.ValueText;
                if (!string.IsNullOrEmpty(argName))
                    targetType = TryGetCommonNamedParameterType(methods, argName);
            }

            var boundExpr = targetType is null
                ? BindExpression(arg.Expression)
                : BindExpressionWithTargetType(arg.Expression, targetType);

            if (IsErrorExpression(boundExpr))
                hasErrors = true;

            var name = arg.NameColon?.Name.Identifier.ValueText;
            if (string.IsNullOrEmpty(name))
                name = null;

            boundArguments[i] = new BoundArgument(boundExpr, RefKind.None, name, arg);
        }

        return boundArguments;

        static ITypeSymbol? TryGetCommonPositionalParameterType(ImmutableArray<IMethodSymbol> methods, int argumentIndex)
        {
            if (methods.IsDefaultOrEmpty)
                return null;

            ITypeSymbol? common = null;
            bool hasCommon = false;

            foreach (var method in methods)
            {
                if (method is null)
                    continue;

                // For extension methods, the receiver occupies parameter 0.
                var parameterIndex = method.IsExtensionMethod ? argumentIndex + 1 : argumentIndex;

                if (parameterIndex < 0 || parameterIndex >= method.Parameters.Length)
                    return null;

                var type = method.Parameters[parameterIndex].Type;

                if (!hasCommon)
                {
                    common = type;
                    hasCommon = true;
                    continue;
                }

                if (!SymbolEqualityComparer.Default.Equals(common, type))
                    return null;
            }

            return hasCommon ? common : null;
        }

        static ITypeSymbol? TryGetCommonNamedParameterType(ImmutableArray<IMethodSymbol> methods, string argumentName)
        {
            if (methods.IsDefaultOrEmpty)
                return null;

            ITypeSymbol? common = null;
            bool hasCommon = false;

            foreach (var method in methods)
            {
                if (method is null)
                    continue;

                // Named arguments bind by parameter name.
                var parameter = method.Parameters.FirstOrDefault(p => p.Name == argumentName);
                if (parameter is null)
                    return null;

                var type = parameter.Type;

                if (!hasCommon)
                {
                    common = type;
                    hasCommon = true;
                    continue;
                }

                if (!SymbolEqualityComparer.Default.Equals(common, type))
                    return null;
            }

            return hasCommon ? common : null;
        }
    }

    private BoundExpression BindConstructorInvocation(
        INamedTypeSymbol typeSymbol,
        InvocationExpressionSyntax invocation,
        SyntaxNode receiverSyntax,
        BoundExpression? receiver = null)
    {
        // Bind constructor arguments while supplying best-effort target types based on ctor parameter types.
        var ctorsForArgumentBinding = FilterInvocationCandidatesForArgumentBinding(typeSymbol.Constructors, invocation.ArgumentList.Arguments);
        var boundArguments = BindInvocationArgumentsWithCandidateTargetTypes(ctorsForArgumentBinding, invocation.ArgumentList.Arguments, out var hasErrors);

        if (hasErrors)
            return new BoundErrorExpression(typeSymbol, null, BoundExpressionReason.ArgumentBindingFailed);

        return BindConstructorInvocation(typeSymbol, boundArguments, invocation, receiverSyntax, receiver);
    }

    private BoundExpression BindConstructorInvocation(
        INamedTypeSymbol typeSymbol,
        BoundArgument[] boundArguments,
        SyntaxNode callSyntax,
        SyntaxNode receiverSyntax,
        BoundExpression? receiver = null)
    {
        if (typeSymbol.TypeKind == TypeKind.Error)
            return new BoundErrorExpression(typeSymbol, null, BoundExpressionReason.OtherError);

        if (typeSymbol.IsStatic)
        {
            _diagnostics.ReportStaticTypeCannotBeInstantiated(typeSymbol.Name, callSyntax.GetLocation());
            return new BoundErrorExpression(typeSymbol, null, BoundExpressionReason.OtherError);
        }

        var resolution = OverloadResolver.ResolveOverload(typeSymbol.Constructors, boundArguments, Compilation, canBindLambda: EnsureLambdaCompatible, callSyntax: callSyntax);
        if (resolution.Success)
        {
            var constructor = resolution.Method!;
            if (!EnsureMemberAccessible(constructor, receiverSyntax.GetLocation(), "constructor"))
                return new BoundErrorExpression(typeSymbol, null, BoundExpressionReason.Inaccessible);
            var convertedArgs = ConvertArguments(constructor.Parameters, boundArguments);

            BoundObjectInitializer? initializer = null;

            if (callSyntax is InvocationExpressionSyntax { Initializer: not null } o1)
            {
                initializer = BindObjectInitializer(typeSymbol, o1.Initializer);
            }
            else if (callSyntax is ObjectCreationExpressionSyntax { Initializer: not null } o2)
            {
                initializer = BindObjectInitializer(typeSymbol, o2.Initializer);
            }

            return new BoundObjectCreationExpression(constructor, convertedArgs, receiver, initializer);
        }

        if (resolution.IsAmbiguous)
        {
            _diagnostics.ReportCallIsAmbiguous(typeSymbol.Name, resolution.AmbiguousCandidates, callSyntax.GetLocation());
            return new BoundErrorExpression(
                typeSymbol,
                null,
                BoundExpressionReason.Ambiguous,
                AsSymbolCandidates(resolution.AmbiguousCandidates));
        }

        ReportSuppressedLambdaDiagnostics(boundArguments);
        _diagnostics.ReportNoOverloadForMethod("constructor for type", typeSymbol.Name, boundArguments.Length, callSyntax.GetLocation());
        return new BoundErrorExpression(typeSymbol, null, BoundExpressionReason.OverloadResolutionFailed);
    }

    private BoundExpression BindObjectCreationExpression(ObjectCreationExpressionSyntax syntax)
    {
        INamedTypeSymbol? typeSymbol = null;

        var typeExpr = BindTypeSyntax(syntax.Type);

        if (typeExpr is BoundTypeExpression boundType)
        {
            typeSymbol = boundType.Type as INamedTypeSymbol;
        }
        else
        {
            //_diagnostics.ReportInvalidObjectCreation(syntax.Type.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.NotFound);
        }

        if (typeSymbol == null)
        {
            //_diagnostics.ReportInvalidObjectCreation(syntax.Type.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.NotFound);
        }

        if (typeSymbol == null)
        {
            _diagnostics.ReportTheNameDoesNotExistInTheCurrentContext(syntax.Type.ToString(), syntax.Type.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.NotFound);
        }

        if (typeSymbol.TypeKind == TypeKind.Error)
            return new BoundErrorExpression(typeSymbol, null, BoundExpressionReason.OtherError);

        var validatedType = EnsureTypeAccessible(typeSymbol, syntax.Type.GetLocation());
        if (validatedType.TypeKind == TypeKind.Error)
            return new BoundErrorExpression(typeSymbol, null, BoundExpressionReason.Inaccessible);

        if (typeSymbol.IsStatic)
        {
            _diagnostics.ReportStaticTypeCannotBeInstantiated(typeSymbol.Name, syntax.Type.GetLocation());
            return new BoundErrorExpression(typeSymbol, null, BoundExpressionReason.OtherError);
        }

        // Bind arguments while supplying a best-effort target type from ctor parameter types.
        var ctorsForArgumentBinding = FilterInvocationCandidatesForArgumentBinding(typeSymbol.Constructors, syntax.ArgumentList.Arguments);
        var boundArguments = BindInvocationArgumentsWithCandidateTargetTypes(ctorsForArgumentBinding, syntax.ArgumentList.Arguments, out var hasErrors);

        if (hasErrors)
            return new BoundErrorExpression(typeSymbol, null, BoundExpressionReason.ArgumentBindingFailed);

        // Overload resolution
        var resolution = OverloadResolver.ResolveOverload(typeSymbol.Constructors, boundArguments, Compilation, canBindLambda: EnsureLambdaCompatible, callSyntax: syntax);
        if (resolution.Success)
        {
            var constructor = resolution.Method!;
            constructor = EnsureConstructedConstructor(constructor, typeSymbol);
            if (!EnsureMemberAccessible(constructor, syntax.Type.GetLocation(), "constructor"))
                return new BoundErrorExpression(typeSymbol, null, BoundExpressionReason.Inaccessible);

            var convertedArgs = ConvertArguments(constructor.Parameters, boundArguments);
            return new BoundObjectCreationExpression(constructor, convertedArgs);
        }

        if (resolution.IsAmbiguous)
        {
            _diagnostics.ReportCallIsAmbiguous(typeSymbol.Name, resolution.AmbiguousCandidates, syntax.GetLocation());
            return new BoundErrorExpression(
                typeSymbol,
                null,
                BoundExpressionReason.Ambiguous,
                AsSymbolCandidates(resolution.AmbiguousCandidates));
        }

        ReportSuppressedLambdaDiagnostics(boundArguments);
        _diagnostics.ReportNoOverloadForMethod("constructor", typeSymbol.Name, boundArguments.Length, syntax.GetLocation());
        return new BoundErrorExpression(typeSymbol, null, BoundExpressionReason.OverloadResolutionFailed);
    }

    protected static IMethodSymbol EnsureConstructedConstructor(IMethodSymbol constructor, INamedTypeSymbol typeSymbol)
    {
        if (constructor is null)
            throw new ArgumentNullException(nameof(constructor));
        if (typeSymbol is null)
            throw new ArgumentNullException(nameof(typeSymbol));

        if (constructor is SubstitutedMethodSymbol || constructor is ConstructedMethodSymbol)
            return constructor;

        if (typeSymbol is not ConstructedNamedTypeSymbol constructed)
            return constructor;

        if (constructed.ConstructedFrom is not INamedTypeSymbol originalDefinition)
            return constructor;

        if (constructor.ContainingType is not INamedTypeSymbol containingDefinition)
            return constructor;

        if (!SymbolEqualityComparer.Default.Equals(containingDefinition, originalDefinition))
            return constructor;

        return new SubstitutedMethodSymbol(constructor, constructed);
    }

    // ============================
    // Conditional access helpers
    // ============================

    private static bool TryGetOptionType(ITypeSymbol? type, out INamedTypeSymbol option, out ITypeSymbol payload)
    {
        option = null!;
        payload = null!;

        if (type is null)
            return false;

        type = type.UnwrapLiteralType() ?? type;

        if (type is INamedTypeSymbol named &&
            named.Arity == 1 &&
            string.Equals(named.Name, "Option", StringComparison.Ordinal))
        {
            option = named;
            payload = named.TypeArguments[0];
            return true;
        }

        return false;
    }

    private static bool TryGetResultType(ITypeSymbol? type, out INamedTypeSymbol result, out ITypeSymbol payload, out ITypeSymbol error)
    {
        result = null!;
        payload = null!;
        error = null!;

        if (type is null)
            return false;

        type = type.UnwrapLiteralType() ?? type;

        if (type is INamedTypeSymbol named &&
            named.Arity == 2 &&
            string.Equals(named.Name, "Result", StringComparison.Ordinal))
        {
            result = named;
            payload = named.TypeArguments[0];
            error = named.TypeArguments[1];
            return true;
        }

        return false;
    }

    private ITypeSymbol? GetConditionalAccessLookupType(ITypeSymbol? type)
    {
        if (type is null)
            return null;

        type = type.UnwrapLiteralType() ?? type;

        // Carrier-aware lookup:
        // - Nullable wrapper: look up on underlying T
        // - Option<T>: look up on payload T
        // - Result<T,E>: look up on payload T
        if (TryGetOptionType(type, out _, out var optPayload))
            return optPayload.GetPlainType();

        if (TryGetResultType(type, out _, out var resPayload, out _))
            return resPayload.GetPlainType();

        // Raven nullable wrapper
        return type.GetPlainType();
    }

    // ============================
    // Conditional access binding
    // ============================

    private BoundExpression BindConditionalAccessExpression(ConditionalAccessExpressionSyntax syntax)
    {
        var receiver = BindExpressionAllowingEvent(syntax.Expression);
        if (receiver is BoundErrorExpression) return receiver;

        // classify carrier
        var receiverType = UnwrapAlias(receiver.Type ?? Compilation.ErrorTypeSymbol);

        if (TryGetOptionType(receiverType, out var opt, out var optPayload))
            return BindCarrierConditionalAccess(syntax, receiver, BoundCarrierKind.Option, opt, optPayload, errorType: null);

        if (TryGetResultType(receiverType, out var res, out var resPayload, out var resError))
            return BindCarrierConditionalAccess(syntax, receiver, BoundCarrierKind.Result, res, resPayload, errorType: resError);

        // fallback: your existing nullable conditional access path
        return BindNullableConditionalAccessExpression(syntax, receiver);
    }

    private BoundExpression BindCarrierConditionalAccess(
    ConditionalAccessExpressionSyntax syntax,
    BoundExpression carrierReceiver,
    BoundCarrierKind kind,
    INamedTypeSymbol carrierGeneric,
    ITypeSymbol payloadType,
    ITypeSymbol? errorType)
    {
        // Synthesize a payload local that exists only for this node’s WhenPresent binding
        var payloadLocal = CreateTempLocal("payload", payloadType, syntax);
        var payloadReceiver = new BoundLocalAccess(payloadLocal);

        // Bind WhenPresent using payloadReceiver *as the receiver*.
        // (This is different from your current approach, which binds against `receiver`
        // but uses receiverTypeForLookup to *pretend* it’s payload.)
        BoundExpression whenPresent = syntax.WhenNotNull switch
        {
            MemberBindingExpressionSyntax memberBinding =>
                BindMemberAccessOnReceiver(
                    payloadReceiver,
                    memberBinding.Name,
                    preferMethods: false,
                    allowEventAccess: true,
                    suppressNullWarning: true,
                    receiverTypeForLookup: payloadType,
                    forceExtensionReceiver: true),

            InvocationExpressionSyntax { Expression: MemberBindingExpressionSyntax mb } inv =>
                BindInvocationOnMethodGroup(
                    (BoundMethodGroupExpression)BindMemberAccessOnReceiver(
                        payloadReceiver, mb.Name,
                        preferMethods: true,
                        allowEventAccess: false,
                        suppressNullWarning: true,
                        receiverTypeForLookup: payloadType,
                        forceExtensionReceiver: true),
                    inv),

            ElementBindingExpressionSyntax eb =>
                BindElementAccessExpression(payloadReceiver, eb.ArgumentList, eb, suppressNullWarning: true),

            InvocationExpressionSyntax { Expression: ReceiverBindingExpressionSyntax } inv =>
                BindInvocationExpressionCore(payloadReceiver, "Invoke", inv.ArgumentList, syntax.Expression, inv, suppressNullWarning: true),

            _ => ErrorExpression(reason: BoundExpressionReason.NotFound)
        };

        whenPresent = IsErrorExpression(whenPresent) ? AsErrorExpression(whenPresent) : whenPresent;

        var accessType = whenPresent.Type ?? Compilation.ErrorTypeSymbol;
        var u = (accessType.UnwrapLiteralType() ?? accessType).GetPlainType();

        ITypeSymbol resultType =
            kind == BoundCarrierKind.Option ? carrierGeneric.Construct(u) :
            kind == BoundCarrierKind.Result ? carrierGeneric.Construct(u, errorType!) :
            throw new InvalidOperationException();

        // Resolve carrier APIs as symbols so codegen doesn't have to guess via reflection.
        INamedTypeSymbol? carrierTypeSymbol = resultType as INamedTypeSymbol;

        INamedTypeSymbol? resOk = null;
        INamedTypeSymbol? resErr = null;
        IMethodSymbol? resTryOk = null;
        IMethodSymbol? resTryErr = null;
        IMethodSymbol? resOkValue = null;
        IMethodSymbol? resErrData = null;
        IMethodSymbol? resOkCtor = null;
        IMethodSymbol? resErrCtor = null;
        IMethodSymbol? resImplOk = null;
        IMethodSymbol? resImplErr = null;

        // Receiver Result<TPayload, E> API
        INamedTypeSymbol? recvOk = null;
        INamedTypeSymbol? recvErr = null;
        IMethodSymbol? recvOkValue = null;
        IMethodSymbol? recvErrData = null;

        INamedTypeSymbol? optSome = null;
        INamedTypeSymbol? optNone = null;
        IMethodSymbol? optTrySome = null;
        IMethodSymbol? optSomeValue = null;
        IMethodSymbol? optSomeCtor = null;
        IMethodSymbol? optNoneCtor = null;
        IMethodSymbol? optImplSome = null;
        IMethodSymbol? optImplNone = null;

        if (carrierTypeSymbol is not null)
        {
            if (kind == BoundCarrierKind.Result)
            {
                // Receiver is Result<T,E>
                var receiverNamed = (INamedTypeSymbol)carrierReceiver.Type!;
                resTryOk = FindSingleOutBoolMethod(receiverNamed, "TryGetOk");
                resTryErr = FindSingleOutBoolMethod(receiverNamed, "TryGetError");

                // Receiver Result<TPayload, E> cases
                recvOk = FindNestedCase(receiverNamed, "Ok");
                recvErr = FindNestedCase(receiverNamed, "Error");
                recvOkValue = recvOk is not null ? FindPropertyGetter(recvOk, "Value") : null;
                recvErrData = recvErr is not null
                    ? (FindPropertyGetter(recvErr, "Data") ?? FindPropertyGetter(recvErr, "Value"))
                    : null;

                // Result<U,E> cases (carrier)
                resOk = FindNestedCase(carrierTypeSymbol, "Ok");
                resErr = FindNestedCase(carrierTypeSymbol, "Error");

                if (resOk is not null)
                {
                    resOkValue = FindPropertyGetter(resOk, "Value");
                    resOkCtor = FindSingleArgCtor(resOk);
                    resImplOk = FindImplicitConversion(carrierTypeSymbol, resOk);
                }

                if (resErr is not null)
                {
                    resErrData = FindPropertyGetter(resErr, "Data") ?? FindPropertyGetter(resErr, "Value");
                    resErrCtor = FindSingleArgCtor(resErr) ?? FindParameterlessCtor(resErr);
                    resImplErr = FindImplicitConversion(carrierTypeSymbol, resErr);
                }
            }
            else if (kind == BoundCarrierKind.Option)
            {
                var receiverNamed = (INamedTypeSymbol)carrierReceiver.Type!;
                optTrySome = FindSingleOutBoolMethod(receiverNamed, "TryGetSome")
                    ?? FindSingleOutBoolMethod(receiverNamed, "TryGetValue")
                    ?? FindSingleOutBoolMethod(receiverNamed, "TryGet");

                optSome = FindNestedCase(carrierTypeSymbol, "Some");
                optNone = FindNestedCase(carrierTypeSymbol, "None");

                if (optSome is not null)
                {
                    optSomeValue = FindPropertyGetter(optSome, "Value") ?? FindPropertyGetter(optSome, "Item") ?? FindPropertyGetter(optSome, "Payload");
                    optSomeCtor = FindSingleArgCtor(optSome);
                    optImplSome = FindImplicitConversion(carrierTypeSymbol, optSome);
                }

                if (optNone is not null)
                {
                    optNoneCtor = FindParameterlessCtor(optNone);
                    optImplNone = FindImplicitConversion(carrierTypeSymbol, optNone);
                }
            }
        }

        return new BoundCarrierConditionalAccessExpression(
            carrierReceiver,
            kind,
            payloadType,
            payloadLocal,
            whenPresent,
            resultType,
            carrierType: carrierTypeSymbol,

            receiverResultOkCaseType: recvOk,
            receiverResultErrorCaseType: recvErr,
            receiverResultOkValueGetter: recvOkValue,
            receiverResultErrorDataGetter: recvErrData,

            resultOkCaseType: resOk,
            resultErrorCaseType: resErr,
            resultTryGetOkMethod: resTryOk,
            resultTryGetErrorMethod: resTryErr,
            resultOkValueGetter: resOkValue,
            resultErrorDataGetter: resErrData,
            resultOkCtor: resOkCtor,
            resultErrorCtor: resErrCtor,
            resultImplicitFromOk: resImplOk,
            resultImplicitFromError: resImplErr,

            optionSomeCaseType: optSome,
            optionNoneCaseType: optNone,
            optionTryGetSomeMethod: optTrySome,
            optionSomeValueGetter: optSomeValue,
            optionSomeCtor: optSomeCtor,
            optionNoneCtorOrFactory: optNoneCtor,
            optionImplicitFromSome: optImplSome,
            optionImplicitFromNone: optImplNone);
    }

    private static INamedTypeSymbol? FindNestedCase(INamedTypeSymbol carrier, string name)
    {
        // Try exact name first, then prefix (e.g. Ok`2)
        var exact = carrier.GetTypeMembers(name).FirstOrDefault();
        if (exact is not null)
            return exact;

        return carrier.GetTypeMembers()
            .FirstOrDefault(t => t.Name.StartsWith(name, StringComparison.Ordinal));
    }

    private static IMethodSymbol? FindSingleOutBoolMethod(INamedTypeSymbol receiverType, string name)
    {
        return receiverType.GetMembers(name)
            .OfType<IMethodSymbol>()
            .FirstOrDefault(m =>
                m.Parameters.Length == 1 &&
                m.ReturnType.SpecialType == SpecialType.System_Boolean &&
                (m.Parameters[0].RefKind == RefKind.Out || m.Parameters[0].RefKind == RefKind.Ref));
    }

    private static IMethodSymbol? FindPropertyGetter(INamedTypeSymbol type, string propertyName)
    {
        return type.GetMembers(propertyName)
            .OfType<IPropertySymbol>()
            .Select(p => p.GetMethod)
            .FirstOrDefault(m => m is not null);
    }

    private static IMethodSymbol? FindSingleArgCtor(INamedTypeSymbol type)
    {
        return type.Constructors.FirstOrDefault(c => c.Parameters.Length == 1);
    }

    private static IMethodSymbol? FindParameterlessCtor(INamedTypeSymbol type)
    {
        return type.Constructors.FirstOrDefault(c => c.Parameters.Length == 0);
    }

    private static IMethodSymbol? FindImplicitConversion(INamedTypeSymbol carrier, INamedTypeSymbol from)
    {
        foreach (var m in carrier.GetMembers("op_Implicit").OfType<IMethodSymbol>())
        {
            var returnType =
                SymbolEqualityComparer.Default.Equals(m.ReturnType, carrier);

            var param = m.Parameters[0];
            var type = param.Type;

            var parameter =
                SymbolEqualityComparer.Default.Equals(type, from);

            if (
                m.IsStatic && returnType &&
                m.Parameters.Length == 1 && parameter)
            {
                return m;
            }
        }

        return null;

        /*return carrier.GetMembers("op_Implicit")
            .OfType<IMethodSymbol>()
            .FirstOrDefault(m =>
                m.IsStatic &&
                SymbolEqualityComparer.Default.Equals(m.ReturnType, carrier) &&
                m.Parameters.Length == 1 &&
                SymbolEqualityComparer.Default.Equals(m.Parameters[0].Type, from));
                */
    }

    private BoundExpression BindNullableConditionalAccessExpression(
        ConditionalAccessExpressionSyntax syntax,
        BoundExpression receiver)
    {
        if (receiver is BoundMemberAccessExpression { Member: IEventSymbol eventSymbol } eventAccess)
        {
            receiver = BindEventInvocationReceiver(eventSymbol, eventAccess, syntax.Expression);
            if (IsErrorExpression(receiver))
                return receiver is BoundErrorExpression boundError
                    ? boundError
                    : new BoundErrorExpression(receiver.Type ?? Compilation.ErrorTypeSymbol, null, BoundExpressionReason.OtherError);
        }

        var lookupType = GetConditionalAccessLookupType(receiver.Type);

        BoundExpression whenNotNull;

        switch (syntax.WhenNotNull)
        {
            case MemberBindingExpressionSyntax memberBinding:
                {
                    // Bind using the same logic as '.' (including extensions),
                    // but:
                    // - lookup against underlying type (T? -> T)
                    // - suppress null warning (because it's conditional access)
                    // - force extension lookup even if receiver is nullable
                    whenNotNull = BindMemberAccessOnReceiver(
                        receiver,
                        memberBinding.Name,
                        preferMethods: false,
                        allowEventAccess: true,
                        suppressNullWarning: true,
                        receiverTypeForLookup: lookupType,
                        forceExtensionReceiver: true);

                    if (IsErrorExpression(whenNotNull))
                        whenNotNull = AsErrorExpression(whenNotNull);

                    break;
                }

            case InvocationExpressionSyntax { Expression: MemberBindingExpressionSyntax memberBinding } invocation:
                {
                    // First bind "<receiver>.<name>" as a method group (incl. extensions),
                    // then reuse normal invocation binding.
                    var member = BindMemberAccessOnReceiver(
                        receiver,
                        memberBinding.Name,
                        preferMethods: true,
                        allowEventAccess: false,
                        suppressNullWarning: true,
                        receiverTypeForLookup: lookupType,
                        forceExtensionReceiver: true);

                    if (IsErrorExpression(member))
                    {
                        whenNotNull = AsErrorExpression(member);
                        break;
                    }

                    if (member is BoundMethodGroupExpression mg)
                    {
                        whenNotNull = BindInvocationOnMethodGroup(mg, invocation);
                        if (IsErrorExpression(whenNotNull))
                            whenNotNull = AsErrorExpression(whenNotNull);
                    }
                    else
                    {
                        _diagnostics.ReportInvalidInvocation(invocation.GetLocation());
                        whenNotNull = ErrorExpression(reason: BoundExpressionReason.NotFound);
                    }

                    break;
                }

            case InvocationExpressionSyntax { Expression: ReceiverBindingExpressionSyntax } invocation:
                {
                    var result = BindInvocationExpressionCore(receiver, "Invoke", invocation.ArgumentList, syntax.Expression, invocation, suppressNullWarning: true);
                    whenNotNull = IsErrorExpression(result) ? AsErrorExpression(result) : result;
                    break;
                }

            case ElementBindingExpressionSyntax elementBinding:
                {
                    whenNotNull = BindElementAccessExpression(receiver, elementBinding.ArgumentList, elementBinding, suppressNullWarning: true);
                    if (IsErrorExpression(whenNotNull))
                        whenNotNull = AsErrorExpression(whenNotNull);
                    break;
                }

            default:
                whenNotNull = ErrorExpression(reason: BoundExpressionReason.NotFound);
                break;
        }

        var resultType = whenNotNull.Type;
        if (!resultType.IsNullable)
            resultType = resultType.MakeNullable();

        return new BoundConditionalAccessExpression(receiver, whenNotNull, resultType);
    }

    private BoundExpression BindElementAccessExpression(ElementAccessExpressionSyntax syntax)
    {
        var receiver = BindExpression(syntax.Expression);
        return BindElementAccessExpression(receiver, syntax.ArgumentList, syntax, suppressNullWarning: false);
    }

    private BoundExpression BindElementAccessExpression(
        BoundExpression receiver,
        BracketedArgumentListSyntax argumentList,
        SyntaxNode syntax,
        bool suppressNullWarning)
    {
        if (!suppressNullWarning)
            ReportPossibleNullReferenceAccess(receiver, syntax);

        var argumentExprs = argumentList.Arguments.Select(x => BindExpression(x.Expression)).ToArray();

        if (IsErrorExpression(receiver))
            return receiver is BoundErrorExpression boundError
                ? boundError
                : new BoundErrorExpression(receiver.Type ?? Compilation.ErrorTypeSymbol, null, BoundExpressionReason.OtherError);

        var firstErrorArg = argumentExprs.FirstOrDefault(IsErrorExpression);
        if (firstErrorArg is not null)
            return firstErrorArg is BoundErrorExpression errorArg
                ? errorArg
                : new BoundErrorExpression(
                    firstErrorArg.Type ?? Compilation.ErrorTypeSymbol,
                    null,
                    BoundExpressionReason.OtherError);

        var receiverType = receiver.Type;
        if (receiverType is null)
        {
            _diagnostics.ReportInvalidInvocation(syntax.GetLocation());
            return new BoundErrorExpression(
                Compilation.GetSpecialType(SpecialType.System_Object),
                null,
                BoundExpressionReason.NotFound);
        }

        if (receiverType.TypeKind is TypeKind.Array)
        {
            return new BoundArrayAccessExpression(receiver, argumentExprs, ((IArrayTypeSymbol)receiverType).ElementType);
        }

        var indexer = ResolveIndexer(receiverType, argumentExprs.Length);

        if (indexer is null)
        {
            _diagnostics.ReportCannotApplyIndexingWithToAnExpressionOfType(
                receiverType.Name,
                syntax.GetLocation());
            return new BoundErrorExpression(receiverType, null, BoundExpressionReason.NotFound);
        }

        return new BoundIndexerAccessExpression(receiver, argumentExprs, indexer);
    }

    private IPropertySymbol? ResolveIndexer(ITypeSymbol receiverType, int argCount)
    {
        return receiverType
            .GetMembers()
            .OfType<IPropertySymbol>()
            .FirstOrDefault(p => p.IsIndexer &&
                                 p.GetMethod is not null &&
                                 p.GetMethod.Parameters.Length == argCount);
    }

    private BoundExpression BindAssignment(ExpressionSyntax leftSyntax, ExpressionSyntax rightSyntax, SyntaxNode node, SyntaxKind operatorTokenKind)
    {
        if (operatorTokenKind != SyntaxKind.EqualsToken)
            return BindCompoundAssignment(leftSyntax, rightSyntax, node, operatorTokenKind);

        if (leftSyntax is DiscardExpressionSyntax)
        {
            var right = BindExpression(rightSyntax);
            var assignmentType = right.Type ?? Compilation.ErrorTypeSymbol;
            var discardType = assignmentType.TypeKind == TypeKind.Error ? Compilation.ErrorTypeSymbol : assignmentType;
            var pattern = new BoundDiscardPattern(discardType);
            return BoundFactory.CreatePatternAssignmentExpression(assignmentType, pattern, right);
        }

        if (leftSyntax is ElementAccessExpressionSyntax elementAccess)
        {
            var receiver = BindExpression(elementAccess.Expression);
            var args = elementAccess.ArgumentList.Arguments.Select(x => BindExpression(x.Expression)).ToArray();

            if (IsErrorExpression(receiver))
                return receiver is BoundErrorExpression boundError
                    ? boundError
                    : new BoundErrorExpression(
                        receiver.Type ?? Compilation.ErrorTypeSymbol,
                        null,
                        BoundExpressionReason.OtherError);

            var firstErrorArg = args.FirstOrDefault(IsErrorExpression);
            if (firstErrorArg is not null)
                return firstErrorArg is BoundErrorExpression errorArg
                    ? errorArg
                    : new BoundErrorExpression(
                        firstErrorArg.Type ?? Compilation.ErrorTypeSymbol,
                        null,
                        BoundExpressionReason.OtherError);

            if (receiver.Type is IArrayTypeSymbol arrayType)
            {
                var arrayRightExpression = BindExpressionWithTargetType(rightSyntax, arrayType.ElementType);

                if (IsErrorExpression(arrayRightExpression))
                    return AsErrorExpression(arrayRightExpression);

                if (arrayType.ElementType.TypeKind != TypeKind.Error &&
                    ShouldAttemptConversion(arrayRightExpression))
                {
                    var right = BindLambdaToDelegateIfNeeded(arrayRightExpression, arrayType.ElementType);
                    if (!IsAssignable(arrayType.ElementType, right.Type, out var conversion))
                    {
                        _diagnostics.ReportCannotAssignFromTypeToType(
                            right.Type.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                            arrayType.ElementType.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                            rightSyntax.GetLocation());
                        return new BoundErrorExpression(arrayType.ElementType, null, BoundExpressionReason.TypeMismatch);
                    }

                    right = ApplyConversion(right, arrayType.ElementType, conversion, rightSyntax);
                    arrayRightExpression = right;
                }

                return BoundFactory.CreateArrayAssignmentExpression(
                    new BoundArrayAccessExpression(receiver, args, arrayType.ElementType),
                    arrayRightExpression);
            }

            var indexer = ResolveIndexer(receiver.Type!, args.Length);

            if (indexer is null || indexer.SetMethod is null)
            {
                _diagnostics.ReportLeftOfAssignmentMustBeAVariablePropertyOrIndexer(node.GetLocation());
                return new BoundErrorExpression(receiver.Type!, null, BoundExpressionReason.NotFound);
            }

            var indexerRightExpression = BindExpressionWithTargetType(rightSyntax, indexer.Type);

            if (IsErrorExpression(indexerRightExpression))
                return AsErrorExpression(indexerRightExpression);

            var access = new BoundIndexerAccessExpression(receiver, args, indexer);
            if (indexer.Type.TypeKind != TypeKind.Error &&
                ShouldAttemptConversion(indexerRightExpression))
            {
                var right = BindLambdaToDelegateIfNeeded(indexerRightExpression, indexer.Type);
                if (!IsAssignable(indexer.Type, right.Type, out var conversion))
                {
                    _diagnostics.ReportCannotAssignFromTypeToType(
                        right.Type.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        indexer.Type.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        rightSyntax.GetLocation());
                    return new BoundErrorExpression(indexer.Type, null, BoundExpressionReason.TypeMismatch);
                }

                right = ApplyConversion(right, indexer.Type, conversion, rightSyntax);
                indexerRightExpression = right;
            }

            return BoundFactory.CreateIndexerAssignmentExpression(access, indexerRightExpression);
        }

        // Fall back to normal variable/property assignment
        var left = BindExpressionAllowingEvent(leftSyntax);

        if (IsErrorExpression(left))
            return AsErrorExpression(left);

        if (left.Symbol is IEventSymbol eventSymbol)
        {
            _diagnostics.ReportEventCanOnlyBeUsedWithPlusOrMinus(eventSymbol.Name, leftSyntax.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.NotFound);
        }

        if (left is BoundLocalAccess localAccess)
        {
            var localSymbol = localAccess.Local;
            var localType = localSymbol.Type;

            var rightTargetType = localType is ByRefTypeSymbol byRefLocal
                ? byRefLocal.ElementType
                : localType;
            var right2 = BindExpressionWithTargetType(rightSyntax, rightTargetType);

            if (IsErrorExpression(right2))
                return AsErrorExpression(right2);

            if (localType is ByRefTypeSymbol byRefLocalType)
            {
                var converted = ConvertValueForAssignment(right2, byRefLocalType.ElementType, rightSyntax);
                if (converted is BoundErrorExpression)
                    return converted;

                return BoundFactory.CreateByRefAssignmentExpression(localAccess, byRefLocalType.ElementType, converted);
            }

            if (!localSymbol.IsMutable)
            {
                _diagnostics.ReportThisValueIsNotMutable(leftSyntax.GetLocation());
                return ErrorExpression(reason: BoundExpressionReason.NotFound);
            }

            if (right2 is BoundEmptyCollectionExpression)
            {
                return BoundFactory.CreateLocalAssignmentExpression(localSymbol, new BoundEmptyCollectionExpression(localSymbol.Type));
            }

            if (localType.TypeKind != TypeKind.Error &&
                ShouldAttemptConversion(right2))
            {
                right2 = BindLambdaToDelegateIfNeeded(right2, localType);
                if (!IsAssignable(localType, right2.Type!, out var conversion))
                {
                    _diagnostics.ReportCannotAssignFromTypeToType(
                        right2.Type!.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        localType.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        rightSyntax.GetLocation());
                    return new BoundErrorExpression(localType, null, BoundExpressionReason.TypeMismatch);
                }

                right2 = ApplyConversion(right2, localType, conversion, rightSyntax);
            }

            return BoundFactory.CreateLocalAssignmentExpression(localSymbol, right2);
        }
        else if (left is BoundParameterAccess parameterAccess)
        {
            var parameterSymbol = parameterAccess.Parameter;
            var parameterType = parameterSymbol.Type;

            if (!parameterSymbol.IsMutable)
            {
                _diagnostics.ReportThisValueIsNotMutable(leftSyntax.GetLocation());
                return ErrorExpression(reason: BoundExpressionReason.NotFound);
            }

            var rightTargetType = parameterType is ByRefTypeSymbol byRefParameter &&
                parameterSymbol.RefKind is RefKind.Ref or RefKind.Out
                ? byRefParameter.ElementType
                : parameterType;
            var right2 = BindExpressionWithTargetType(rightSyntax, rightTargetType);

            if (IsErrorExpression(right2))
                return AsErrorExpression(right2);

            if (parameterType is ByRefTypeSymbol byRefParameterType &&
                parameterSymbol.RefKind is RefKind.Ref or RefKind.Out)
            {
                var converted = ConvertValueForAssignment(right2, byRefParameterType.ElementType, rightSyntax);
                if (converted is BoundErrorExpression)
                    return converted;

                return BoundFactory.CreateByRefAssignmentExpression(parameterAccess, byRefParameterType.ElementType, converted);
            }

            if (parameterType.TypeKind != TypeKind.Error &&
                ShouldAttemptConversion(right2))
            {
                right2 = BindLambdaToDelegateIfNeeded(right2, parameterType);
                if (!IsAssignable(parameterType, right2.Type!, out var conversion))
                {
                    _diagnostics.ReportCannotAssignFromTypeToType(
                        right2.Type!.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        parameterType.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        rightSyntax.GetLocation());
                    return new BoundErrorExpression(parameterType, null, BoundExpressionReason.TypeMismatch);
                }

                right2 = ApplyConversion(right2, parameterType, conversion, rightSyntax);
            }

            return BoundFactory.CreateParameterAssignmentExpression(parameterSymbol, right2);
        }
        else if (left.Symbol is IFieldSymbol fieldSymbol)
        {
            if (fieldSymbol.IsConst)
            {
                _diagnostics.ReportThisValueIsNotMutable(leftSyntax.GetLocation());
                return new BoundErrorExpression(fieldSymbol.Type, null, BoundExpressionReason.NotFound);
            }

            var receiver = GetReceiver(left);

            var right2 = BindExpressionWithTargetType(rightSyntax, fieldSymbol.Type);

            if (IsErrorExpression(right2))
                return AsErrorExpression(right2);

            if (!CanAssignToField(fieldSymbol, receiver, leftSyntax))
                return new BoundErrorExpression(fieldSymbol.Type, fieldSymbol, BoundExpressionReason.NotFound);

            if (right2 is BoundEmptyCollectionExpression)
            {
                return CreateFieldAssignmentExpression(receiver, fieldSymbol, BoundFactory.CreateEmptyCollectionExpression(fieldSymbol.Type));
            }

            if (fieldSymbol.Type.TypeKind != TypeKind.Error &&
                ShouldAttemptConversion(right2))
            {
                right2 = BindLambdaToDelegateIfNeeded(right2, fieldSymbol.Type);
                if (!IsAssignable(fieldSymbol.Type, right2.Type!, out var conversion))
                {
                    _diagnostics.ReportCannotAssignFromTypeToType(
                        right2.Type!.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        fieldSymbol.Type.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        rightSyntax.GetLocation());
                    return new BoundErrorExpression(fieldSymbol.Type, null, BoundExpressionReason.TypeMismatch);
                }

                right2 = ApplyConversion(right2, fieldSymbol.Type, conversion, rightSyntax);
            }

            return CreateFieldAssignmentExpression(receiver, fieldSymbol, right2);
        }
        else if (left.Symbol is IPropertySymbol propertySymbol)
        {
            SourceFieldSymbol? backingField = null;

            var receiver = GetReceiver(left);

            if (IsInitOnly(propertySymbol) && !IsInInitOnlyAssignmentContext)
            {
                // IMPORTANT: reuse your *existing* “property has no setter / not writable” diagnostic path
                // (same one you use when SetMethod is null), so you don’t need a new diagnostic.

                // Example shape (adapt to your existing diag helpers):
                _diagnostics.ReportPropertyOrIndexerCannotBeAssignedIsReadOnly(propertySymbol.Name, leftSyntax.GetLocation());
                return new BoundErrorExpression(propertySymbol.Type ?? Compilation.ErrorTypeSymbol, propertySymbol, BoundExpressionReason.UnsupportedOperation);
            }

            if (propertySymbol.SetMethod is null)
            {
                if (!TryGetWritableAutoPropertyBackingField(propertySymbol, left, out backingField))
                {
                    _diagnostics.ReportPropertyOrIndexerCannotBeAssignedIsReadOnly(propertySymbol.Name, leftSyntax.GetLocation());
                    return ErrorExpression(reason: BoundExpressionReason.NotFound);
                }
            }

            var right2 = BindExpressionWithTargetType(rightSyntax, propertySymbol.Type);

            if (IsErrorExpression(right2))
                return AsErrorExpression(right2);

            if (right2 is BoundEmptyCollectionExpression)
            {
                var empty = new BoundEmptyCollectionExpression(propertySymbol.Type);

                if (backingField is not null)
                {
                    if (!CanAssignToField(backingField, receiver, leftSyntax))
                        return new BoundErrorExpression(backingField.Type, backingField, BoundExpressionReason.NotFound);

                    return CreateFieldAssignmentExpression(receiver, backingField, empty);
                }

                return BoundFactory.CreatePropertyAssignmentExpression(receiver, propertySymbol, empty);
            }

            if (propertySymbol.Type.TypeKind != TypeKind.Error &&
                ShouldAttemptConversion(right2))
            {
                right2 = BindLambdaToDelegateIfNeeded(right2, propertySymbol.Type);
                if (!IsAssignable(propertySymbol.Type, right2.Type!, out var conversion))
                {
                    _diagnostics.ReportCannotAssignFromTypeToType(
                        right2.Type!.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        propertySymbol.Type.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        rightSyntax.GetLocation());
                    return new BoundErrorExpression(propertySymbol.Type, null, BoundExpressionReason.TypeMismatch);
                }

                right2 = ApplyConversion(right2, propertySymbol.Type, conversion, rightSyntax);
            }

            if (backingField is not null)
            {
                if (!CanAssignToField(backingField, receiver, leftSyntax))
                    return new BoundErrorExpression(backingField.Type, backingField, BoundExpressionReason.NotFound);

                return CreateFieldAssignmentExpression(receiver, backingField, right2);
            }

            return BoundFactory.CreatePropertyAssignmentExpression(receiver, propertySymbol, right2);
        }

        return ErrorExpression(reason: BoundExpressionReason.NotFound);
    }

    private BoundExpression BindAssignmentExpression(AssignmentExpressionSyntax syntax)
    {
        return BindAssignment(syntax.Left, syntax.Right, syntax, syntax.OperatorToken.Kind);
    }

    private BoundExpression ConvertValueForAssignment(BoundExpression value, ITypeSymbol targetType, SyntaxNode syntax)
    {
        if (targetType.TypeKind == TypeKind.Error || value.Type is null || !ShouldAttemptConversion(value))
            return value;

        value = BindLambdaToDelegateIfNeeded(value, targetType);

        if (!IsAssignable(targetType, value.Type, out var conversion))
        {
            _diagnostics.ReportCannotAssignFromTypeToType(
                value.Type.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                targetType.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                syntax.GetLocation());
            return new BoundErrorExpression(targetType, null, BoundExpressionReason.TypeMismatch);
        }

        return ApplyConversion(value, targetType, conversion, syntax);
    }

    private BoundStatement BindAssignmentStatement(AssignmentStatementSyntax syntax)
    {
        var bound = BindAssignment(syntax.Left, syntax.Right, syntax, syntax.OperatorToken.Kind);
        if (bound is BoundAssignmentExpression assignment)
        {
            ClearNullableFlowOnAssignment(assignment);
            return new BoundAssignmentStatement(assignment);
        }

        return new BoundExpressionStatement(bound);
    }

    private BoundExpression BindAssignment(ExpressionOrPatternSyntax leftSyntax, ExpressionSyntax rightSyntax, SyntaxNode node, SyntaxKind operatorTokenKind)
    {
        if (leftSyntax is ExpressionSyntax leftExpression)
            return BindAssignment(leftExpression, rightSyntax, node, operatorTokenKind);

        var right = BindExpression(rightSyntax);
        if (leftSyntax is PatternSyntax patternSyntax)
        {
            if (operatorTokenKind != SyntaxKind.EqualsToken)
            {
                _diagnostics.ReportLeftOfAssignmentMustBeAVariablePropertyOrIndexer(node.GetLocation());
                return ErrorExpression(reason: BoundExpressionReason.UnsupportedOperation);
            }

            return BindPatternAssignment(patternSyntax, right, node);
        }

        _diagnostics.ReportLeftOfAssignmentMustBeAVariablePropertyOrIndexer(node.GetLocation());
        return ErrorExpression(right.Type, reason: BoundExpressionReason.NotFound);
    }

    // ============================
    // Member access (updated)
    // ============================

    private BoundExpression BindMemberAccessExpression(
       MemberAccessExpressionSyntax memberAccess,
       bool preferMethods = false,
       bool allowEventAccess = false)
    {
        // Binding for explicit receiver
        var receiver = BindExpression(memberAccess.Expression);

        if (IsErrorExpression(receiver))
            return receiver is BoundErrorExpression boundError
                ? boundError
                : new BoundErrorExpression(receiver.Type ?? Compilation.ErrorTypeSymbol, null, BoundExpressionReason.OtherError);

        if (receiver.Type is { } boundReceiverType && boundReceiverType.ContainsErrorType())
            return new BoundErrorExpression(boundReceiverType, receiver.Symbol, BoundExpressionReason.OtherError);

        ReportPossibleNullReferenceAccess(receiver, memberAccess.Expression);

        return BindMemberAccessOnReceiver(
            receiver,
            memberAccess.Name,
            preferMethods,
            allowEventAccess,
            suppressNullWarning: true,
            receiverTypeForLookup: null,
            forceExtensionReceiver: false);
    }

    /// <summary>
    /// Shared member-access core used by both '.' and '?.' so conditional access can see extensions.
    /// </summary>
    private BoundExpression BindMemberAccessOnReceiver(
        BoundExpression receiver,
        SimpleNameSyntax simpleName,
        bool preferMethods,
        bool allowEventAccess,
        bool suppressNullWarning,
        ITypeSymbol? receiverTypeForLookup,
        bool forceExtensionReceiver)
    {
        // NOTE: For '.' we already reported null-ref and pass suppressNullWarning=true here.
        // For '?.' we also suppress, because conditional access handles null.
        // This method should not report possible null reference itself.

        if (simpleName.Identifier.IsMissing)
        {
            _diagnostics.ReportIdentifierExpected(simpleName.Identifier.GetLocation());
            _diagnostics.ReportTheNameDoesNotExistInTheCurrentContext(string.Empty, simpleName.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.NotFound);
        }

        var name = simpleName.Identifier.ValueText;
        ImmutableArray<ITypeSymbol>? explicitTypeArguments = null;
        GenericNameSyntax? genericTypeSyntax = null;

        if (simpleName is GenericNameSyntax genericName)
        {
            var boundTypeArguments = TryBindTypeArguments(genericName);
            if (boundTypeArguments is null)
                return ErrorExpression(reason: BoundExpressionReason.TypeMismatch);

            explicitTypeArguments = boundTypeArguments;
            genericTypeSyntax = genericName;
        }

        var nameLocation = simpleName.GetLocation();
        // Deferred field to hold an inaccessible non-method member candidate.
        ISymbol? inaccessibleNonMethodMember = null;

        if (receiver is BoundNamespaceExpression nsExpr)
        {
            var member = nsExpr.Namespace.GetMembers(name).FirstOrDefault();

            if (member is INamespaceSymbol ns2)
                return new BoundNamespaceExpression(ns2);

            if (member is ITypeSymbol type)
                return new BoundTypeExpression(type);

            _diagnostics.ReportTypeOrNamespaceNameDoesNotExistInTheNamespace(name, nsExpr.Namespace.Name, nameLocation);
            return ErrorExpression(reason: BoundExpressionReason.NotFound);
        }

        if (receiver is BoundTypeExpression typeExpr)
        {
            if (preferMethods)
            {
                var methodCandidates = new SymbolQuery(name, typeExpr.Type, IsStatic: true)
                    .LookupMethods(this)
                    .ToImmutableArray();

                if (!methodCandidates.IsDefaultOrEmpty)
                {
                    if (explicitTypeArguments is { } typeArgs && genericTypeSyntax is not null)
                    {
                        var instantiated = InstantiateMethodCandidates(methodCandidates, typeArgs, genericTypeSyntax, nameLocation);
                        if (!instantiated.IsDefaultOrEmpty)
                            return BindMethodGroup(typeExpr, instantiated, nameLocation);
                    }
                    else
                    {
                        return BindMethodGroup(typeExpr, methodCandidates, nameLocation);
                    }
                }
                else
                {
                    var extensionCandidates = LookupExtensionStaticMethods(name, typeExpr.Type).ToImmutableArray();

                    if (!extensionCandidates.IsDefaultOrEmpty)
                    {
                        if (explicitTypeArguments is { } typeArgs && genericTypeSyntax is not null)
                        {
                            var instantiated = InstantiateMethodCandidates(extensionCandidates, typeArgs, genericTypeSyntax, nameLocation);
                            if (!instantiated.IsDefaultOrEmpty)
                                return BindMethodGroup(typeExpr, instantiated, nameLocation);
                        }
                        else
                        {
                            return BindMethodGroup(typeExpr, extensionCandidates, nameLocation);
                        }
                    }
                }
            }

            var nonMethodMember = new SymbolQuery(name, typeExpr.Type, IsStatic: true)
                .Lookup(this)
                .FirstOrDefault(static m => m is not IMethodSymbol);

            if (nonMethodMember is not null)
            {
                if (nonMethodMember is IEventSymbol && !allowEventAccess)
                {
                    _diagnostics.ReportEventCanOnlyBeUsedWithPlusOrMinus(nonMethodMember.Name, nameLocation);
                    return ErrorExpression(reason: BoundExpressionReason.NotFound);
                }

                if (!IsSymbolAccessible(nonMethodMember))
                {
                    inaccessibleNonMethodMember = nonMethodMember;
                }
                else
                {
                    if (nonMethodMember is ITypeSymbol typeMember)
                    {
                        if (explicitTypeArguments is { } typeArgs && genericTypeSyntax is not null && typeMember is INamedTypeSymbol namedMember)
                        {
                            if (!ValidateTypeArgumentConstraints(namedMember, typeArgs, i => GetTypeArgumentLocation(genericTypeSyntax.TypeArgumentList.Arguments, genericTypeSyntax.GetLocation(), i), namedMember.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)))
                                return ErrorExpression(reason: BoundExpressionReason.TypeMismatch);

                            if (TryConstructGeneric(namedMember, typeArgs, namedMember.Arity) is INamedTypeSymbol constructedType)
                                typeMember = constructedType;
                        }

                        return new BoundTypeExpression(typeMember);
                    }

                    return new BoundMemberAccessExpression(typeExpr, nonMethodMember);
                }
            }

            if (!preferMethods)
            {
                var methodCandidates = new SymbolQuery(name, typeExpr.Type, IsStatic: true)
                    .LookupMethods(this)
                    .ToImmutableArray();

                if (!methodCandidates.IsDefaultOrEmpty)
                {
                    if (explicitTypeArguments is { } typeArgs && genericTypeSyntax is not null)
                    {
                        var instantiated = InstantiateMethodCandidates(methodCandidates, typeArgs, genericTypeSyntax, nameLocation);
                        if (!instantiated.IsDefaultOrEmpty)
                            return BindMethodGroup(typeExpr, instantiated, nameLocation);
                    }
                    else
                    {
                        return BindMethodGroup(typeExpr, methodCandidates, nameLocation);
                    }
                }
                else
                {
                    var extensionCandidates = LookupExtensionStaticMethods(name, typeExpr.Type).ToImmutableArray();

                    if (!extensionCandidates.IsDefaultOrEmpty)
                    {
                        if (explicitTypeArguments is { } typeArgs && genericTypeSyntax is not null)
                        {
                            var instantiated = InstantiateMethodCandidates(extensionCandidates, typeArgs, genericTypeSyntax, nameLocation);
                            if (!instantiated.IsDefaultOrEmpty)
                                return BindMethodGroup(typeExpr, instantiated, nameLocation);
                        }
                        else
                        {
                            return BindMethodGroup(typeExpr, extensionCandidates, nameLocation);
                        }
                    }
                }
            }

            var member = new SymbolQuery(name, typeExpr.Type, IsStatic: true)
                .Lookup(this)
                .FirstOrDefault();

            if (member is null)
            {
                var extensionProperties = LookupExtensionStaticProperties(name, typeExpr.Type).ToImmutableArray();

                if (!extensionProperties.IsDefaultOrEmpty)
                {
                    var accessibleProperties = GetAccessibleProperties(extensionProperties, nameLocation);

                    if (!accessibleProperties.IsDefaultOrEmpty)
                    {
                        if (accessibleProperties.Length == 1)
                            return new BoundMemberAccessExpression(typeExpr, accessibleProperties[0]);

                        var ambiguousMethods = accessibleProperties
                            .Select(p => p.GetMethod ?? p.SetMethod)
                            .Where(m => m is not null)
                            .Cast<IMethodSymbol>()
                            .ToImmutableArray();

                        if (!ambiguousMethods.IsDefaultOrEmpty)
                            _diagnostics.ReportCallIsAmbiguous(name, ambiguousMethods, nameLocation);

                        return ErrorExpression(
                            reason: BoundExpressionReason.Ambiguous,
                            candidates: AsSymbolCandidates(ambiguousMethods));
                    }

                    EnsureMemberAccessible(extensionProperties[0], nameLocation, GetSymbolKindForDiagnostic(extensionProperties[0]));
                    return ErrorExpression(reason: BoundExpressionReason.Inaccessible);
                }

                if (TryBindDiscriminatedUnionCase(typeExpr.Type, name, nameLocation) is BoundExpression unionCase)
                    return unionCase;

                var typeName = typeExpr.Symbol!.Name;
                _diagnostics.ReportMemberDoesNotContainDefinition(typeName, simpleName.ToString(), nameLocation);
                return ErrorExpression(reason: BoundExpressionReason.NotFound);
            }

            if (!EnsureMemberAccessible(member, nameLocation, GetSymbolKindForDiagnostic(member)))
                return ErrorExpression(reason: BoundExpressionReason.Inaccessible);

            if (member is ITypeSymbol typeMemberSymbol)
            {
                if (BindDiscriminatedUnionCaseType(typeMemberSymbol) is { } unionCase)
                    return unionCase;

                return new BoundTypeExpression(typeMemberSymbol);
            }

            if (member is IEventSymbol && !allowEventAccess)
            {
                _diagnostics.ReportEventCanOnlyBeUsedWithPlusOrMinus(member.Name, nameLocation);
                return ErrorExpression(reason: BoundExpressionReason.NotFound);
            }

            return new BoundMemberAccessExpression(typeExpr, member);
        }

        if (receiver.Type is not null)
        {
            var receiverType = receiverTypeForLookup ?? (receiver.Type.UnwrapLiteralType() ?? receiver.Type);

            if (!suppressNullWarning)
                ReportPossibleNullReferenceAccess(receiver, simpleName);

            var allowExtensions = forceExtensionReceiver || IsExtensionReceiver(receiver);

            if (preferMethods)
            {
                var methodCandidates = ImmutableArray<IMethodSymbol>.Empty;

                var instanceMethods = new SymbolQuery(name, receiverType, IsStatic: false)
                    .LookupMethods(this)
                    .ToImmutableArray();

                if (!instanceMethods.IsDefaultOrEmpty)
                    methodCandidates = instanceMethods;

                if (allowExtensions)
                {
                    var extensionMethods = LookupExtensionMethods(name, receiverType)
                        .ToImmutableArray();

                    if (!extensionMethods.IsDefaultOrEmpty)
                    {
                        methodCandidates = methodCandidates.IsDefaultOrEmpty
                            ? extensionMethods
                            : methodCandidates.AddRange(extensionMethods);
                    }
                }

                if (!methodCandidates.IsDefaultOrEmpty)
                {
                    if (explicitTypeArguments is { } typeArgs && genericTypeSyntax is not null)
                    {
                        var instantiated = InstantiateMethodCandidates(methodCandidates, typeArgs, genericTypeSyntax, nameLocation);
                        if (!instantiated.IsDefaultOrEmpty)
                            return BindMethodGroup(receiver, instantiated, nameLocation);
                    }
                    else
                    {
                        return BindMethodGroup(receiver, methodCandidates, nameLocation);
                    }
                }
            }

            var nonMethodMember = new SymbolQuery(name, receiverType, IsStatic: false)
                .Lookup(this)
                .FirstOrDefault(static m => m is not IMethodSymbol);

            if (nonMethodMember is not null)
            {
                if (nonMethodMember is IEventSymbol && !allowEventAccess)
                {
                    _diagnostics.ReportEventCanOnlyBeUsedWithPlusOrMinus(nonMethodMember.Name, nameLocation);
                    return ErrorExpression(reason: BoundExpressionReason.NotFound);
                }

                // DO NOT report accessibility yet — may still bind to methods/extensions
                if (IsSymbolAccessible(nonMethodMember))
                    return new BoundMemberAccessExpression(receiver, nonMethodMember);
                inaccessibleNonMethodMember = nonMethodMember;
            }

            if (!preferMethods)
            {
                var methodCandidates = ImmutableArray<IMethodSymbol>.Empty;

                var instanceMethods = new SymbolQuery(name, receiverType, IsStatic: false)
                    .LookupMethods(this)
                    .ToImmutableArray();

                if (!instanceMethods.IsDefaultOrEmpty)
                    methodCandidates = instanceMethods;

                if (allowExtensions)
                {
                    var extensionMethods = LookupExtensionMethods(name, receiverType)
                        .ToImmutableArray();

                    if (!extensionMethods.IsDefaultOrEmpty)
                    {
                        methodCandidates = methodCandidates.IsDefaultOrEmpty
                            ? extensionMethods
                            : methodCandidates.AddRange(extensionMethods);
                    }
                }

                if (!methodCandidates.IsDefaultOrEmpty)
                {
                    if (explicitTypeArguments is { } typeArgs && genericTypeSyntax is not null)
                    {
                        var instantiated = InstantiateMethodCandidates(methodCandidates, typeArgs, genericTypeSyntax, nameLocation);
                        if (!instantiated.IsDefaultOrEmpty)
                            return BindMethodGroup(receiver, instantiated, nameLocation);
                    }
                    else
                    {
                        return BindMethodGroup(receiver, methodCandidates, nameLocation);
                    }
                }
            }

            var instanceMember = new SymbolQuery(name, receiverType, IsStatic: false)
                .Lookup(this)
                .FirstOrDefault();

            if (instanceMember is not null)
            {
                if (!EnsureMemberAccessible(instanceMember, nameLocation, GetSymbolKindForDiagnostic(instanceMember)))
                    return ErrorExpression(reason: BoundExpressionReason.Inaccessible);

                if (instanceMember is IEventSymbol && !allowEventAccess)
                {
                    _diagnostics.ReportEventCanOnlyBeUsedWithPlusOrMinus(instanceMember.Name, nameLocation);
                    return ErrorExpression(reason: BoundExpressionReason.NotFound);
                }

                return new BoundMemberAccessExpression(receiver, instanceMember);
            }

            if (allowExtensions)
            {
                var extensionProperties = LookupExtensionProperties(name, receiverType).ToImmutableArray();

                if (!extensionProperties.IsDefaultOrEmpty)
                {
                    var accessibleProperties = GetAccessibleProperties(extensionProperties, nameLocation);

                    if (!accessibleProperties.IsDefaultOrEmpty)
                    {
                        if (accessibleProperties.Length == 1)
                            return new BoundMemberAccessExpression(receiver, accessibleProperties[0]);

                        var ambiguousMethods = accessibleProperties
                            .Select(p => p.GetMethod ?? p.SetMethod)
                            .Where(m => m is not null)
                            .Cast<IMethodSymbol>()
                            .ToImmutableArray();

                        if (!ambiguousMethods.IsDefaultOrEmpty)
                            _diagnostics.ReportCallIsAmbiguous(name, ambiguousMethods, nameLocation);

                        return ErrorExpression(
                            reason: BoundExpressionReason.Ambiguous,
                            candidates: AsSymbolCandidates(ambiguousMethods));
                    }

                    EnsureMemberAccessible(extensionProperties[0], nameLocation, GetSymbolKindForDiagnostic(extensionProperties[0]));
                    return ErrorExpression(reason: BoundExpressionReason.Inaccessible);
                }
            }
        }

        // Fallback: if we saw a non-method member but it was inaccessible, report that now.
        if (inaccessibleNonMethodMember is not null)
        {
            EnsureMemberAccessible(inaccessibleNonMethodMember, nameLocation, GetSymbolKindForDiagnostic(inaccessibleNonMethodMember));
            return ErrorExpression(reason: BoundExpressionReason.Inaccessible);
        }

        _diagnostics.ReportTheNameDoesNotExistInTheCurrentContext(name, nameLocation);
        return ErrorExpression(reason: BoundExpressionReason.NotFound);
    }

    // --- your BindMemberBindingExpression and below remains unchanged ---
    // (keeping your original implementation)

    private BoundExpression BindMemberBindingExpression(
        MemberBindingExpressionSyntax memberBinding,
        bool allowEventAccess = false)
    {
        // Member bindings like `.Human` / `.Male` are target-typed. They can only be resolved when
        // a target type is available (e.g. argument position, assignment, return, etc.).
        var expectedType = GetTargetType(memberBinding);

        if (expectedType is not null && expectedType.TypeKind != TypeKind.Error)
        {
            return BindTargetTypedMemberAccess(memberBinding.Name, expectedType, allowEventAccess);
        }

        // RAV2010
        var memberName = memberBinding.Name.Identifier.ValueText;
        _diagnostics.ReportMemberAccessRequiresTargetType(memberName, memberBinding.GetLocation());
        return ErrorExpression(reason: BoundExpressionReason.MissingType);
    }

    private BoundExpression BindMemberBindingExpression(
        MemberBindingExpressionSyntax memberBinding,
        ITypeSymbol expectedType,
        bool allowEventAccess = false)
    {
        return BindTargetTypedMemberAccess(memberBinding.Name, expectedType, allowEventAccess);
    }

    private BoundExpression BindTargetTypedMemberAccess(
        SimpleNameSyntax simpleName,
        ITypeSymbol expectedType,
        bool allowEventAccess = false)
    {
        // Target-typed member bindings should operate on the *value* target type.
        // In async contexts the expected type may be Task<T>/ValueTask<T>; unwrap so `.None` etc.
        // bind against `T` rather than the task-like wrapper.
        expectedType = UnwrapTaskLikeTargetType(expectedType);

        var memberName = simpleName.Identifier.ValueText;
        ImmutableArray<ITypeSymbol>? explicitTypeArguments = null;
        GenericNameSyntax? genericTypeSyntax = null;

        if (simpleName is GenericNameSyntax genericName)
        {
            var typeArgs = TryBindTypeArguments(genericName);
            if (typeArgs is null)
                return ErrorExpression(reason: BoundExpressionReason.TypeMismatch);

            explicitTypeArguments = typeArgs;
            genericTypeSyntax = genericName;
        }

        var nameLocation = simpleName.GetLocation();

        var methodCandidates = new SymbolQuery(memberName, expectedType, IsStatic: true)
            .LookupMethods(this)
            .ToImmutableArray();

        if (!methodCandidates.IsDefaultOrEmpty)
        {
            if (explicitTypeArguments is { } typeArgs && genericTypeSyntax is not null)
            {
                var instantiated = InstantiateMethodCandidates(methodCandidates, typeArgs, genericTypeSyntax, simpleName.GetLocation());
                if (!instantiated.IsDefaultOrEmpty)
                    return BindMethodGroup(new BoundTypeExpression(expectedType), instantiated, nameLocation);
            }
            else
            {
                return BindMethodGroup(new BoundTypeExpression(expectedType), methodCandidates, nameLocation);
            }
        }

        var member = new SymbolQuery(memberName, expectedType, IsStatic: true)
            .Lookup(this)
            .FirstOrDefault();

        if (member is null)
        {
            var extensionCandidates = LookupExtensionStaticMethods(memberName, expectedType).ToImmutableArray();

            if (!extensionCandidates.IsDefaultOrEmpty)
            {
                if (explicitTypeArguments is { } typeArgs && genericTypeSyntax is not null)
                {
                    var instantiated = InstantiateMethodCandidates(extensionCandidates, typeArgs, genericTypeSyntax, simpleName.GetLocation());
                    if (!instantiated.IsDefaultOrEmpty)
                        return BindMethodGroup(new BoundTypeExpression(expectedType), instantiated, nameLocation);
                }
                else
                {
                    return BindMethodGroup(new BoundTypeExpression(expectedType), extensionCandidates, nameLocation);
                }
            }

            var extensionProperties = LookupExtensionStaticProperties(memberName, expectedType).ToImmutableArray();

            if (!extensionProperties.IsDefaultOrEmpty)
            {
                var accessibleProperties = GetAccessibleProperties(extensionProperties, nameLocation);

                if (!accessibleProperties.IsDefaultOrEmpty)
                {
                    if (accessibleProperties.Length == 1)
                        return new BoundMemberAccessExpression(new BoundTypeExpression(expectedType), accessibleProperties[0]);

                    var ambiguousMethods = accessibleProperties
                        .Select(p => p.GetMethod ?? p.SetMethod)
                        .Where(m => m is not null)
                        .Cast<IMethodSymbol>()
                        .ToImmutableArray();

                    if (!ambiguousMethods.IsDefaultOrEmpty)
                        _diagnostics.ReportCallIsAmbiguous(memberName, ambiguousMethods, nameLocation);

                    return ErrorExpression(
                        reason: BoundExpressionReason.Ambiguous,
                        candidates: AsSymbolCandidates(ambiguousMethods));
                }

                EnsureMemberAccessible(extensionProperties[0], nameLocation, GetSymbolKindForDiagnostic(extensionProperties[0]));
                return ErrorExpression(reason: BoundExpressionReason.Inaccessible);
            }

            if (TryBindDiscriminatedUnionCase(expectedType, memberName, nameLocation) is BoundExpression unionCase)
                return unionCase;

            _diagnostics.ReportTheNameDoesNotExistInTheCurrentContext(memberName, nameLocation);
            return ErrorExpression(reason: BoundExpressionReason.NotFound);
        }

        if (!EnsureMemberAccessible(member, nameLocation, GetSymbolKindForDiagnostic(member)))
            return ErrorExpression(reason: BoundExpressionReason.Inaccessible);

        if (member is ITypeSymbol typeMember)
        {
            if (BindDiscriminatedUnionCaseType(typeMember) is { } unionCase)
            {
                // Unit-case sugar: `.Case` => `.Case(())` for single-Unit payload cases,
                // but ONLY when `.Case` is used as a standalone expression.
                var isInvocationCallee =
                    simpleName.Parent is MemberBindingExpressionSyntax mb &&
                    mb.Parent is InvocationExpressionSyntax inv &&
                    ReferenceEquals(inv.Expression, mb);

                if (!isInvocationCallee &&
                    unionCase is BoundTypeExpression { Type: INamedTypeSymbol caseType } &&
                    caseType.TryGetDiscriminatedUnionCase() is not null)
                {
                    var unitArgCtor = caseType.Constructors.FirstOrDefault(ctor =>
                        ctor.Parameters.Length == 1 &&
                        IsUnitType(ctor.Parameters[0].Type));

                    if (unitArgCtor is not null)
                    {
                        if (!EnsureMemberAccessible(unitArgCtor, nameLocation, "constructor"))
                            return ErrorExpression(reason: BoundExpressionReason.Inaccessible);

                        var unitType = unitArgCtor.Parameters[0].Type;
                        var unitValue = new BoundUnitExpression(unitType);
                        return new BoundObjectCreationExpression(
                            unitArgCtor,
                            ImmutableArray.Create<BoundExpression>(unitValue));
                    }
                }

                return unionCase;
            }

            return new BoundTypeExpression(typeMember);
        }

        if (member is IEventSymbol && !allowEventAccess)
        {
            _diagnostics.ReportEventCanOnlyBeUsedWithPlusOrMinus(member.Name, nameLocation);
            return ErrorExpression(reason: BoundExpressionReason.NotFound);
        }

        return new BoundMemberAccessExpression(new BoundTypeExpression(expectedType), member);
    }

    private BoundExpression? TryBindDiscriminatedUnionCase(ITypeSymbol? receiverType, string memberName, Location location)
    {
        var targetType = receiverType?.UnwrapLiteralType() ?? receiverType;
        if (targetType is not INamedTypeSymbol namedType)
            return null;

        if (namedType.TryGetDiscriminatedUnion() is null)
            return null;

        foreach (var member in namedType.GetMembers(memberName))
        {
            if (member is not ITypeSymbol typeMember)
                continue;

            if (typeMember.TryGetDiscriminatedUnionCase() is null)
                continue;

            var accessibleType = EnsureTypeAccessible(typeMember, location);
            if (accessibleType.TypeKind == TypeKind.Error)
                return ErrorExpression(reason: BoundExpressionReason.Inaccessible);

            return BindDiscriminatedUnionCaseType(accessibleType);
        }

        return null;
    }

    private ITypeSymbol UnwrapTaskLikeTargetType(ITypeSymbol type)
    {
        if (type is null)
            return Compilation.ErrorTypeSymbol;

        // Normalize literals first.
        type = type.UnwrapLiteralType() ?? type;

        if (type is INamedTypeSymbol named && !named.TypeArguments.IsDefaultOrEmpty)
        {
            // Unwrap common task-like wrappers so target-typed members bind to the underlying value.
            // This is important for constructs like `return .None` in an `async` method returning `Task<Option<T>>`.
            var ns = named.ContainingNamespace?.ToDisplayString(SymbolDisplayFormat.RavenErrorMessageFormat
                .WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces));
            if (ns == "System.Threading.Tasks" && (named.Name == "Task" || named.Name == "ValueTask"))
                return named.TypeArguments[0];
        }

        return type;
    }

    private BoundExpression? BindDiscriminatedUnionCaseType(ITypeSymbol typeMember)
    {
        var isUnionCase = typeMember.TryGetDiscriminatedUnionCase() is not null;

        if (!isUnionCase &&
            typeMember is INamedTypeSymbol namedType &&
            namedType.ContainingType?.TryGetDiscriminatedUnion() is not null)
        {
            isUnionCase = true;
        }

        if (!isUnionCase && typeMember.DeclaringSyntaxReferences.Any(static r => r.GetSyntax() is Raven.CodeAnalysis.Syntax.UnionCaseClauseSyntax))
        {
            isUnionCase = true;
        }

        if (!isUnionCase)
            return null;

        if (typeMember is INamedTypeSymbol caseType)
        {
            var parameterlessCtor = caseType.Constructors.FirstOrDefault(static ctor => ctor.Parameters.Length == 0);

            if (parameterlessCtor is not null)
                return new BoundObjectCreationExpression(parameterlessCtor, ImmutableArray<BoundExpression>.Empty);
        }

        return new BoundTypeExpression(typeMember);
    }
    private static bool IsUnitType(ITypeSymbol type)
    {
        type = type.UnwrapLiteralType() ?? type;

        if (type is INamedTypeSymbol named)
        {
            if (!string.Equals(named.Name, "Unit", StringComparison.Ordinal))
                return false;

            // In Raven.Core this is typically `System.Unit`.
            var ns = named.ContainingNamespace?.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
            return ns is not null && ns.EndsWith("System", StringComparison.Ordinal);
        }

        return false;
    }
}
