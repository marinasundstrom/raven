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
        var candidatesForArgumentBinding = FilterInvocationCandidatesForArgumentBinding(methodGroup.Methods, syntax.ArgumentList.Arguments, methodGroup.Receiver);
        var boundArguments = BindInvocationArgumentsWithCandidateTargetTypes(candidatesForArgumentBinding, syntax.ArgumentList.Arguments, out var hasErrors, methodGroup.Receiver);

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

        // Extract explicit method type arguments at the call site (if any), e.g. `items.CountItems<double>(2)`.
        // We bind these here (binder context) so the selected fast-path can still apply partial generic arguments.
        ImmutableArray<ITypeSymbol> explicitTypeArguments = ImmutableArray<ITypeSymbol>.Empty;

        if (syntax.Expression is MemberAccessExpressionSyntax { Name: GenericNameSyntax g })
        {
            var builder = ImmutableArray.CreateBuilder<ITypeSymbol>(g.TypeArgumentList.Arguments.Count);
            foreach (var ta in g.TypeArgumentList.Arguments)
            {
                var boundType = BindTypeSyntaxAsExpression(ta.Type);
                builder.Add(boundType.Type ?? Compilation.ErrorTypeSymbol);
            }
            explicitTypeArguments = builder.ToImmutable();
        }
        else if (syntax.Expression is GenericNameSyntax g2)
        {
            var builder = ImmutableArray.CreateBuilder<ITypeSymbol>(g2.TypeArgumentList.Arguments.Count);
            foreach (var ta in g2.TypeArgumentList.Arguments)
            {
                var boundType = BindTypeSyntaxAsExpression(ta.Type);
                builder.Add(boundType.Type ?? Compilation.ErrorTypeSymbol);
            }
            explicitTypeArguments = builder.ToImmutable();
        }

        if (selected is not null)
        {
            var inferred = OverloadResolver.ApplyTypeArgumentInference(selected, extensionReceiver, boundArguments, Compilation, explicitTypeArguments);
            if (inferred is not null)
            {
                // If we still have unbound type parameters, skip the fast-path and fall back to full overload resolution.
                if (selected.IsGenericMethod && selected.TypeParameters.Length > 0)
                {
                    // Continue into normal overload resolution below.
                }
                else if (AreArgumentsCompatibleWithMethod(selected, boundArguments.Length, extensionReceiver, boundArguments))
                {
                    var converted = ConvertInvocationArguments(
                        selected,
                        boundArguments,
                        extensionReceiver,
                        receiverSyntax,
                        out var convertedExtensionReceiver);
                    ReportObsoleteIfNeeded(selected, syntax.Expression.GetLocation());
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
            callSyntax: syntax,
            explicitTypeArguments: explicitTypeArguments);

        if (resolution.Success)
        {
            var method = resolution.Method!;
            var convertedArgs = ConvertInvocationArguments(
                method,
                boundArguments,
                extensionReceiver,
                receiverSyntax,
                out var convertedExtensionReceiver);
            ReportObsoleteIfNeeded(method, syntax.Expression.GetLocation());
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

    private ImmutableArray<IMethodSymbol> FilterInvocationCandidatesForArgumentBinding(
        ImmutableArray<IMethodSymbol> methods,
        SeparatedSyntaxList<ArgumentSyntax> arguments,
        BoundExpression? receiver = null)
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

            if (method.IsExtensionMethod &&
                receiver is not null &&
                method.Parameters.Length > 0)
            {
                var extensionReceiverType = method.Parameters[0].Type;
                var conversion = Compilation.ClassifyConversion(receiver.Type, extensionReceiverType);
                if (!conversion.Exists || !conversion.IsImplicit)
                    continue;
            }

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
        out bool hasErrors,
        BoundExpression? receiver = null,
        ITypeSymbol? pipeReceiverType = null)
    {
        // Bind invocation arguments while supplying a best-effort target type.
        // This is important for target-typed member bindings inside argument position,
        // e.g. `format(.Ok(42))`, where `.Ok(42)` needs the parameter type to resolve.
        //
        // pipeReceiverType: when non-null, the method candidates are being called via the pipe
        // operator and `pipeReceiverType` is the type of the left-hand side of the pipe.  For
        // non-extension pipe methods parameter[0] is the implicit pipe source, so each explicit
        // argument at index i maps to parameter[i+1].
        //
        // Generic type inference (C#-style phase 1 + phase 2):
        // Before binding any argument with a target type we run a pre-inference pass that binds
        // every non-lambda argument naturally (no target type). The resulting types are unified
        // against the matching parameter types to build a preliminary {T -> int, U -> string, ...}
        // substitution map. Those substitutions are then applied to every target type before use,
        // so that lambdas see concrete types (int -> bool) instead of open generics (T -> bool).

        hasErrors = false;

        // Pre-inference pass: infer type parameters from non-lambda arguments.
        var preInferredSubstitutions = TryPreInferTypeArguments(methods, arguments, receiver, pipeReceiverType);

        // Collect all method type parameters so we can detect unresolved ones.
        var allMethodTypeParams = CollectDistinctMethodTypeParameters(methods);

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
                targetType = TryGetCommonPositionalParameterType(methods, i, receiver);
            }
            else
            {
                var argName = arg.NameColon.Name.Identifier.ValueText;
                if (!string.IsNullOrEmpty(argName))
                    targetType = TryGetCommonNamedParameterType(methods, argName, receiver);
            }

            // For lambda arguments where candidates disagree on the delegate type
            // (TryGetCommonPositionalParameterType returns null), fall back to the first
            // candidate that provides a delegate type for this argument position.  This
            // handles cases like [1,2,3].ToDictionary(x => x, y => y) where overloads agree
            // on input parameter types but differ on return-type type parameters
            // (e.g. Func<int,TKey1> vs Func<int,TKey2>).
            if (targetType is null && arg.Expression is LambdaExpressionSyntax)
                targetType = TryGetFirstDelegateParameterType(methods, i, receiver, pipeReceiverType);

            // Apply pre-inferred type-parameter substitutions to the target type, then
            // discard the target type if it still contains unresolved type parameters —
            // passing an open generic as a hint causes wrong inference.
            if (targetType is not null && preInferredSubstitutions.Count > 0)
                targetType = SubstituteTypeParameters(targetType, preInferredSubstitutions);

            // For lambda arguments, only null out the target type when the DELEGATE INPUT
            // PARAMETER TYPES still contain unresolved type parameters.  The return-type
            // position can be left open — the lambda binder already handles that correctly by
            // inferring the return type from the body (see BlockBinder.Lambda.cs line ~541).
            if (targetType is not null)
            {
                var hasUnresolved = arg.Expression is LambdaExpressionSyntax
                    ? ContainsAnyTypeParameterInDelegateInputParams(targetType, allMethodTypeParams)
                    : ContainsAnyTypeParameter(targetType, allMethodTypeParams);
                if (hasUnresolved)
                    targetType = null;
            }

            var boundExpr = targetType is null
                ? BindExpression(arg.Expression)
                : BindExpressionWithTargetType(arg.Expression, targetType);

            if (IsErrorExpression(boundExpr) && targetType is not null)
            {
                // Target-typed binding can fail for otherwise-valid expressions (e.g., nested union
                // case construction). Retry without the target type before treating it as an error.
                RemoveCachedBoundNode(arg.Expression);
                boundExpr = BindExpression(arg.Expression);
            }

            if (IsErrorExpression(boundExpr))
                hasErrors = true;

            var name = arg.NameColon?.Name.Identifier.ValueText;
            if (string.IsNullOrEmpty(name))
                name = null;

            boundArguments[i] = new BoundArgument(boundExpr, RefKind.None, name, arg);
        }

        return boundArguments;

        static ImmutableArray<ITypeParameterSymbol> CollectDistinctMethodTypeParameters(ImmutableArray<IMethodSymbol> candidates)
        {
            if (candidates.IsDefaultOrEmpty)
                return ImmutableArray<ITypeParameterSymbol>.Empty;

            var builder = ImmutableArray.CreateBuilder<ITypeParameterSymbol>();
            foreach (var method in candidates)
            {
                if (method is null)
                    continue;

                var typeParameters = method.TypeParameters;
                if (typeParameters.IsDefaultOrEmpty)
                    continue;

                foreach (var typeParameter in typeParameters)
                {
                    var exists = builder.Any(existing => SymbolEqualityComparer.Default.Equals(existing, typeParameter));
                    if (!exists)
                        builder.Add(typeParameter);
                }
            }

            return builder.ToImmutable();
        }

        ITypeSymbol? TryGetCommonPositionalParameterType(ImmutableArray<IMethodSymbol> methods, int argumentIndex, BoundExpression? invocationReceiver)
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
                // For non-extension pipe methods (pipeReceiverType != null), the pipe source
                // also occupies parameter 0, so explicit argument i maps to parameter i+1.
                var parameterIndex = (method.IsExtensionMethod || pipeReceiverType is not null)
                    ? argumentIndex + 1
                    : argumentIndex;

                if (parameterIndex < 0 || parameterIndex >= method.Parameters.Length)
                    return null;

                var type = GetInvocationParameterTypeForArgumentBinding(method, parameterIndex, invocationReceiver, pipeReceiverType);

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

        ITypeSymbol? TryGetCommonNamedParameterType(ImmutableArray<IMethodSymbol> methods, string argumentName, BoundExpression? invocationReceiver)
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
                // Use OrdinalIgnoreCase: Raven convention declares record parameters as PascalCase
                // (e.g. Code: ErrorCode) but call sites use camelCase labels (e.g. code:).
                var parameter = method.Parameters.FirstOrDefault(p => string.Equals(p.Name, argumentName, StringComparison.OrdinalIgnoreCase));
                if (parameter is null)
                    return null;

                var parameterIndex = method.Parameters.IndexOf(parameter);
                if (parameterIndex < 0)
                    return null;

                var type = GetInvocationParameterTypeForArgumentBinding(method, parameterIndex, invocationReceiver);

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

    private ITypeSymbol GetInvocationParameterTypeForArgumentBinding(
        IMethodSymbol method,
        int parameterIndex,
        BoundExpression? receiver,
        ITypeSymbol? pipeReceiverType = null)
    {
        var parameterType = method.Parameters[parameterIndex].Type;

        if (method.IsExtensionMethod)
        {
            if (receiver is null || method.Parameters.IsDefaultOrEmpty)
                return parameterType;

            var extensionReceiverType = method.GetExtensionReceiverType() ?? method.Parameters[0].Type;
            if (extensionReceiverType is null || extensionReceiverType.TypeKind == TypeKind.Error)
                return parameterType;

            var substitutions = new Dictionary<ITypeParameterSymbol, ITypeSymbol>(SymbolEqualityComparer.Default);
            if (!TryUnifyExtensionReceiverType(extensionReceiverType, receiver.Type, substitutions))
                return parameterType;

            return SubstituteTypeParameters(parameterType, substitutions);
        }

        // For non-extension pipe methods with unbound type parameters, infer the substitution
        // by unifying parameter[0] (the implicit pipe source parameter) with the actual pipe
        // receiver type — mirroring the extension-method path above.
        if (pipeReceiverType is not null && !method.TypeParameters.IsDefaultOrEmpty && !method.Parameters.IsDefaultOrEmpty)
        {
            var firstParamType = method.Parameters[0].Type;
            var substitutions = new Dictionary<ITypeParameterSymbol, ITypeSymbol>(SymbolEqualityComparer.Default);
            if (TryUnifyExtensionReceiverType(firstParamType, pipeReceiverType, substitutions))
                return SubstituteTypeParameters(parameterType, substitutions);
        }

        return parameterType;
    }

    private static bool TryUnifyExtensionReceiverType(
        ITypeSymbol parameterType,
        ITypeSymbol argumentType,
        Dictionary<ITypeParameterSymbol, ITypeSymbol> substitutions)
    {
        parameterType = NormalizeTypeForExtensionInference(parameterType);
        argumentType = NormalizeTypeForExtensionInference(argumentType);

        if (parameterType is ITypeParameterSymbol typeParameter)
            return TryRecordExtensionSubstitution(typeParameter, argumentType, substitutions);

        if (parameterType is INamedTypeSymbol paramNamed)
        {
            if (argumentType is INamedTypeSymbol argNamed)
            {
                if (TryUnifyNamedType(paramNamed, argNamed, substitutions))
                    return true;

                foreach (var iface in argNamed.AllInterfaces)
                {
                    if (TryUnifyNamedType(paramNamed, iface, substitutions))
                        return true;
                }

                for (var baseType = argNamed.BaseType; baseType is not null; baseType = baseType.BaseType)
                {
                    if (TryUnifyNamedType(paramNamed, baseType, substitutions))
                        return true;
                }

                return false;
            }

            if (argumentType is IArrayTypeSymbol arrayArgument)
            {
                if (TryUnifyArrayLike(paramNamed, arrayArgument, substitutions))
                    return true;

                foreach (var iface in arrayArgument.AllInterfaces)
                {
                    if (TryUnifyNamedType(paramNamed, iface, substitutions))
                        return true;
                }

                return false;
            }

            if (Conversion.IsNullable(argumentType))
            {
                var plainArgument = argumentType.GetPlainType();
                if (plainArgument is INamedTypeSymbol plainNamed && TryUnifyNamedType(paramNamed, plainNamed, substitutions))
                    return true;

                foreach (var iface in plainArgument.AllInterfaces)
                {
                    if (TryUnifyNamedType(paramNamed, iface, substitutions))
                        return true;
                }

                return false;
            }
        }

        if (parameterType is IArrayTypeSymbol paramArray && argumentType is IArrayTypeSymbol argArray)
            return TryUnifyExtensionReceiverType(paramArray.ElementType, argArray.ElementType, substitutions);

        if (Conversion.IsNullable(parameterType))
        {
            var plainParameter = parameterType.GetPlainType();

            if (Conversion.IsNullable(argumentType))
                return TryUnifyExtensionReceiverType(plainParameter, argumentType.GetPlainType(), substitutions);

            if (!argumentType.IsValueType)
                return TryUnifyExtensionReceiverType(plainParameter, argumentType, substitutions);

            return false;
        }

        return SymbolEqualityComparer.Default.Equals(parameterType, argumentType);

        static bool TryUnifyNamedType(
            INamedTypeSymbol parameterNamed,
            INamedTypeSymbol argumentNamed,
            Dictionary<ITypeParameterSymbol, ITypeSymbol> map)
        {
            var parameterDefinition = parameterNamed.OriginalDefinition ?? parameterNamed;
            var argumentDefinition = argumentNamed.OriginalDefinition ?? argumentNamed;

            if (!SymbolEqualityComparer.Default.Equals(parameterDefinition, argumentDefinition))
                return false;

            var parameterArguments = parameterNamed.TypeArguments;
            var argumentArguments = argumentNamed.TypeArguments;

            if (parameterArguments.IsDefault || argumentArguments.IsDefault || parameterArguments.Length != argumentArguments.Length)
                return false;

            for (int i = 0; i < parameterArguments.Length; i++)
            {
                if (!TryUnifyExtensionReceiverType(parameterArguments[i], argumentArguments[i], map))
                    return false;
            }

            return true;
        }

        static bool TryUnifyArrayLike(
            INamedTypeSymbol parameterNamed,
            IArrayTypeSymbol argumentArray,
            Dictionary<ITypeParameterSymbol, ITypeSymbol> map)
        {
            var constructedFrom = parameterNamed.ConstructedFrom ?? parameterNamed;

            if (constructedFrom.SpecialType is SpecialType.System_Collections_Generic_IEnumerable_T or
                SpecialType.System_Collections_Generic_ICollection_T or
                SpecialType.System_Collections_Generic_IList_T ||
                IsGenericCollectionInterface(parameterNamed, "IReadOnlyCollection") ||
                IsGenericCollectionInterface(parameterNamed, "IReadOnlyList"))
            {
                return TryUnifyExtensionReceiverType(parameterNamed.TypeArguments[0], argumentArray.ElementType, map);
            }

            return false;
        }

        static bool IsGenericCollectionInterface(INamedTypeSymbol parameterNamed, string interfaceName)
        {
            var definition = parameterNamed.ConstructedFrom ?? parameterNamed;

            if (!string.Equals(definition.Name, interfaceName, StringComparison.Ordinal))
                return false;

            var ns = definition.ContainingNamespace?.ToDisplayString();
            return string.Equals(ns, "System.Collections.Generic", StringComparison.Ordinal);
        }
    }

    private static bool TryRecordExtensionSubstitution(
        ITypeParameterSymbol typeParameter,
        ITypeSymbol argumentType,
        Dictionary<ITypeParameterSymbol, ITypeSymbol> substitutions)
    {
        argumentType = NormalizeTypeForExtensionInference(argumentType);

        if (substitutions.TryGetValue(typeParameter, out var existing))
        {
            existing = NormalizeTypeForExtensionInference(existing);

            if (SymbolEqualityComparer.Default.Equals(existing, argumentType))
                return true;

            return false;
        }

        substitutions[typeParameter] = argumentType;
        return true;
    }

    private static ITypeSymbol NormalizeTypeForExtensionInference(ITypeSymbol type)
    {
        return type switch
        {
            LiteralTypeSymbol literal => literal.UnderlyingType,
            _ => type
        };
    }

    private static ITypeSymbol SubstituteTypeParameters(
        ITypeSymbol type,
        Dictionary<ITypeParameterSymbol, ITypeSymbol> substitutions)
    {
        if (type is ITypeParameterSymbol parameter &&
            substitutions.TryGetValue(parameter, out var replacement))
        {
            return replacement;
        }

        if (type is NullableTypeSymbol nullableType)
        {
            var substituted = SubstituteTypeParameters(nullableType.UnderlyingType, substitutions);
            return SymbolEqualityComparer.Default.Equals(substituted, nullableType.UnderlyingType)
                ? type
                : substituted.MakeNullable();
        }

        if (type is RefTypeSymbol refTypeType)
        {
            var substituted = SubstituteTypeParameters(refTypeType.ElementType, substitutions);
            return SymbolEqualityComparer.Default.Equals(substituted, refTypeType.ElementType)
                ? type
                : new RefTypeSymbol(substituted);
        }

        if (type is IAddressTypeSymbol addressType)
        {
            var substituted = SubstituteTypeParameters(addressType.ReferencedType, substitutions);
            return SymbolEqualityComparer.Default.Equals(substituted, addressType.ReferencedType)
                ? type
                : new AddressTypeSymbol(substituted);
        }

        if (type is IArrayTypeSymbol arrayType)
        {
            var substituted = SubstituteTypeParameters(arrayType.ElementType, substitutions);
            return SymbolEqualityComparer.Default.Equals(substituted, arrayType.ElementType)
                ? type
                : new ArrayTypeSymbol(arrayType.BaseType, substituted, arrayType.ContainingSymbol, arrayType.ContainingType, arrayType.ContainingNamespace, [], arrayType.Rank);
        }

        if (type is INamedTypeSymbol namedType && !namedType.TypeArguments.IsDefaultOrEmpty)
        {
            var typeArguments = namedType.TypeArguments;
            var substituted = new ITypeSymbol[typeArguments.Length];
            var changed = false;

            for (int i = 0; i < typeArguments.Length; i++)
            {
                substituted[i] = SubstituteTypeParameters(typeArguments[i], substitutions);
                changed |= !SymbolEqualityComparer.Default.Equals(substituted[i], typeArguments[i]);
            }

            if (changed)
                return namedType.Construct(substituted);
        }

        return type;
    }

    /// <summary>
    /// Phase-1 generic type inference: bind every non-lambda argument naturally (no target type)
    /// and unify its type against the corresponding parameter type to build a
    /// <c>{ T → int, U → string, … }</c> substitution map.
    ///
    /// This mirrors C#'s two-phase inference: infer type parameters from fixed arguments first,
    /// then use the inferred types when target-typing lambda arguments.
    /// </summary>
    private Dictionary<ITypeParameterSymbol, ITypeSymbol> TryPreInferTypeArguments(
        ImmutableArray<IMethodSymbol> methods,
        SeparatedSyntaxList<ArgumentSyntax> arguments,
        BoundExpression? receiver,
        ITypeSymbol? pipeReceiverType)
    {
        var substitutions = new Dictionary<ITypeParameterSymbol, ITypeSymbol>(SymbolEqualityComparer.Default);

        // Only pre-infer when there is a single candidate with type parameters.
        // With multiple overloads the "natural" types may not pick the right one.
        if (methods.IsDefaultOrEmpty || methods.Length != 1)
            return substitutions;

        var method = methods[0];
        if (method.TypeParameters.IsDefaultOrEmpty)
            return substitutions;

        // Seed from the implicit first argument (pipe source or extension receiver).
        if (pipeReceiverType is not null && !method.Parameters.IsDefaultOrEmpty)
        {
            TryUnifyExtensionReceiverType(method.Parameters[0].Type, pipeReceiverType, substitutions);
        }
        else if (method.IsExtensionMethod && receiver is not null && !method.Parameters.IsDefaultOrEmpty)
        {
            var extensionReceiverType = method.GetExtensionReceiverType() ?? method.Parameters[0].Type;
            if (extensionReceiverType is not null && extensionReceiverType.TypeKind != TypeKind.Error)
                TryUnifyExtensionReceiverType(extensionReceiverType, receiver.Type, substitutions);
        }

        for (int i = 0; i < arguments.Count; i++)
        {
            var arg = arguments[i];

            // Skip lambda arguments — they consume the inferred types, not the other way around.
            if (arg.Expression is LambdaExpressionSyntax)
                continue;

            int parameterIndex;
            if (arg.NameColon is not null)
            {
                var argName = arg.NameColon.Name.Identifier.ValueText;
                var param = method.Parameters.FirstOrDefault(p =>
                    string.Equals(p.Name, argName, StringComparison.OrdinalIgnoreCase));
                if (param is null) continue;
                parameterIndex = method.Parameters.IndexOf(param);
            }
            else
            {
                parameterIndex = (method.IsExtensionMethod || pipeReceiverType is not null) ? i + 1 : i;
            }

            if (parameterIndex < 0 || parameterIndex >= method.Parameters.Length)
                continue;

            var parameterType = method.Parameters[parameterIndex].Type;

            // Only bother if this parameter type involves type parameters.
            if (!ContainsAnyTypeParameter(parameterType, method.TypeParameters))
                continue;

            // Bind the argument without any target type to get its natural type.
            var naturalBound = BindExpression(arg.Expression);
            if (naturalBound.Type is null || naturalBound.Type.TypeKind == TypeKind.Error)
                continue;

            TryUnifyExtensionReceiverType(parameterType, naturalBound.Type, substitutions);
        }

        return substitutions;
    }

    /// <summary>
    /// Returns <c>true</c> when <paramref name="type"/> references any of the given type parameters.
    /// </summary>
    private static bool ContainsAnyTypeParameter(
        ITypeSymbol type,
        ImmutableArray<ITypeParameterSymbol> typeParameters)
    {
        if (typeParameters.IsDefaultOrEmpty)
            return false;

        return ContainsAnyTypeParameterCore(type, typeParameters);
    }

    private static bool ContainsAnyTypeParameterCore(
        ITypeSymbol type,
        ImmutableArray<ITypeParameterSymbol> typeParameters)
    {
        if (type is ITypeParameterSymbol tp)
            return typeParameters.Contains(tp, SymbolEqualityComparer.Default);

        if (type is INamedTypeSymbol named)
        {
            foreach (var arg in named.TypeArguments)
            {
                if (ContainsAnyTypeParameterCore(arg, typeParameters))
                    return true;
            }
        }

        if (type is IArrayTypeSymbol array)
            return ContainsAnyTypeParameterCore(array.ElementType, typeParameters);

        return false;
    }

    /// <summary>
    /// For lambda argument positions where no common delegate target type could be computed
    /// (i.e. <see cref="TryGetCommonPositionalParameterType"/> returned <c>null</c> because the
    /// candidates' parameter types disagree), returns the first candidate that provides a
    /// delegate type for this argument index.  This lets lambdas infer their parameter types
    /// even when multiple overloads differ only in the return-type type parameter
    /// (e.g. <c>Func&lt;int,TKey1&gt;</c> vs <c>Func&lt;int,TKey2&gt;</c>).
    /// </summary>
    private ITypeSymbol? TryGetFirstDelegateParameterType(
        ImmutableArray<IMethodSymbol> methods,
        int argumentIndex,
        BoundExpression? receiver,
        ITypeSymbol? pipeReceiverType)
    {
        ITypeSymbol? firstConcreteDelegate = null;
        var sawSystemDelegateLike = false;

        foreach (var method in methods)
        {
            if (method is null)
                continue;

            var parameterIndex = (method.IsExtensionMethod || pipeReceiverType is not null)
                ? argumentIndex + 1
                : argumentIndex;

            if (parameterIndex < 0 || parameterIndex >= method.Parameters.Length)
                continue;

            var type = GetInvocationParameterTypeForArgumentBinding(method, parameterIndex, receiver, pipeReceiverType);

            if (type is INamedTypeSymbol namedType)
            {
                if (namedType.TypeKind == TypeKind.Delegate)
                {
                    firstConcreteDelegate ??= type;
                    continue;
                }

                // Do not force the lambda into an arbitrary concrete delegate when overloads also
                // include System.Delegate/MulticastDelegate handlers (e.g. ASP.NET MapGet with
                // RequestDelegate + Delegate overloads). Let overload resolution choose first.
                if (IsSystemDelegateLike(namedType))
                    sawSystemDelegateLike = true;
            }
        }

        return sawSystemDelegateLike ? null : firstConcreteDelegate;
    }

    /// <summary>
    /// Returns <c>true</c> when the <em>input parameter types</em> (not the return type) of a
    /// delegate target type reference any of the given method type parameters.
    /// For non-delegate types this falls back to the full <see cref="ContainsAnyTypeParameter"/>
    /// check.
    /// </summary>
    /// <remarks>
    /// The lambda binder already handles an unresolved return-type type parameter by inferring
    /// the return type from the lambda body, so it is safe to use a delegate target type whose
    /// return type is still open as long as the input parameter types are fully resolved.
    /// </remarks>
    private static bool ContainsAnyTypeParameterInDelegateInputParams(
        ITypeSymbol type,
        ImmutableArray<ITypeParameterSymbol> typeParameters)
    {
        if (typeParameters.IsDefaultOrEmpty)
            return false;

        if (type is INamedTypeSymbol namedType && namedType.TypeKind == TypeKind.Delegate)
        {
            var invoke = namedType.GetDelegateInvokeMethod();
            if (invoke is not null)
            {
                // Only check input parameter types; the return type is intentionally excluded.
                foreach (var param in invoke.Parameters)
                {
                    if (ContainsAnyTypeParameterCore(param.Type, typeParameters))
                        return true;
                }
                return false;
            }
        }

        // Not a recognised delegate type — fall back to the full check.
        return ContainsAnyTypeParameterCore(type, typeParameters);
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
        if (typeSymbol.IsAbstract)
        {
            _diagnostics.ReportCannotInstantiateAbstractType(typeSymbol.Name, callSyntax.GetLocation());
            return new BoundErrorExpression(typeSymbol, null, BoundExpressionReason.OtherError);
        }

        // For uninstantiated generic types (TypeArguments empty), infer type arguments from the
        // constructor arguments BEFORE attempting overload resolution on the open type.
        // Without this, overload resolution can succeed with an unsubstituted constructor
        // (e.g. Error<E_case>.init(data: E_case) matched against an argument of type E_method),
        // because two unrelated type parameters are treated as freely compatible.  That produces a
        // SourceMethodSymbol on the open generic, which codegen emits as `newobj Type`1::.ctor(!0)`
        // (a type generic parameter) instead of the correct `newobj Type`1<!!E>::.ctor(!!E)`
        // (a method generic parameter).
        // For generic types whose type arguments are still open (either empty or all are the type's
        // own type-level parameters), infer concrete type arguments from the constructor arguments
        // BEFORE attempting overload resolution.  Without this, overload resolution can succeed with
        // an unsubstituted constructor (e.g. Error<E_case>.init(data: E_case) matched against an
        // argument of type E_method) because two unrelated type parameters are treated as freely
        // compatible.  That produces a SourceMethodSymbol on the open/self-constructed generic, which
        // codegen emits as `newobj Type`1::.ctor(!0)` instead of the correct
        // `newobj Type`1<!!E>::.ctor(!!E)`.
        if (typeSymbol.IsGenericType && IsUninstantiatedGenericType(typeSymbol)
            && TryInferConstructedTypeForConstructor(typeSymbol, boundArguments, out var earlyInferred)
            && !SymbolEqualityComparer.Default.Equals(earlyInferred, typeSymbol))
        {
            return BindConstructorInvocation(earlyInferred, boundArguments, callSyntax, receiverSyntax, receiver);
        }

        var resolution = OverloadResolver.ResolveOverload(typeSymbol.Constructors, boundArguments, Compilation, canBindLambda: EnsureLambdaCompatible, callSyntax: callSyntax);
        if (resolution.Success)
        {
            var constructor = resolution.Method!;
            if (!EnsureMemberAccessible(constructor, receiverSyntax.GetLocation(), "constructor"))
                return new BoundErrorExpression(typeSymbol, null, BoundExpressionReason.Inaccessible);
            ReportObsoleteIfNeeded(constructor, callSyntax.GetLocation());
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

            ValidateRequiredMembers(typeSymbol, constructor, callSyntax, initializerSyntax: callSyntax switch
            {
                InvocationExpressionSyntax { Initializer: { } i } => i,
                ObjectCreationExpressionSyntax { Initializer: { } i } => i,
                _ => null
            });

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

        if (TryInferConstructedTypeForConstructor(typeSymbol, boundArguments, out var inferredType) &&
            !SymbolEqualityComparer.Default.Equals(inferredType, typeSymbol))
        {
            return BindConstructorInvocation(inferredType, boundArguments, callSyntax, receiverSyntax, receiver);
        }

        ReportSuppressedLambdaDiagnostics(boundArguments);
        _diagnostics.ReportNoOverloadForMethod("constructor for type", typeSymbol.Name, boundArguments.Length, callSyntax.GetLocation());
        return new BoundErrorExpression(typeSymbol, null, BoundExpressionReason.OverloadResolutionFailed);
    }

    /// <summary>
    /// Returns true when <paramref name="typeSymbol"/> is a generic type whose type arguments are
    /// not yet concretely instantiated — either because the TypeArguments list is empty/default (the
    /// open generic form used by source and PE original-definition symbols), or because every type
    /// argument is still one of the type's OWN type-level type parameters (i.e. the type was
    /// constructed with its own parameters as placeholders, which also represents an unresolved
    /// open form).
    /// </summary>
    private static bool IsUninstantiatedGenericType(INamedTypeSymbol typeSymbol)
    {
        if (!typeSymbol.IsGenericType)
            return false;

        var typeArguments = typeSymbol.TypeArguments;
        if (typeArguments.IsDefaultOrEmpty)
            return true;

        // If all type arguments are the type's own type-level parameters then the type is
        // effectively uninstantiated (constructed with itself).
        var typeParameters = typeSymbol.TypeParameters;
        if (!typeParameters.IsDefaultOrEmpty && typeArguments.Length == typeParameters.Length)
        {
            for (int i = 0; i < typeArguments.Length; i++)
            {
                if (typeArguments[i] is not ITypeParameterSymbol { OwnerKind: TypeParameterOwnerKind.Type })
                    return false;
            }
            return true;
        }

        return false;
    }

    private bool TryInferConstructedTypeForConstructor(
        INamedTypeSymbol typeSymbol,
        BoundArgument[] boundArguments,
        out INamedTypeSymbol inferredType)
    {
        inferredType = typeSymbol;

        if (!typeSymbol.IsGenericType || typeSymbol.TypeParameters.IsDefaultOrEmpty)
            return false;

        var typeParameters = typeSymbol.TypeParameters;

        // PE generic type definitions have TypeArguments == [] (the open form uses TypeParameters
        // as placeholders). Fall back to treating each type parameter as its own current argument
        // so inference can still infer concrete types from constructor arguments.
        var originalTypeArguments = typeSymbol.TypeArguments;
        if (originalTypeArguments.IsDefaultOrEmpty)
            originalTypeArguments = typeParameters.Cast<ITypeSymbol>().ToImmutableArray();

        // When typeSymbol is already constructed (e.g. Ok<Unit> from target-typed resolution),
        // its constructors have substituted parameter types (Unit instead of T), which prevents
        // inference. Use the original definition's unsubstituted constructors instead.
        var definitionForInference = (typeSymbol.OriginalDefinition is INamedTypeSymbol origDef &&
                                      !SymbolEqualityComparer.Default.Equals(origDef, typeSymbol))
            ? origDef : typeSymbol;
        var candidates = definitionForInference.Constructors;
        if (candidates.IsDefaultOrEmpty)
            return false;

        foreach (var constructor in candidates)
        {
            var substitutions = new Dictionary<ITypeParameterSymbol, ITypeSymbol>(SymbolEqualityComparer.Default);
            if (!TryInferConstructorTypeArguments(typeParameters, constructor, boundArguments, substitutions))
                continue;

            if (substitutions.Count == 0)
                continue;

            var changed = false;
            var projected = new ITypeSymbol[typeParameters.Length];

            for (var i = 0; i < typeParameters.Length; i++)
            {
                var parameter = typeParameters[i];
                var current = originalTypeArguments[i];

                if (substitutions.TryGetValue(parameter, out var replacement))
                {
                    projected[i] = replacement;
                    if (!SymbolEqualityComparer.Default.Equals(current, replacement))
                        changed = true;
                }
                else
                {
                    projected[i] = current;
                }
            }

            if (!changed)
                continue;

            inferredType = (INamedTypeSymbol)definitionForInference.Construct(projected);
            return true;
        }

        return false;
    }

    private bool TryInferConstructorTypeArguments(
        ImmutableArray<ITypeParameterSymbol> typeParameters,
        IMethodSymbol constructor,
        BoundArgument[] boundArguments,
        Dictionary<ITypeParameterSymbol, ITypeSymbol> substitutions)
    {
        if (boundArguments.Length == 0)
            return false;

        var parameters = constructor.Parameters;
        if (parameters.IsDefaultOrEmpty)
            return false;

        for (var argumentIndex = 0; argumentIndex < boundArguments.Length; argumentIndex++)
        {
            var argument = boundArguments[argumentIndex];
            var argumentType = argument.Expression.Type?.UnwrapLiteralType() ?? argument.Expression.Type;
            if (argumentType is null || argumentType.TypeKind == TypeKind.Error)
                continue;

            IParameterSymbol? parameter = null;
            if (!string.IsNullOrEmpty(argument.Name))
                parameter = parameters.FirstOrDefault(p => string.Equals(p.Name, argument.Name, StringComparison.Ordinal));
            else if (argumentIndex < parameters.Length)
                parameter = parameters[argumentIndex];

            if (parameter is null)
                return false;

            if (!TryUnifyConstructorParameterType(typeParameters, parameter.Type, argumentType, substitutions))
                return false;
        }

        return substitutions.Count > 0;
    }

    private bool TryUnifyConstructorParameterType(
        ImmutableArray<ITypeParameterSymbol> typeParameters,
        ITypeSymbol parameterType,
        ITypeSymbol argumentType,
        Dictionary<ITypeParameterSymbol, ITypeSymbol> substitutions)
    {
        parameterType = parameterType.UnwrapLiteralType() ?? parameterType;
        argumentType = argumentType.UnwrapLiteralType() ?? argumentType;

        if (parameterType is ITypeParameterSymbol typeParameter &&
            typeParameters.Any(tp => SymbolEqualityComparer.Default.Equals(tp, typeParameter)))
        {
            if (substitutions.TryGetValue(typeParameter, out var existing))
                return SymbolEqualityComparer.Default.Equals(existing, argumentType);

            substitutions[typeParameter] = argumentType;
            return true;
        }

        if (parameterType is NullableTypeSymbol parameterNullable)
        {
            if (argumentType is NullableTypeSymbol argumentNullable)
                return TryUnifyConstructorParameterType(typeParameters, parameterNullable.UnderlyingType, argumentNullable.UnderlyingType, substitutions);

            return TryUnifyConstructorParameterType(typeParameters, parameterNullable.UnderlyingType, argumentType, substitutions);
        }

        if (parameterType is RefTypeSymbol parameterRef && argumentType is RefTypeSymbol argumentRef)
            return TryUnifyConstructorParameterType(typeParameters, parameterRef.ElementType, argumentRef.ElementType, substitutions);

        if (parameterType is IAddressTypeSymbol parameterAddress && argumentType is IAddressTypeSymbol argumentAddress)
            return TryUnifyConstructorParameterType(typeParameters, parameterAddress.ReferencedType, argumentAddress.ReferencedType, substitutions);

        if (parameterType is IArrayTypeSymbol parameterArray && argumentType is IArrayTypeSymbol argumentArray)
            return TryUnifyConstructorParameterType(typeParameters, parameterArray.ElementType, argumentArray.ElementType, substitutions);

        if (parameterType is INamedTypeSymbol parameterNamed && argumentType is INamedTypeSymbol argumentNamed)
        {
            var parameterDefinition = parameterNamed.OriginalDefinition ?? parameterNamed;
            var argumentDefinition = argumentNamed.OriginalDefinition ?? argumentNamed;

            if (!SymbolEqualityComparer.Default.Equals(parameterDefinition, argumentDefinition))
                return true;

            var parameterArguments = parameterNamed.TypeArguments;
            var argumentArguments = argumentNamed.TypeArguments;

            if (parameterArguments.IsDefaultOrEmpty || argumentArguments.IsDefaultOrEmpty || parameterArguments.Length != argumentArguments.Length)
                return true;

            for (var i = 0; i < parameterArguments.Length; i++)
            {
                if (!TryUnifyConstructorParameterType(typeParameters, parameterArguments[i], argumentArguments[i], substitutions))
                    return false;
            }

            return true;
        }

        return true;
    }

    private void ValidateRequiredMembers(
    INamedTypeSymbol typeSymbol,
    IMethodSymbol constructor,
    SyntaxNode creationSyntax,
    ObjectInitializerExpressionSyntax? initializerSyntax)
    {
        // If the selected constructor claims it sets required members, we're done.
        // Later steps can implement deeper checks (e.g. ctor chaining, actual assignments).
        if (constructor.SetsRequiredMembers)
            return;

        // Collect required member names from this type + base types.
        var required = GetRequiredMembers(typeSymbol);
        if (required.IsDefaultOrEmpty)
            return;

        // Determine which members are assigned in the object initializer.
        var assigned = initializerSyntax is null
            ? new HashSet<string>(StringComparer.Ordinal)
            : GetAssignedMemberNames(initializerSyntax);

        foreach (var memberName in required)
        {
            if (!assigned.Contains(memberName))
                _diagnostics.ReportRequiredMemberMustBeSet(memberName, creationSyntax.GetLocation());
        }
    }

    private static ImmutableArray<string> GetRequiredMembers(INamedTypeSymbol type)
    {
        var builder = ImmutableArray.CreateBuilder<string>();

        for (INamedTypeSymbol? t = type; t is not null; t = t.BaseType)
        {
            foreach (var member in t.GetMembers())
            {
                if (member is SourceFieldSymbol field && field.IsRequired)
                    builder.Add(field.Name);
                else if (member is SourcePropertySymbol prop && prop.IsRequired)
                    builder.Add(prop.Name);
            }
        }

        return builder.ToImmutable();
    }

    private static HashSet<string> GetAssignedMemberNames(ObjectInitializerExpressionSyntax initializer)
    {
        var names = new HashSet<string>(StringComparer.Ordinal);

        // Best-effort: look for assignment expressions within the initializer and capture
        // simple `Identifier = ...` and `this.Identifier = ...` / `obj.Identifier = ...`.
        foreach (var node in initializer.DescendantNodes())
        {
            if (node is ObjectInitializerAssignmentEntrySyntax assign)
            {
                switch (assign.Name)
                {
                    case IdentifierNameSyntax id:
                        names.Add(id.Identifier.ValueText);
                        break;
                }
            }
        }

        return names;
    }

    private BoundExpression BindObjectCreationExpression(ObjectCreationExpressionSyntax syntax)
    {
        INamedTypeSymbol? typeSymbol = null;

        var typeExpr = BindTypeSyntaxAsExpression(syntax.Type);

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
        if (typeSymbol.IsAbstract)
        {
            _diagnostics.ReportCannotInstantiateAbstractType(typeSymbol.Name, syntax.Type.GetLocation());
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
            ReportObsoleteIfNeeded(constructor, syntax.GetLocation());

            var convertedArgs = ConvertArguments(constructor.Parameters, boundArguments);

            BoundObjectInitializer? initializer = null;
            if (syntax.Initializer is not null)
                initializer = BindObjectInitializer(typeSymbol, syntax.Initializer);

            ValidateRequiredMembers(typeSymbol, constructor, syntax, syntax.Initializer);

            return new BoundObjectCreationExpression(constructor, convertedArgs, receiver: null, initializer);
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

        ValidateCarrierConditionalAccessReturnCompatibility(syntax, kind, resultType, errorType);

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

                // Receiver Result<TPayload, E> cases
                recvOk = FindUnionCase(receiverNamed, "Ok");
                recvErr = FindUnionCase(receiverNamed, "Error");
                resTryOk = FindTryGetValueMethod(receiverNamed, recvOk);
                resTryErr = FindTryGetValueMethod(receiverNamed, recvErr);
                recvOkValue = recvOk is not null ? FindPropertyGetter(recvOk, "Value") : null;
                recvErrData = recvErr is not null
                    ? (FindPropertyGetter(recvErr, "Data") ?? FindPropertyGetter(recvErr, "Value"))
                    : null;

                // Result<U,E> cases (carrier)
                resOk = FindUnionCase(carrierTypeSymbol, "Ok");
                resErr = FindUnionCase(carrierTypeSymbol, "Error");

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
                optSome = FindUnionCase(carrierTypeSymbol, "Some");
                optNone = FindUnionCase(carrierTypeSymbol, "None");
                optTrySome = FindTryGetValueMethod(receiverNamed, optSome);

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
            resultTryGetValueForOkCaseMethod: resTryOk,
            resultTryGetValueForErrorCaseMethod: resTryErr,
            resultOkValueGetter: resOkValue,
            resultErrorDataGetter: resErrData,
            resultOkCtor: resOkCtor,
            resultErrorCtor: resErrCtor,
            resultImplicitFromOk: resImplOk,
            resultImplicitFromError: resImplErr,

            optionSomeCaseType: optSome,
            optionNoneCaseType: optNone,
            optionTryGetValueMethod: optTrySome,
            optionSomeValueGetter: optSomeValue,
            optionSomeCtor: optSomeCtor,
            optionNoneCtorOrFactory: optNoneCtor,
            optionImplicitFromSome: optImplSome,
            optionImplicitFromNone: optImplNone);
    }

    private void ValidateCarrierConditionalAccessReturnCompatibility(
        ConditionalAccessExpressionSyntax syntax,
        BoundCarrierKind kind,
        ITypeSymbol resultType,
        ITypeSymbol? resultErrorType)
    {
        if (!IsDirectReturnExpression(syntax))
            return;

        if (!TryGetEnclosingCarrierReturnType(out var enclosingReturnType) ||
            enclosingReturnType is null ||
            enclosingReturnType.TypeKind == TypeKind.Error)
        {
            return;
        }

        if (!TryGetPropagationInfo(enclosingReturnType, out var enclosingInfo))
        {
            _diagnostics.ReportCannotConvertFromTypeToType(
                resultType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                enclosingReturnType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                syntax.OperatorToken.GetLocation());
            return;
        }

        if ((kind == BoundCarrierKind.Result && enclosingInfo.Kind != PropagationKind.Result) ||
            (kind == BoundCarrierKind.Option && enclosingInfo.Kind != PropagationKind.Option))
        {
            _diagnostics.ReportCannotConvertFromTypeToType(
                resultType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                enclosingInfo.UnionType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                syntax.OperatorToken.GetLocation());
            return;
        }

        if (kind != BoundCarrierKind.Result ||
            resultErrorType is null ||
            enclosingInfo.ErrorPayloadType is null)
        {
            return;
        }

        var errorConversion = Compilation.ClassifyConversion(resultErrorType, enclosingInfo.ErrorPayloadType);
        if (!errorConversion.Exists)
        {
            _diagnostics.ReportCannotConvertFromTypeToType(
                resultErrorType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                enclosingInfo.ErrorPayloadType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                syntax.OperatorToken.GetLocation());
        }
        else if (errorConversion.IsImplicit &&
                 !errorConversion.IsIdentity &&
                 errorConversion.IsUserDefined &&
                 errorConversion.MethodSymbol is { } conversionMethod)
        {
            _diagnostics.ReportResultPropagationImplicitErrorConversion(
                resultErrorType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                enclosingInfo.ErrorPayloadType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                conversionMethod.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat),
                syntax.OperatorToken.GetLocation());
        }
    }

    private static bool IsDirectReturnExpression(ConditionalAccessExpressionSyntax syntax)
    {
        if (syntax.Parent is ReturnStatementSyntax returnStatement &&
            ReferenceEquals(returnStatement.Expression, syntax))
        {
            return true;
        }

        return syntax.Parent is ArrowExpressionClauseSyntax arrowExpressionClause &&
               ReferenceEquals(arrowExpressionClause.Expression, syntax);
    }

    private static INamedTypeSymbol? FindUnionCase(INamedTypeSymbol carrier, string name)
    {
        var union = carrier.TryGetDiscriminatedUnion();
        if (union is not null)
        {
            var caseSymbol = union.Cases.FirstOrDefault(@case => @case.Name == name);
            if (caseSymbol is INamedTypeSymbol namedCase)
                return namedCase;
        }

        return carrier.GetTypeMembers(name).FirstOrDefault()
            ?? carrier.GetTypeMembers()
            .FirstOrDefault(t => t.Name.StartsWith(name, StringComparison.Ordinal));
    }

    private static IMethodSymbol? FindTryGetValueMethod(INamedTypeSymbol receiverType, INamedTypeSymbol? caseType)
    {
        if (caseType is null)
            return null;

        var caseTypePlain = caseType.GetPlainType();

        return receiverType.GetMembers("TryGetValue")
            .OfType<IMethodSymbol>()
            .FirstOrDefault(m =>
                m.Parameters.Length == 1 &&
                m.ReturnType.SpecialType == SpecialType.System_Boolean &&
                (m.Parameters[0].RefKind == RefKind.Out || m.Parameters[0].RefKind == RefKind.Ref) &&
                SymbolEqualityComparer.Default.Equals(
                    m.Parameters[0].GetByRefElementType().GetPlainType(),
                    caseTypePlain));
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
        return carrier.GetMembers("op_Implicit")
            .OfType<IMethodSymbol>()
            .FirstOrDefault(m =>
                m.IsStatic &&
                SymbolEqualityComparer.Default.Equals(m.ReturnType, carrier) &&
                m.Parameters.Length == 1 &&
                SymbolEqualityComparer.Default.Equals(m.Parameters[0].Type, from));
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
                        if (TryGetInvokedMemberName(memberBinding, out var memberName))
                            _diagnostics.ReportNonInvocableMember(memberName, invocation.GetLocation());
                        else
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
            var arrayType = (IArrayTypeSymbol)receiverType;
            if (argumentExprs.Length == 1 && IsRangeType(argumentExprs[0].Type))
            {
                return BindArrayRangeAccess(receiver, arrayType, argumentExprs[0], argumentList.Arguments[0].Expression);
            }

            if (argumentExprs.Any(argument => IsRangeType(argument.Type)))
            {
                _diagnostics.ReportCannotApplyIndexingWithToAnExpressionOfType(
                    syntax.GetLocation());
                return new BoundErrorExpression(receiverType, null, BoundExpressionReason.NotFound);
            }

            return new BoundArrayAccessExpression(receiver, argumentExprs, ((IArrayTypeSymbol)receiverType).ElementType);
        }

        var indexer = ResolveIndexer(receiverType, argumentExprs, argumentList.Arguments, requireSetter: false, out var convertedArguments);

        if (indexer is null)
        {
            _diagnostics.ReportCannotApplyIndexingWithToAnExpressionOfType(
                syntax.GetLocation());
            return new BoundErrorExpression(receiverType, null, BoundExpressionReason.NotFound);
        }

        return new BoundIndexerAccessExpression(receiver, convertedArguments, indexer);
    }

    private IPropertySymbol? ResolveIndexer(
        ITypeSymbol receiverType,
        BoundExpression[] arguments,
        SeparatedSyntaxList<ArgumentSyntax> argumentSyntaxes,
        bool requireSetter,
        out BoundExpression[] convertedArguments)
    {
        convertedArguments = arguments;

        var candidates = receiverType
            .GetMembers()
            .OfType<IPropertySymbol>()
            .Where(p => p.GetMethod is not null &&
                        (p.IsIndexer || p.GetMethod.Parameters.Length > 0) &&
                        (!requireSetter || p.SetMethod is not null) &&
                        p.GetMethod.Parameters.Length == arguments.Length)
            .ToArray();

        IPropertySymbol? best = null;
        BoundExpression[]? bestConverted = null;
        var bestConversionCost = int.MaxValue;

        foreach (var candidate in candidates)
        {
            var converted = new BoundExpression[arguments.Length];
            var conversionCost = 0;
            var valid = true;

            for (var i = 0; i < arguments.Length; i++)
            {
                var parameterType = candidate.GetMethod!.Parameters[i].Type;
                var convertedArgument = arguments[i];

                if (parameterType.TypeKind != TypeKind.Error &&
                    ShouldAttemptConversion(convertedArgument))
                {
                    convertedArgument = BindLambdaToDelegateIfNeeded(convertedArgument, parameterType);

                    if (!IsAssignable(parameterType, convertedArgument.Type, out var conversion))
                    {
                        valid = false;
                        break;
                    }

                    convertedArgument = ApplyConversion(convertedArgument, parameterType, conversion, argumentSyntaxes[i].Expression);
                    if (!conversion.IsIdentity)
                        conversionCost++;
                }

                converted[i] = convertedArgument;
            }

            if (!valid)
                continue;

            if (best is null || conversionCost < bestConversionCost)
            {
                best = candidate;
                bestConverted = converted;
                bestConversionCost = conversionCost;
            }
        }

        if (best is not null && bestConverted is not null)
            convertedArguments = bestConverted;

        return best;
    }

    private BoundExpression BindArrayRangeAccess(
        BoundExpression receiver,
        IArrayTypeSymbol arrayType,
        BoundExpression rangeExpression,
        ExpressionSyntax rangeSyntax)
    {
        if (arrayType.Rank != 1)
        {
            _diagnostics.ReportCannotApplyIndexingWithToAnExpressionOfType(rangeSyntax.GetLocation());
            return new BoundErrorExpression(arrayType, null, BoundExpressionReason.NotFound);
        }

        var rangeType = GetRangeType();
        if (rangeType.TypeKind != TypeKind.Error && ShouldAttemptConversion(rangeExpression))
        {
            if (!IsAssignable(rangeType, rangeExpression.Type, out var conversion))
            {
                _diagnostics.ReportCannotConvertFromTypeToType(
                    rangeExpression.Type.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    rangeType.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    rangeSyntax.GetLocation());
                return new BoundErrorExpression(rangeType, null, BoundExpressionReason.TypeMismatch);
            }

            rangeExpression = ApplyConversion(rangeExpression, rangeType, conversion, rangeSyntax);
        }

        var subArrayMethod = ResolveArraySliceMethod(arrayType);
        if (subArrayMethod is null)
        {
            _diagnostics.ReportCannotApplyIndexingWithToAnExpressionOfType(rangeSyntax.GetLocation());
            return new BoundErrorExpression(arrayType, null, BoundExpressionReason.NotFound);
        }

        return new BoundInvocationExpression(subArrayMethod, [receiver, rangeExpression], receiver: null, extensionReceiver: null);
    }

    private IMethodSymbol? ResolveArraySliceMethod(IArrayTypeSymbol arrayType)
    {
        var runtimeHelpers = Compilation.GetTypeByMetadataName("System.Runtime.CompilerServices.RuntimeHelpers");
        var rangeType = GetRangeType();
        if (runtimeHelpers is null || runtimeHelpers.TypeKind == TypeKind.Error || rangeType.TypeKind == TypeKind.Error)
            return null;

        foreach (var candidate in runtimeHelpers.GetMembers("GetSubArray").OfType<IMethodSymbol>())
        {
            if (!candidate.IsStatic || !candidate.IsGenericMethod || candidate.TypeParameters.Length != 1 || candidate.Parameters.Length != 2)
                continue;

            if (candidate.Parameters[0].Type is not IArrayTypeSymbol parameterArray || parameterArray.Rank != 1)
                continue;

            if (!SymbolEqualityComparer.Default.Equals(candidate.Parameters[1].Type, rangeType))
                continue;

            return candidate.Construct(arrayType.ElementType);
        }

        return null;
    }

    private static bool IsRangeType(ITypeSymbol? type)
        => type is INamedTypeSymbol named && named.ToFullyQualifiedMetadataName() == "System.Range";

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
                if (args.Any(argument => IsRangeType(argument.Type)))
                {
                    _diagnostics.ReportLeftOfAssignmentMustBeAVariablePropertyOrIndexer(node.GetLocation());
                    return new BoundErrorExpression(receiver.Type!, null, BoundExpressionReason.NotFound);
                }

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

            var indexer = ResolveIndexer(receiver.Type!, args, elementAccess.ArgumentList.Arguments, requireSetter: true, out var convertedArguments);

            if (indexer is null || !indexer.IsMutable)
            {
                _diagnostics.ReportLeftOfAssignmentMustBeAVariablePropertyOrIndexer(node.GetLocation());
                return new BoundErrorExpression(receiver.Type!, null, BoundExpressionReason.NotFound);
            }

            var indexerRightExpression = BindExpressionWithTargetType(rightSyntax, indexer.Type);

            if (IsErrorExpression(indexerRightExpression))
                return AsErrorExpression(indexerRightExpression);

            var access = new BoundIndexerAccessExpression(receiver, convertedArguments, indexer);
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

        if (left is BoundDereferenceExpression dereference)
        {
            var right2 = BindExpressionWithTargetType(rightSyntax, dereference.ElementType);

            if (IsErrorExpression(right2))
                return AsErrorExpression(right2);

            var converted = ConvertValueForAssignment(right2, dereference.ElementType, rightSyntax);
            if (converted is BoundErrorExpression)
                return converted;

            return BoundFactory.CreateByRefAssignmentExpression(dereference.Reference, dereference.ElementType, converted);
        }

        if (left is BoundLocalAccess localAccess)
        {
            var localSymbol = localAccess.Local;
            var localType = localSymbol.Type;

            var rightTargetType = localType is RefTypeSymbol refTypeLocal
                ? refTypeLocal.ElementType
                : localType;
            var right2 = BindExpressionWithTargetType(rightSyntax, rightTargetType);

            if (IsErrorExpression(right2))
                return AsErrorExpression(right2);

            if (localType is RefTypeSymbol refTypeLocalType)
            {
                var converted = ConvertValueForAssignment(right2, refTypeLocalType.ElementType, rightSyntax);
                if (converted is BoundErrorExpression)
                    return converted;

                return BoundFactory.CreateByRefAssignmentExpression(localAccess, refTypeLocalType.ElementType, converted);
            }

            if (!localSymbol.IsMutable)
            {
                _diagnostics.ReportThisValueIsNotMutable(leftSyntax.GetLocation());
                return ErrorExpression(reason: BoundExpressionReason.NotFound);
            }

            if (right2 is BoundEmptyCollectionExpression)
            {
                return BoundFactory.CreateLocalAssignmentExpression(localSymbol, localAccess, new BoundEmptyCollectionExpression(localSymbol.Type));
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

            return BoundFactory.CreateLocalAssignmentExpression(localSymbol, localAccess, right2);
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

            var rightTargetType = parameterSymbol.RefKind is RefKind.Ref or RefKind.Out
                ? parameterSymbol.GetByRefElementType()
                : parameterType;
            var right2 = BindExpressionWithTargetType(rightSyntax, rightTargetType);

            if (IsErrorExpression(right2))
                return AsErrorExpression(right2);

            if (parameterSymbol.RefKind is RefKind.Ref or RefKind.Out)
            {
                var byRefElementType = parameterSymbol.GetByRefElementType();
                var converted = ConvertValueForAssignment(right2, byRefElementType, rightSyntax);
                if (converted is BoundErrorExpression)
                    return converted;

                return BoundFactory.CreateByRefAssignmentExpression(parameterAccess, byRefElementType, converted);
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

            return BoundFactory.CreateParameterAssignmentExpression(parameterSymbol, parameterAccess, right2);
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
            var useFieldOnlyLowering = TryGetFieldOnlyPropertyBackingField(propertySymbol, out backingField);

            var receiver = GetReceiver(left);

            if (IsInitOnly(propertySymbol) && !IsInInitOnlyAssignmentContext)
            {
                // IMPORTANT: reuse your *existing* “property has no setter / not writable” diagnostic path
                // (same one you use when SetMethod is null), so you don’t need a new diagnostic.

                // Example shape (adapt to your existing diag helpers):
                _diagnostics.ReportPropertyOrIndexerCannotBeAssignedIsReadOnly(propertySymbol.Name, leftSyntax.GetLocation());
                return new BoundErrorExpression(propertySymbol.Type ?? Compilation.ErrorTypeSymbol, propertySymbol, BoundExpressionReason.UnsupportedOperation);
            }

            if (!useFieldOnlyLowering && !propertySymbol.IsMutable)
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

    private BoundExpression BindMemberAccessExpression(
       MemberAccessExpressionSyntax memberAccess,
       bool preferMethods = false,
       bool allowEventAccess = false)
    {
        if (memberAccess.IsKind(SyntaxKind.PointerMemberAccessExpression))
        {
            return BindPointerMemberAccessExpression(memberAccess, preferMethods, allowEventAccess);
        }

        // First, attempt to treat the *entire* member access as a type name.
        // This enables nested-type construction like `Outer<int>.Inner<string>` and `Foo<int>.Bar`.
        // Avoid rewriting syntax trees; resolve directly in the binder.
        if (TryBindMemberAccessExpressionAsType(memberAccess, out var resolvedType) &&
            resolvedType.TypeKind != TypeKind.Error)
        {
            // If the resolved type is a DU case type (e.g. Err.MissingName), promote it to a
            // BoundUnionCaseExpression whose Type is the union root so that generic type
            // inference infers Err rather than the individual case type MissingName.
            if (BindDiscriminatedUnionCaseType(resolvedType) is { } unionCaseExpr)
                return unionCaseExpr;

            return new BoundTypeExpression(resolvedType);
        }

        // Regular expression receiver binding.
        // IMPORTANT: Many name nodes (IdentifierName/GenericName/QualifiedName) also implement TypeSyntax.
        // We must NOT treat those as type receivers here, or ordinary member access like `handler.Handle`
        // would be bound as a static type access.
        BoundExpression receiver;
        if (memberAccess.Expression is TypeSyntax ts && memberAccess.Expression is not NameSyntax)
        {
            receiver = BindTypeSyntaxAsExpression(ts);
        }
        else
        {
            receiver = BindExpression(memberAccess.Expression);
        }

        if (IsErrorExpression(receiver))
            return receiver is BoundErrorExpression boundError
                ? boundError
                : new BoundErrorExpression(receiver.Type ?? Compilation.ErrorTypeSymbol, null, BoundExpressionReason.OtherError);

        if (receiver.Type is { } boundReceiverType && boundReceiverType.ContainsErrorType())
            return new BoundErrorExpression(boundReceiverType, receiver.Symbol, BoundExpressionReason.OtherError);

        if (memberAccess.OperatorToken.Kind == SyntaxKind.ArrowToken)
        {
            if (!IsUnsafeEnabled)
            {
                _diagnostics.ReportPointerOperationRequiresUnsafe(memberAccess.OperatorToken.GetLocation());
                return ErrorExpression(reason: BoundExpressionReason.UnsupportedOperation);
            }

            if (receiver.Type is not IPointerTypeSymbol pointerReceiver)
            {
                var receiverTypeText = (receiver.Type ?? Compilation.ErrorTypeSymbol)
                    .ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);
                _diagnostics.ReportPointerMemberAccessRequiresPointer(receiverTypeText, memberAccess.OperatorToken.GetLocation());
                return ErrorExpression(reason: BoundExpressionReason.TypeMismatch);
            }

            var dereferencedReceiver = new BoundDereferenceExpression(receiver, pointerReceiver.PointedAtType);
            return BindMemberAccessOnReceiver(
                dereferencedReceiver,
                memberAccess.Name,
                preferMethods,
                allowEventAccess,
                suppressNullWarning: true,
                receiverTypeForLookup: pointerReceiver.PointedAtType,
                forceExtensionReceiver: false);
        }

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

    private BoundExpression BindPointerMemberAccessExpression(
        MemberAccessExpressionSyntax memberAccess,
        bool preferMethods,
        bool allowEventAccess)
    {
        // Bind receiver expression (same pattern as normal member access).
        BoundExpression receiver;
        if (memberAccess.Expression is TypeSyntax ts && memberAccess.Expression is not NameSyntax)
            receiver = BindTypeSyntaxAsExpression(ts);
        else
            receiver = BindExpression(memberAccess.Expression);

        if (IsErrorExpression(receiver))
            return receiver is BoundErrorExpression boundError
                ? boundError
                : new BoundErrorExpression(receiver.Type ?? Compilation.ErrorTypeSymbol, null, BoundExpressionReason.OtherError);

        if (!IsUnsafeEnabled)
        {
            _diagnostics.ReportPointerOperationRequiresUnsafe(memberAccess.OperatorToken.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.UnsupportedOperation);
        }

        if (receiver.Type is not IPointerTypeSymbol pointerReceiver)
        {
            var receiverTypeText = (receiver.Type ?? Compilation.ErrorTypeSymbol)
                .ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);

            _diagnostics.ReportPointerMemberAccessRequiresPointer(receiverTypeText, memberAccess.OperatorToken.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.TypeMismatch);
        }

        var lookupType = pointerReceiver.PointedAtType;

        // Bind as if it were `(*receiver).Name` to reuse the proven lookup logic.
        var dereferencedReceiver = new BoundDereferenceExpression(receiver, lookupType);

        var bound = BindMemberAccessOnReceiver(
            dereferencedReceiver,
            memberAccess.Name,
            preferMethods,
            allowEventAccess,
            suppressNullWarning: true,
            receiverTypeForLookup: lookupType,
            forceExtensionReceiver: false);

        if (IsErrorExpression(bound))
            return AsErrorExpression(bound);

        // If it’s a plain member access, preserve the pointer nature explicitly.
        if (bound is BoundMemberAccessExpression ma)
            return new BoundPointerMemberAccessExpression(receiver, ma.Member, ma.Reason);

        // Method groups / invocations / other special forms: return as-is for now.
        return bound;
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
        // Raven feature: allow partial explicit method type arguments at the call site, e.g.:
        //   items.CountItems<double>(2)
        // Overload resolution will right-align the explicit args and infer the rest.
        // Therefore, member lookup must NOT require exact generic arity matches here.
        //
        // Implementation: strip the type-argument list for lookup so the existing method/extension
        // lookup can find candidates by name, then let OverloadResolver apply the explicit args.
        if (preferMethods && simpleName is GenericNameSyntax g)
        {
            var identifierOnly = SyntaxFactory.IdentifierName(g.Identifier);

            return BindMemberAccessOnReceiver(
                receiver,
                identifierOnly,
                preferMethods,
                allowEventAccess,
                suppressNullWarning,
                receiverTypeForLookup,
                forceExtensionReceiver);
        }

        // NOTE: For '.' we already reported null-ref and pass suppressNullWarning=true here.
        // For '?.' we also suppress, because conditional access handles null.
        // This method should not report possible null reference itself.

        if (simpleName.Identifier.IsMissing)
        {
            _diagnostics.ReportIdentifierExpected(simpleName.Identifier.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.NotFound);
        }

        var name = simpleName.Identifier.ValueText;
        ImmutableArray<ITypeSymbol>? explicitTypeArguments = null;
        GenericNameSyntax? genericTypeSyntax = null;

        if (simpleName is GenericNameSyntax genericName)
        {
            _ = BindTypeSyntax(genericName);

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

                        // For DU case types (e.g. Err.MissingName), return a BoundUnionCaseExpression
                        // whose Type is the union root (Err) so that generic type inference infers
                        // the union root rather than the individual case type.
                        if (BindDiscriminatedUnionCaseType(typeMember, typeExpr.Type as INamedTypeSymbol) is { } unionCaseExpr)
                            return ApplyTargetTypedUnionCarrier(unionCaseExpr, simpleName);

                        return new BoundTypeExpression(typeMember);
                    }

                    ReportObsoleteIfNeeded(nonMethodMember, nameLocation);
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
                            return BindExtensionPropertyGetInvocation(typeExpr, accessibleProperties[0], receiverSyntax: simpleName);

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
                    return ApplyTargetTypedUnionCarrier(unionCase, simpleName);

                return new BoundTypeExpression(typeMemberSymbol);
            }

            if (member is IEventSymbol && !allowEventAccess)
            {
                _diagnostics.ReportEventCanOnlyBeUsedWithPlusOrMinus(member.Name, nameLocation);
                return ErrorExpression(reason: BoundExpressionReason.NotFound);
            }

            ReportObsoleteIfNeeded(member, nameLocation);
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
                {
                    ReportObsoleteIfNeeded(nonMethodMember, nameLocation);

                    if (nonMethodMember is IPropertySymbol property &&
                        TryGetFieldOnlyPropertyBackingField(property, out var backingField))
                    {
                        return new BoundMemberAccessExpression(receiver, backingField);
                    }

                    return new BoundMemberAccessExpression(receiver, nonMethodMember);
                }
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

                ReportObsoleteIfNeeded(instanceMember, nameLocation);

                if (instanceMember is IPropertySymbol property &&
                    TryGetFieldOnlyPropertyBackingField(property, out var backingField))
                {
                    return new BoundMemberAccessExpression(receiver, backingField);
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
                        {
                            var receiverSyntaxNode = simpleName.Parent ?? (SyntaxNode)simpleName;
                            return BindExtensionPropertyGetInvocation(receiver, accessibleProperties[0], receiverSyntaxNode);
                        }

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

        if (preferMethods && receiver.Type is not null)
        {
            var receiverType = receiverTypeForLookup ?? (receiver.Type.UnwrapLiteralType() ?? receiver.Type);
            _diagnostics.ReportMemberDoesNotContainDefinition(receiverType.Name, name, nameLocation ?? simpleName.GetLocation() ?? Location.None);
        }
        else
        {
            _diagnostics.ReportTheNameDoesNotExistInTheCurrentContext(name, nameLocation ?? simpleName.GetLocation() ?? Location.None);
        }
        return ErrorExpression(reason: BoundExpressionReason.NotFound);
    }

    private BoundExpression ApplyTargetTypedUnionCarrier(BoundExpression expression, SyntaxNode syntax)
    {
        if (expression is not BoundUnionCaseExpression unionCase)
            return expression;

        var targetType = GetTargetType(syntax);
        if (targetType is null)
            return expression;

        targetType = UnwrapAlias(targetType);
        targetType = UnwrapTaskLikeTargetType(targetType);

        var targetUnion =
            targetType.TryGetDiscriminatedUnion() as INamedTypeSymbol
            ?? targetType.TryGetDiscriminatedUnionCase()?.Union as INamedTypeSymbol;

        if (targetUnion is null)
            return expression;

        if (!SymbolEqualityComparer.Default.Equals(
                (unionCase.UnionType.OriginalDefinition as INamedTypeSymbol) ?? unionCase.UnionType,
                (targetUnion.OriginalDefinition as INamedTypeSymbol) ?? targetUnion))
        {
            return expression;
        }

        if (SymbolEqualityComparer.Default.Equals(unionCase.UnionType, targetUnion))
            return expression;

        if (IsUninstantiatedGenericType(unionCase.UnionType) || !IsUninstantiatedGenericType(targetUnion))
        {
            return new BoundUnionCaseExpression(
                targetUnion,
                unionCase.CaseType,
                unionCase.CaseConstructor,
                unionCase.Arguments);
        }

        return expression;
    }

    private BoundExpression BindExtensionPropertyGetInvocation(
        BoundExpression receiver,
        IPropertySymbol property,
        SyntaxNode receiverSyntax)
    {
        ReportObsoleteIfNeeded(property, receiverSyntax.GetLocation());

        var getter = property.GetMethod;
        if (getter is null)
            return new BoundMemberAccessExpression(receiver, property);

        // Lowered extension-property getter should be: static get_Xxx(self: TReceiver, ...) -> TResult
        var looksLikeLoweredExtensionGetter =
            getter.IsStatic &&
            getter.Parameters.Length > 0 &&
            string.Equals(getter.Parameters[0].Name, "self", StringComparison.Ordinal);

        // If this isn’t the lowered extension form, keep it as a normal property access.
        if (!looksLikeLoweredExtensionGetter && !getter.IsExtensionMethod)
            return new BoundMemberAccessExpression(receiver, property);

        // Property getter has no call-site args.
        var boundArguments = Array.Empty<BoundArgument>();

        // Receiver becomes the extension receiver.
        var extensionReceiver = receiver;

        // Critical: run type argument inference so T/E get inferred from `self` (and/or return type later).
        var inferred = OverloadResolver.ApplyTypeArgumentInference(
            getter,
            extensionReceiver,
            boundArguments,
            Compilation,
            explicitTypeArguments: ImmutableArray<ITypeSymbol>.Empty);

        var chosen = inferred ?? getter;

        var convertedArgs = ConvertInvocationArguments(
            chosen,
            boundArguments,
            extensionReceiver,
            receiverSyntax,
            out var convertedExtensionReceiver);

        // Extension call: no instance receiver, only ExtensionReceiver.
        ReportObsoleteIfNeeded(chosen, receiverSyntax.GetLocation());
        return new BoundInvocationExpression(
            chosen,
            convertedArgs,
            receiver: null,
            convertedExtensionReceiver);
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

        if (expectedType is null && memberBinding.Parent is InvocationExpressionSyntax invocation)
        {
            expectedType = GetTargetType(invocation);

            if (expectedType is null &&
                invocation.Parent is ReturnStatementSyntax &&
                ContainingSymbol is IMethodSymbol containingMethod)
            {
                expectedType = GetReturnTargetType(containingMethod);
            }
        }

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
                    {
                        ReportObsoleteIfNeeded(accessibleProperties[0], nameLocation);
                        return new BoundMemberAccessExpression(new BoundTypeExpression(expectedType), accessibleProperties[0]);
                    }

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

            _diagnostics.ReportTheNameDoesNotExistInTheCurrentContext(memberName, nameLocation ?? Location.None);
            return ErrorExpression(reason: BoundExpressionReason.NotFound);
        }

        if (!EnsureMemberAccessible(member, nameLocation, GetSymbolKindForDiagnostic(member)))
            return ErrorExpression(reason: BoundExpressionReason.Inaccessible);

        if (member is ITypeSymbol typeMember)
        {
            // Prefer contextual DU-case binding so case type arguments are projected from the
            // expected union carrier (e.g. `.Created` in `HttpResult<()>` should not stay
            // `HttpResult<T>`).
            if (TryBindDiscriminatedUnionCase(expectedType, memberName, nameLocation) is { } contextualUnionCase)
            {
                return contextualUnionCase;
            }

            if (BindDiscriminatedUnionCaseType(typeMember, expectedType as INamedTypeSymbol) is { } unionCase)
            {
                // Unit-case sugar: `.Case` => `.Case(())` for single-Unit payload cases,
                // but ONLY when `.Case` is used as a standalone expression.
                var isInvocationCallee =
                    simpleName.Parent is MemberBindingExpressionSyntax mb &&
                    mb.Parent is InvocationExpressionSyntax inv &&
                    ReferenceEquals(inv.Expression, mb);

                // Unit-payload sugar: when the case has no resolved constructor yet (null
                // CaseConstructor on BoundUnionCaseExpression) it may have a unit-arg ctor.
                // Also handle the legacy BoundTypeExpression path.
                if (!isInvocationCallee)
                {
                    INamedTypeSymbol? caseTypeForUnit = null;
                    INamedTypeSymbol? unionTypeForUnit = null;

                    if (unionCase is BoundUnionCaseExpression { CaseConstructor: null } rawUnionCase)
                    {
                        caseTypeForUnit = rawUnionCase.CaseType;
                        unionTypeForUnit = rawUnionCase.UnionType;
                    }
                    else if (unionCase is BoundTypeExpression { Type: INamedTypeSymbol legacyCaseType } &&
                             legacyCaseType.TryGetDiscriminatedUnionCase() is not null)
                    {
                        caseTypeForUnit = legacyCaseType;
                    }

                    if (caseTypeForUnit is not null)
                    {
                        var unitArgCtor = caseTypeForUnit.Constructors.FirstOrDefault(ctor =>
                            ctor.Parameters.Length == 1 &&
                            IsUnitType(ctor.Parameters[0].Type));

                        if (unitArgCtor is not null)
                        {
                            if (!EnsureMemberAccessible(unitArgCtor, nameLocation, "constructor"))
                                return ErrorExpression(reason: BoundExpressionReason.Inaccessible);
                            ReportObsoleteIfNeeded(unitArgCtor, nameLocation);

                            var unitType = unitArgCtor.Parameters[0].Type;
                            var unitValue = new BoundUnitExpression(unitType);

                            // Return a BoundUnionCaseExpression so the type is the union root.
                            if (unionTypeForUnit is not null)
                                return new BoundUnionCaseExpression(unionTypeForUnit, caseTypeForUnit, unitArgCtor, ImmutableArray.Create<BoundExpression>(unitValue));

                            return new BoundObjectCreationExpression(
                                unitArgCtor,
                                ImmutableArray.Create<BoundExpression>(unitValue));
                        }
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

        ReportObsoleteIfNeeded(member, nameLocation);
        return new BoundMemberAccessExpression(new BoundTypeExpression(expectedType), member);
    }

    private BoundExpression? TryBindDiscriminatedUnionCase(ITypeSymbol? receiverType, string memberName, Location location)
    {
        var targetType = receiverType?.UnwrapLiteralType() ?? receiverType;
        targetType = UnwrapAlias(targetType);
        if (targetType is not null)
            targetType = UnwrapTaskLikeTargetType(targetType);

        if (targetType is not INamedTypeSymbol namedType)
            return null;

        // Keep two views:
        // 1) union definition for case-name lookup
        // 2) carrier type (possibly constructed) for projecting case type arguments
        var unionSymbol = namedType.TryGetDiscriminatedUnion()
            ?? namedType.TryGetDiscriminatedUnionCase()?.Union;

        var unionCarrier = unionSymbol is not null
            ? namedType
            : namedType.TryGetDiscriminatedUnionCase()?.Union as INamedTypeSymbol;

        if (unionCarrier is null && unionSymbol is not null && namedType.TryGetDiscriminatedUnion() is not null)
            unionCarrier = namedType;

        if (unionSymbol is null || unionCarrier is null)
            return null;

        var caseSymbol = unionSymbol.Cases.FirstOrDefault(@case => @case.Name == memberName);
        if (caseSymbol is not ITypeSymbol typeMember)
            return null;

        if (caseSymbol is INamedTypeSymbol caseTypeSymbol)
        {
            typeMember = ProjectCaseTypeToUnionArguments(caseTypeSymbol, unionCarrier);
        }

        var accessibleType = EnsureTypeAccessible(typeMember, location);
        if (accessibleType.TypeKind == TypeKind.Error)
            return ErrorExpression(reason: BoundExpressionReason.Inaccessible);

        // Pass the union carrier so the returned expression's Type is the union root, enabling
        // correct generic type inference when the case value is used as a generic argument.
        return BindDiscriminatedUnionCaseType(accessibleType, unionCarrier);
    }

    private static ITypeSymbol ProjectCaseTypeToUnionArguments(INamedTypeSymbol caseType, INamedTypeSymbol unionType)
    {
        if (!caseType.IsGenericType || caseType.TypeParameters.IsDefaultOrEmpty)
            return caseType;

        var unionDefinition = unionType.TryGetDiscriminatedUnion() ?? unionType;
        var unionTypeParameters = unionDefinition.TypeParameters;
        var unionTypeArguments = unionType.TypeArguments;

        if (unionTypeParameters.IsDefaultOrEmpty || unionTypeArguments.IsDefaultOrEmpty)
            return caseType;

        var projectedArguments = new ITypeSymbol[caseType.TypeParameters.Length];
        var changed = false;
        var caseSymbol = caseType.TryGetDiscriminatedUnionCase();
        if (caseSymbol is null)
            return caseType;

        for (var i = 0; i < caseType.TypeParameters.Length; i++)
        {
            var parameter = caseType.TypeParameters[i];
            if (DiscriminatedUnionFacts.TryProjectCaseTypeParameterFromUnionArguments(
                caseSymbol,
                parameter,
                unionTypeParameters,
                unionTypeArguments,
                out var mapped))
            {
                projectedArguments[i] = mapped;
                if (!SymbolEqualityComparer.Default.Equals(mapped, parameter))
                    changed = true;
            }
            else
            {
                projectedArguments[i] = parameter;
            }
        }

        if (!changed)
            return caseType;

        return caseType.Construct(projectedArguments);
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

    /// <summary>
    /// Binds a discriminated union case type symbol as a value expression.
    /// When <paramref name="unionCarrier"/> is supplied the returned expression is a
    /// <see cref="BoundUnionCaseExpression"/> whose <c>Type</c> is the union root, so that
    /// generic type inference sees the union root rather than the individual case type.
    /// When <paramref name="unionCarrier"/> is not supplied (legacy call sites that have no
    /// explicit carrier context) the method falls back to the old behaviour and returns a
    /// <see cref="BoundObjectCreationExpression"/> or <see cref="BoundTypeExpression"/>.
    /// </summary>
    private BoundExpression? BindDiscriminatedUnionCaseType(ITypeSymbol typeMember, INamedTypeSymbol? unionCarrier = null)
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
            // Resolve the union carrier: prefer the explicitly supplied one, then fall back to
            // what the case type itself reports.
            var resolvedUnion = unionCarrier
                ?? caseType.TryGetDiscriminatedUnionCase()?.Union as INamedTypeSymbol
                ?? caseType.ContainingType?.TryGetDiscriminatedUnion() as INamedTypeSymbol;

            var parameterlessCtor = caseType.Constructors.FirstOrDefault(static ctor => ctor.Parameters.Length == 0);

            if (parameterlessCtor is not null)
            {
                // Return a BoundUnionCaseExpression whose Type is the union root so that generic
                // type inference infers the union root (e.g. Err) and not the case type
                // (e.g. Err_MissingName).
                if (resolvedUnion is not null)
                    return new BoundUnionCaseExpression(resolvedUnion, caseType, parameterlessCtor, ImmutableArray<BoundExpression>.Empty);

                // Fallback (no union info available): legacy behaviour.
                return new BoundObjectCreationExpression(parameterlessCtor, ImmutableArray<BoundExpression>.Empty);
            }

            // No parameterless ctor: fall back to a BoundTypeExpression so the caller can
            // perform unit-payload sugar or invocation binding.
            if (resolvedUnion is not null)
                return new BoundUnionCaseExpression(resolvedUnion, caseType, null, ImmutableArray<BoundExpression>.Empty);
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
