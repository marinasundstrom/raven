using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed class OverloadResolver
{
    public static OverloadResolutionResult ResolveOverload(
        IEnumerable<IMethodSymbol> methods,
        BoundArgument[] arguments,
        Compilation compilation,
        BoundExpression? receiver = null,
        Func<IParameterSymbol, BoundLambdaExpression, bool>? canBindLambda = null)
    {
        IMethodSymbol? bestMatch = null;
        int bestScore = int.MaxValue;
        ImmutableArray<IMethodSymbol>.Builder? ambiguous = null;
        bool bestIsExtension = false;

        foreach (var candidate in methods)
        {
            var method = ApplyTypeArgumentInference(candidate, receiver, arguments, compilation);
            if (method is null)
                continue;

            var parameters = method.Parameters;
            var treatAsExtension = method.IsExtensionMethod && receiver is not null;
            var providedCount = arguments.Length + (treatAsExtension ? 1 : 0);

            if (!HasSufficientArguments(parameters, providedCount))
                continue;

            if (!TryMatch(method, arguments, receiver, treatAsExtension, compilation, canBindLambda, out var score))
                continue;

            if (score < bestScore)
            {
                bestMatch = method;
                bestScore = score;
                ambiguous = null;
                bestIsExtension = treatAsExtension;
                continue;
            }

            if (score > bestScore || bestMatch is null)
                continue;

            if (bestIsExtension != treatAsExtension)
            {
                if (!treatAsExtension)
                {
                    bestMatch = method;
                    bestIsExtension = false;
                    ambiguous = null;
                }
                continue;
            }

            if (IsMoreSpecific(method, bestMatch, arguments, receiver, compilation))
            {
                bestMatch = method;
                ambiguous = null;
                bestIsExtension = treatAsExtension;
                continue;
            }

            if (IsMoreSpecific(bestMatch, method, arguments, receiver, compilation))
                continue;

            if (SymbolEqualityComparer.Default.Equals(bestMatch, method))
                continue;

            ambiguous ??= ImmutableArray.CreateBuilder<IMethodSymbol>();
            AddCandidateIfMissing(ambiguous, bestMatch);
            AddCandidateIfMissing(ambiguous, method);
        }

        if (ambiguous is { Count: > 0 })
            return OverloadResolutionResult.Ambiguous(ambiguous.ToImmutable());

        return new OverloadResolutionResult(bestMatch);
    }

    internal static IMethodSymbol? ApplyTypeArgumentInference(
        IMethodSymbol method,
        BoundExpression? receiver,
        BoundArgument[] arguments,
        Compilation compilation)
    {
        if (!method.IsGenericMethod || method.TypeParameters.IsDefaultOrEmpty || method.TypeParameters.Length == 0)
            return method;

        if (!ReferenceEquals(method, method.ConstructedFrom))
            return method;

        var treatAsExtension = method.IsExtensionMethod && receiver is not null;
        return TryConstructMethodWithInference(method, receiver, arguments, treatAsExtension, compilation);
    }

    private static IMethodSymbol? TryConstructMethodWithInference(
        IMethodSymbol method,
        BoundExpression? receiver,
        BoundArgument[] arguments,
        bool treatAsExtension,
        Compilation compilation)
    {
        var substitutions = new Dictionary<ITypeParameterSymbol, ITypeSymbol>(SymbolEqualityComparer.Default);
        var parameters = method.Parameters;
        var parameterIndex = 0;

        if (treatAsExtension)
        {
            if (receiver?.Type is null)
                return null;

            if (!TryInferFromTypes(compilation, parameters[parameterIndex].Type, receiver.Type, substitutions, method))
                return null;

            parameterIndex++;
        }

        if (!TryMapArguments(parameters, arguments, treatAsExtension, out var mappedArguments))
            return null;

        for (; parameterIndex < parameters.Length; parameterIndex++)
        {
            var mapped = mappedArguments[parameterIndex];
            if (mapped is null)
                continue;

            var expression = mapped.Value.Expression;

            if (expression is BoundLambdaExpression lambda)
            {
                if (!TryInferFromLambda(compilation, parameters[parameterIndex].Type, lambda, substitutions, method))
                    return null;

                continue;
            }

            var argumentType = expression.Type;
            if (argumentType is null || argumentType.TypeKind == TypeKind.Error)
                continue;

            if (!TryInferFromTypes(compilation, parameters[parameterIndex].Type, argumentType, substitutions, method))
                return null;
        }

        var inferredArguments = new ITypeSymbol[method.TypeParameters.Length];
        for (int i = 0; i < method.TypeParameters.Length; i++)
        {
            var typeParameter = method.TypeParameters[i];
            if (!substitutions.TryGetValue(typeParameter, out var inferred))
                return null;

            inferredArguments[i] = NormalizeType(inferred);
        }

        var immutableArguments = ImmutableArray.CreateRange(inferredArguments);

        if (!SatisfiesMethodConstraints(method, immutableArguments))
            return null;

        return method.Construct(inferredArguments);
    }

    private static bool TryInferFromLambda(
        Compilation compilation,
        ITypeSymbol parameterType,
        BoundLambdaExpression lambda,
        Dictionary<ITypeParameterSymbol, ITypeSymbol> substitutions,
        IMethodSymbol? inferenceMethod)
    {
        if (parameterType is not INamedTypeSymbol delegateType ||
            delegateType.TypeKind != TypeKind.Delegate)
        {
            return true;
        }

        var invoke = delegateType.GetDelegateInvokeMethod();
        if (invoke is null)
            return true;

        var lambdaParameters = lambda.Parameters.ToImmutableArray();
        if (invoke.Parameters.Length != lambdaParameters.Length)
            return false;

        for (int i = 0; i < invoke.Parameters.Length; i++)
        {
            var parameter = invoke.Parameters[i];
            var lambdaParameter = lambdaParameters[i];
            var lambdaParameterType = lambdaParameter.Type;

            if (lambdaParameterType is null || lambdaParameterType.TypeKind == TypeKind.Error)
                continue;

            if (lambdaParameterType is ITypeParameterSymbol)
                continue;

            if (!TryInferFromTypes(compilation, parameter.Type, lambdaParameterType, substitutions, inferenceMethod))
                return false;
        }

        var lambdaReturnType = lambda.ReturnType;
        ITypeSymbol? collectedAsyncReturn = null;

        bool ContainsTypeParameter(ITypeSymbol type)
        {
            switch (type)
            {
                case ITypeParameterSymbol:
                    return true;
                case INamedTypeSymbol named when !named.TypeArguments.IsDefaultOrEmpty:
                    return named.TypeArguments.Any(ContainsTypeParameter);
                case IArrayTypeSymbol array:
                    return ContainsTypeParameter(array.ElementType);
                case ByRefTypeSymbol byRef:
                    return ContainsTypeParameter(byRef.ElementType);
                case NullableTypeSymbol nullable:
                    return ContainsTypeParameter(nullable.UnderlyingType);
                case ITupleTypeSymbol tuple:
                    return tuple.TupleElements.Any(e => ContainsTypeParameter(e.Type));
                default:
                    return false;
            }
        }

        if (lambda.Symbol is ILambdaSymbol { IsAsync: true })
        {
            collectedAsyncReturn = ReturnTypeCollector.InferAsync(compilation, lambda.Body);

            // Prefer the already computed async return over re-inferring from the raw body
            // so we keep task-shaped inference (including nullable/Task<Unit> normalization)
            // when replaying the lambda for generic inference.
            if (lambdaReturnType is { TypeKind: not TypeKind.Error })
            {
                lambdaReturnType = AsyncReturnTypeUtilities.InferAsyncReturnType(compilation, lambdaReturnType);
            }
            else
            {
                lambdaReturnType = AsyncReturnTypeUtilities.InferAsyncReturnType(compilation, lambda.Body);
            }

            // If the async return still contains type parameters (e.g., Task<T>), fall back to
            // inferring the async return directly from the body so overload resolution can learn
            // about concrete return values such as `int` from `return 42` in a block-bodied async
            // lambda.
            if (lambdaReturnType is { TypeKind: not TypeKind.Error } withTypeParams && ContainsTypeParameter(withTypeParams))
            {
                var inferredFromBody = collectedAsyncReturn ?? AsyncReturnTypeUtilities.InferAsyncReturnType(compilation, lambda.Body);
                if (inferredFromBody is { TypeKind: not TypeKind.Error })
                    lambdaReturnType = inferredFromBody;
            }
        }

        if ((lambdaReturnType is null || lambdaReturnType.TypeKind == TypeKind.Error) &&
            lambda.Body.Type is { TypeKind: not TypeKind.Error } bodyType)
        {
            lambdaReturnType = bodyType;
        }

        if (collectedAsyncReturn is { TypeKind: not TypeKind.Error })
        {
            if (lambdaReturnType is null || lambdaReturnType.TypeKind == TypeKind.Error || ContainsTypeParameter(lambdaReturnType))
                lambdaReturnType = collectedAsyncReturn;
        }
        if (lambdaReturnType is not null && lambdaReturnType.TypeKind != TypeKind.Error)
        {
            if (lambda.Symbol is ILambdaSymbol { IsAsync: true })
            {
                var lambdaResult = AsyncReturnTypeUtilities.ExtractAsyncResultType(compilation, lambdaReturnType)
                    ?? lambdaReturnType;
                var expectedResult = AsyncReturnTypeUtilities.ExtractAsyncResultType(compilation, invoke.ReturnType)
                    ?? invoke.ReturnType;

                if (lambdaResult is ITypeParameterSymbol &&
                    collectedAsyncReturn is { TypeKind: not TypeKind.Error })
                {
                    var collectedAsyncResult = AsyncReturnTypeUtilities.ExtractAsyncResultType(compilation, collectedAsyncReturn)
                        ?? collectedAsyncReturn;

                    if (collectedAsyncResult is { TypeKind: not TypeKind.Error })
                        lambdaResult = collectedAsyncResult;
                }

                if (lambdaResult is ITypeParameterSymbol)
                    return true;

                if (!TryInferFromTypes(compilation, expectedResult, lambdaResult, substitutions, inferenceMethod))
                    return false;
            }
            else
            {
                if (lambdaReturnType is ITypeParameterSymbol)
                    return true;

                if (!TryInferFromTypes(compilation, invoke.ReturnType, lambdaReturnType, substitutions, inferenceMethod))
                    return false;
            }
        }

        return true;
    }

    private static bool TryInferFromTypes(
        Compilation compilation,
        ITypeSymbol parameterType,
        ITypeSymbol argumentType,
        Dictionary<ITypeParameterSymbol, ITypeSymbol> substitutions,
        IMethodSymbol? inferenceMethod)
    {
        parameterType = NormalizeType(parameterType);
        argumentType = NormalizeType(argumentType);

        if (argumentType.TypeKind == TypeKind.Error)
            return false;

        if (parameterType is ITypeParameterSymbol typeParameter)
        {
            typeParameter = GetCanonicalTypeParameter(typeParameter, inferenceMethod);
            argumentType = NormalizeType(argumentType);

            if (substitutions.TryGetValue(typeParameter, out var existing))
            {
                existing = NormalizeType(existing);

                if (SymbolEqualityComparer.Default.Equals(existing, argumentType))
                    return true;

                if (compilation.ClassifyConversion(argumentType, existing).IsImplicit)
                    return true;

                if (compilation.ClassifyConversion(existing, argumentType).IsImplicit)
                {
                    substitutions[typeParameter] = argumentType;
                    return true;
                }

                return false;
            }

            substitutions[typeParameter] = argumentType;
            return true;
        }

        if (parameterType is INamedTypeSymbol paramNamed)
        {
            if (argumentType is INamedTypeSymbol argNamed)
            {
                if (TryUnifyNamedType(paramNamed, argNamed))
                    return true;

                foreach (var iface in argNamed.AllInterfaces)
                {
                    if (TryUnifyNamedType(paramNamed, iface))
                        return true;
                }

                for (var baseType = argNamed.BaseType; baseType is not null; baseType = baseType.BaseType)
                {
                    if (TryUnifyNamedType(paramNamed, baseType))
                        return true;
                }
            }
            else if (argumentType is IArrayTypeSymbol arrayArgument)
            {
                if (paramNamed.ConstructedFrom.SpecialType is SpecialType.System_Collections_Generic_IEnumerable_T or
                    SpecialType.System_Collections_Generic_ICollection_T or
                    SpecialType.System_Collections_Generic_IList_T ||
                    IsGenericCollectionInterface(paramNamed, "IReadOnlyCollection") ||
                    IsGenericCollectionInterface(paramNamed, "IReadOnlyList"))
                {
                    return TryInferFromTypes(compilation, paramNamed.TypeArguments[0], arrayArgument.ElementType, substitutions, inferenceMethod);
                }

                foreach (var iface in arrayArgument.AllInterfaces)
                {
                    if (TryUnifyNamedType(paramNamed, iface))
                        return true;
                }
            }
        }

        if (parameterType is IArrayTypeSymbol paramArray && argumentType is IArrayTypeSymbol argArray)
            return TryInferFromTypes(compilation, paramArray.ElementType, argArray.ElementType, substitutions, inferenceMethod);

        if (parameterType is NullableTypeSymbol paramNullable && argumentType is NullableTypeSymbol argNullable)
            return TryInferFromTypes(compilation, paramNullable.UnderlyingType, argNullable.UnderlyingType, substitutions, inferenceMethod);

        return true;

        bool TryUnifyNamedType(INamedTypeSymbol parameterNamed, INamedTypeSymbol argumentNamed)
        {
            if (!SymbolEqualityComparer.Default.Equals(parameterNamed.OriginalDefinition, argumentNamed.OriginalDefinition))
                return false;

            var paramArguments = parameterNamed.TypeArguments;
            var argArguments = argumentNamed.TypeArguments;

            if (paramArguments.IsDefault)
                paramArguments = ImmutableArray<ITypeSymbol>.Empty;

            if (argArguments.IsDefault)
                argArguments = ImmutableArray<ITypeSymbol>.Empty;

            if (paramArguments.Length != argArguments.Length)
                return false;

            for (int i = 0; i < paramArguments.Length; i++)
            {
                if (!TryInferFromTypes(compilation, paramArguments[i], argArguments[i], substitutions, inferenceMethod))
                    return false;
            }

            return true;
        }
    }

    private static bool IsGenericCollectionInterface(INamedTypeSymbol parameterNamed, string interfaceName)
    {
        var definition = parameterNamed.ConstructedFrom;

        if (!string.Equals(definition.Name, interfaceName, StringComparison.Ordinal))
            return false;

        var ns = definition.ContainingNamespace?.ToDisplayString();
        return string.Equals(ns, "System.Collections.Generic", StringComparison.Ordinal);
    }

    private static bool SatisfiesMethodConstraints(
        IMethodSymbol method,
        ImmutableArray<ITypeSymbol> typeArguments)
    {
        var typeParameters = method.TypeParameters;

        if (typeParameters.Length != typeArguments.Length)
            return true;

        for (int i = 0; i < typeParameters.Length; i++)
        {
            var typeParameter = typeParameters[i];
            var typeArgument = typeArguments[i];
            var constraintKind = typeParameter.ConstraintKind;

            if ((constraintKind & TypeParameterConstraintKind.ReferenceType) != 0 &&
                !SemanticFacts.SatisfiesReferenceTypeConstraint(typeArgument))
            {
                return false;
            }

            if ((constraintKind & TypeParameterConstraintKind.ValueType) != 0 &&
                !SemanticFacts.SatisfiesValueTypeConstraint(typeArgument))
            {
                return false;
            }

            if ((constraintKind & TypeParameterConstraintKind.TypeConstraint) == 0)
                continue;

            foreach (var constraintType in typeParameter.ConstraintTypes)
            {
                if (constraintType is IErrorTypeSymbol)
                    continue;

                if (constraintType is INamedTypeSymbol namedConstraint)
                {
                    if (!SemanticFacts.SatisfiesNamedTypeConstraint(typeArgument, namedConstraint))
                        return false;

                    continue;
                }

                if (!SemanticFacts.SatisfiesTypeConstraint(typeArgument, constraintType))
                    return false;
            }
        }

        return true;
    }

    private static ITypeSymbol NormalizeType(ITypeSymbol type)
    {
        if (type is LiteralTypeSymbol literal)
            return literal.UnderlyingType;

        return type;
    }

    private static ITypeParameterSymbol GetCanonicalTypeParameter(ITypeParameterSymbol typeParameter, IMethodSymbol? inferenceMethod)
    {
        if (inferenceMethod is null)
            return typeParameter;

        if (typeParameter.ContainingSymbol is IMethodSymbol &&
            typeParameter.Ordinal >= 0 &&
            typeParameter.Ordinal < inferenceMethod.TypeParameters.Length)
        {
            return inferenceMethod.TypeParameters[typeParameter.Ordinal];
        }

        return typeParameter;
    }

    private static void AddCandidateIfMissing(ImmutableArray<IMethodSymbol>.Builder builder, IMethodSymbol candidate)
    {
        foreach (var existing in builder)
        {
            if (SymbolEqualityComparer.Default.Equals(existing, candidate))
                return;
        }

        builder.Add(candidate);
    }

    private static bool IsMoreSpecific(
        IMethodSymbol candidate,
        IMethodSymbol current,
        BoundArgument[] arguments,
        BoundExpression? receiver,
        Compilation compilation)
    {
        bool better = false;
        var candParams = candidate.Parameters;
        var currentParams = current.Parameters;

        if (arguments.Any(static a => a.Expression is BoundLambdaExpression { Symbol: ILambdaSymbol { IsAsync: true } }))
        {
            var candidateTaskDepth = GetTaskDepth(candidate.ReturnType);
            var currentTaskDepth = GetTaskDepth(current.ReturnType);

            if (candidateTaskDepth < currentTaskDepth)
                return true;

            if (currentTaskDepth < candidateTaskDepth)
                return false;
        }

        bool candidateIsExtension = candidate.IsExtensionMethod && receiver is not null;
        bool currentIsExtension = current.IsExtensionMethod && receiver is not null;

        if (candidateIsExtension != currentIsExtension)
            return !currentIsExtension;

        if (candidateIsExtension && currentIsExtension && receiver?.Type is ITypeSymbol receiverType)
        {
            var candParamType = candParams[0].Type;
            var currentParamType = currentParams[0].Type;

            var candImplicit = IsImplicitConversion(compilation, receiverType, candParamType);
            var currentImplicit = IsImplicitConversion(compilation, receiverType, currentParamType);

            if (candImplicit && !currentImplicit)
            {
                better = true;
            }
            else if (!candImplicit && currentImplicit)
            {
                return false;
            }

            var candDist = GetInheritanceDistance(GetUnderlying(receiverType), GetUnderlying(candParamType));
            var currDist = GetInheritanceDistance(GetUnderlying(receiverType), GetUnderlying(currentParamType));

            if (candDist < currDist)
                better = true;
            else if (currDist < candDist)
                return false;
        }

        if (!TryMapArguments(candParams, arguments, candidateIsExtension, out var candidateMapped))
            return false;
        if (!TryMapArguments(currentParams, arguments, currentIsExtension, out var currentMapped))
            return false;

        var candidateMap = BuildArgumentParameterMap(candidateMapped, arguments, candidateIsExtension);
        var currentMap = BuildArgumentParameterMap(currentMapped, arguments, currentIsExtension);

        for (int i = 0; i < arguments.Length; i++)
        {
            var candidateParameterIndex = candidateMap[i];
            var currentParameterIndex = currentMap[i];

            if (candidateParameterIndex < 0 || currentParameterIndex < 0)
                continue;

            var argType = arguments[i].Expression.Type;
            if (argType is null)
                continue;

            var candParamType = candParams[candidateParameterIndex].Type;
            var currentParamType = currentParams[currentParameterIndex].Type;

            if (argType.TypeKind == TypeKind.Null)
            {
                var candImplicit = IsImplicitConversion(compilation, candParamType, currentParamType);
                var currentImplicit = IsImplicitConversion(compilation, currentParamType, candParamType);

                if (candImplicit && !currentImplicit)
                {
                    better = true;
                }
                else if (!candImplicit && currentImplicit)
                {
                    return false;
                }

                continue;
            }

            var candType = GetUnderlying(candParamType);
            var currentType = GetUnderlying(currentParamType);
            var underlyingArgType = GetUnderlying(argType);

            var candDist = GetInheritanceDistance(underlyingArgType, candType);
            var currDist = GetInheritanceDistance(underlyingArgType, currentType);

            if (candDist < currDist)
                better = true;
            else if (currDist < candDist)
                return false;
        }

        return better;
    }

    private static int GetTaskDepth(ITypeSymbol? type)
    {
        if (type is null)
            return int.MaxValue;

        var depth = 0;
        var current = type;

        while (current is INamedTypeSymbol named &&
            (named.SpecialType == SpecialType.System_Threading_Tasks_Task ||
             named.OriginalDefinition.SpecialType == SpecialType.System_Threading_Tasks_Task_T))
        {
            depth++;

            if (named.SpecialType == SpecialType.System_Threading_Tasks_Task)
                break;

            current = named.TypeArguments.Length == 1 ? named.TypeArguments[0] : null;
        }

        return depth;
    }

    private static bool IsImplicitConversion(Compilation compilation, ITypeSymbol source, ITypeSymbol destination)
    {
        var conversion = compilation.ClassifyConversion(source, destination);
        return conversion.Exists && conversion.IsImplicit;
    }

    private static bool TryMatch(
        IMethodSymbol method,
        BoundArgument[] arguments,
        BoundExpression? receiver,
        bool treatAsExtension,
        Compilation compilation,
        Func<IParameterSymbol, BoundLambdaExpression, bool>? canBindLambda,
        out int score)
    {
        score = 0;
        int parameterIndex = 0;
        var parameters = method.Parameters;

        if (treatAsExtension)
        {
            if (receiver is null || receiver.Type is null)
                return false;

            if (!TryEvaluateArgument(parameters[parameterIndex], receiver, compilation, canBindLambda, ref score))
                return false;

            parameterIndex++;
        }

        if (!TryMapArguments(parameters, arguments, treatAsExtension, out var mappedArguments))
            return false;

        for (; parameterIndex < parameters.Length; parameterIndex++)
        {
            var mapped = mappedArguments[parameterIndex];
            if (mapped is null)
            {
                if (!parameters[parameterIndex].HasExplicitDefaultValue)
                    return false;

                continue;
            }

            if (!TryEvaluateArgument(parameters[parameterIndex], mapped.Value.Expression, compilation, canBindLambda, ref score))
                return false;
        }

        return true;
    }

    internal static bool TryMapArguments(
        ImmutableArray<IParameterSymbol> parameters,
        IReadOnlyList<BoundArgument> arguments,
        bool treatAsExtension,
        out BoundArgument?[] orderedArguments)
    {
        orderedArguments = new BoundArgument?[parameters.Length];

        var nextPositional = treatAsExtension ? 1 : 0;
        var maxNamedIndex = -1;
        var seenNamed = false;

        foreach (var argument in arguments)
        {
            if (argument.Name is { } name)
            {
                var parameterIndex = FindParameterIndex(parameters, name);
                if (parameterIndex < 0)
                    return false;

                if (treatAsExtension && parameterIndex == 0)
                    return false;

                if (orderedArguments[parameterIndex] is not null)
                    return false;

                orderedArguments[parameterIndex] = argument;
                seenNamed = true;
                if (parameterIndex > maxNamedIndex)
                    maxNamedIndex = parameterIndex;
                continue;
            }

            while (nextPositional < parameters.Length && orderedArguments[nextPositional] is not null)
                nextPositional++;

            if (nextPositional >= parameters.Length)
                return false;

            if (seenNamed && nextPositional <= maxNamedIndex)
                return false;

            orderedArguments[nextPositional] = argument;
            nextPositional++;
        }

        var requiredStart = treatAsExtension ? 1 : 0;
        for (var i = requiredStart; i < parameters.Length; i++)
        {
            if (orderedArguments[i] is null && !parameters[i].HasExplicitDefaultValue)
                return false;
        }

        return true;
    }

    private static int[] BuildArgumentParameterMap(
        BoundArgument?[] orderedArguments,
        BoundArgument[] originalArguments,
        bool treatAsExtension)
    {
        var map = new int[originalArguments.Length];
        Array.Fill(map, -1);

        for (int parameterIndex = treatAsExtension ? 1 : 0; parameterIndex < orderedArguments.Length; parameterIndex++)
        {
            var argument = orderedArguments[parameterIndex];
            if (argument is null)
                continue;

            for (int argumentIndex = 0; argumentIndex < originalArguments.Length; argumentIndex++)
            {
                if (ReferenceEquals(originalArguments[argumentIndex].Expression, argument.Value.Expression))
                {
                    map[argumentIndex] = parameterIndex;
                    break;
                }
            }
        }

        return map;
    }

    private static int FindParameterIndex(ImmutableArray<IParameterSymbol> parameters, string name)
    {
        for (int i = 0; i < parameters.Length; i++)
        {
            if (string.Equals(parameters[i].Name, name, StringComparison.Ordinal))
                return i;
        }

        return -1;
    }

    private static bool TryEvaluateArgument(
        IParameterSymbol parameter,
        BoundExpression argument,
        Compilation compilation,
        Func<IParameterSymbol, BoundLambdaExpression, bool>? canBindLambda,
        ref int score)
    {
        var argType = argument.Type;
        if (argType is null)
            return false;

        if (argType.SpecialType == SpecialType.System_Void)
            return false;

        if (parameter.RefKind is RefKind.Ref or RefKind.Out or RefKind.In)
        {
            if (argument is not BoundAddressOfExpression ||
                argType is not IAddressTypeSymbol addressType ||
                argType.SpecialType == SpecialType.System_Void)
            {
                return false;
            }

            var parameterType = parameter.Type;
            var referencedType = addressType.ReferencedType;

            if (parameterType is ByRefTypeSymbol paramByRef)
            {
                if (!SymbolEqualityComparer.Default.Equals(referencedType, paramByRef.ElementType))
                    return false;
            }
            else if (!SymbolEqualityComparer.Default.Equals(referencedType, parameterType))
            {
                return false;
            }

            return true;
        }

        bool lambdaCompatible = false;
        if (argument is BoundLambdaExpression lambda && parameter.Type is INamedTypeSymbol delegateType)
        {
            if (lambda.Symbol is ILambdaSymbol { IsAsync: true })
            {
                var invoke = delegateType.GetDelegateInvokeMethod();
                if (invoke is null || !IsAsyncDelegateCompatible(lambda, invoke.ReturnType, compilation))
                    return false;
            }

            if (delegateType.IsGenericType && delegateType.TypeArguments.Any(static t => t is ITypeParameterSymbol))
            {
                lambdaCompatible = true;
            }
            else if (canBindLambda is not null)
            {
                if (!canBindLambda(parameter, lambda))
                    return false;

                lambdaCompatible = true;
            }
            else if (!lambda.IsCompatibleWithDelegate(delegateType, compilation))
                return false;
        }

        if (argument is BoundAddressOfExpression)
        {
            var conversion = compilation.ClassifyConversion(argType, parameter.Type);
            if (!conversion.IsImplicit)
                return false;

            score += GetConversionScore(conversion);
            return true;
        }

        if (!lambdaCompatible)
        {
            var conversion = compilation.ClassifyConversion(argType, parameter.Type);
            if (!conversion.IsImplicit)
                return false;

            var conversionScore = GetConversionScore(conversion);

            if (parameter.Type is NullableTypeSymbol nullableParam && argType is not NullableTypeSymbol)
            {
                var liftedConversion = compilation.ClassifyConversion(argType, nullableParam.UnderlyingType);
                if (liftedConversion.Exists)
                    conversionScore = GetConversionScore(liftedConversion);

                conversionScore++;
            }

            score += conversionScore;
        }
        return true;

        static bool IsAsyncDelegateCompatible(
            BoundLambdaExpression lambda,
            ITypeSymbol delegateReturnType,
            Compilation compilation)
        {
            var lambdaAsyncReturn = AsyncReturnTypeUtilities.InferAsyncReturnType(compilation, lambda.Body);
            var lambdaAsyncResult = AsyncReturnTypeUtilities.ExtractAsyncResultType(compilation, lambdaAsyncReturn)
                ?? lambdaAsyncReturn;

            if (delegateReturnType is NullableTypeSymbol nullable)
                delegateReturnType = nullable.UnderlyingType;

            if (delegateReturnType.SpecialType == SpecialType.System_Void)
            {
                var unitType = compilation.GetSpecialType(SpecialType.System_Unit);
                return SymbolEqualityComparer.Default.Equals(lambdaAsyncResult, unitType);
            }

            return IsAsyncReturn(delegateReturnType);

            static bool IsAsyncReturn(ITypeSymbol type)
            {
                if (type.SpecialType == SpecialType.System_Threading_Tasks_Task)
                    return true;

                if (type is INamedTypeSymbol named &&
                    named.OriginalDefinition.SpecialType == SpecialType.System_Threading_Tasks_Task_T)
                {
                    return true;
                }

                return false;
            }
        }
    }

    private static ITypeSymbol GetUnderlying(ITypeSymbol type) => type switch
    {
        NullableTypeSymbol nt => nt.UnderlyingType,
        LiteralTypeSymbol lt => lt.UnderlyingType,
        IUnionTypeSymbol ut => TypeSymbolExtensionsForCodeGen.FindCommonDenominator(ut.Types) ?? type,
        _ => type,
    };

    private static int GetInheritanceDistance(ITypeSymbol? derived, ITypeSymbol baseType)
    {
        int distance = 0;
        var current = derived;
        while (current is not null)
        {
            if (SymbolEqualityComparer.Default.Equals(current, baseType))
                return distance;
            current = current.BaseType;
            distance++;
        }
        return int.MaxValue;
    }

    private static int GetConversionScore(Conversion conversion)
    {
        if (!conversion.Exists)
            return int.MaxValue; // Not applicable, shouldn't occur during scoring

        if (conversion.IsIdentity)
            return 0;

        if (conversion.IsNumeric)
            return 1;

        if (conversion.IsPointer)
            return 1;

        if (conversion.IsReference)
            return 2;

        if (conversion.IsBoxing)
            return 3;

        if (conversion.IsUserDefined)
            return 4;

        if (conversion.IsUnboxing)
            return 5;

        return 10; // fallback or unspecified conversion
    }

    private static bool HasSufficientArguments(ImmutableArray<IParameterSymbol> parameters, int providedCount)
    {
        if (providedCount > parameters.Length)
            return false;

        var required = GetRequiredParameterCount(parameters);
        return providedCount >= required;
    }

    private static int GetRequiredParameterCount(ImmutableArray<IParameterSymbol> parameters)
    {
        var required = parameters.Length;
        while (required > 0 && parameters[required - 1].HasExplicitDefaultValue)
            required--;

        return required;
    }
}

internal readonly struct OverloadResolutionResult
{
    public OverloadResolutionResult(IMethodSymbol? method)
        : this(method, ImmutableArray<IMethodSymbol>.Empty)
    {
    }

    private OverloadResolutionResult(IMethodSymbol? method, ImmutableArray<IMethodSymbol> ambiguousCandidates)
    {
        Method = method;
        AmbiguousCandidates = ambiguousCandidates;
    }

    public IMethodSymbol? Method { get; }

    public ImmutableArray<IMethodSymbol> AmbiguousCandidates { get; }

    public bool Success => Method is not null && !IsAmbiguous;

    public bool IsAmbiguous => !AmbiguousCandidates.IsDefaultOrEmpty && AmbiguousCandidates.Length > 0;

    public static OverloadResolutionResult Ambiguous(ImmutableArray<IMethodSymbol> candidates)
    {
        if (candidates.IsDefault)
            candidates = ImmutableArray<IMethodSymbol>.Empty;

        return new OverloadResolutionResult(null, candidates);
    }
}
