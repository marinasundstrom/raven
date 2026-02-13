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
        Func<IParameterSymbol, BoundLambdaExpression, bool>? canBindLambda = null,
        SyntaxNode? callSyntax = null,
        ImmutableArray<ITypeSymbol> explicitTypeArguments = default)
    {
        var logger = compilation.Options.OverloadResolutionLogger;
        List<OverloadCandidateLog>? candidateLogs = logger is null ? null : new();

        if (explicitTypeArguments.IsDefault)
            explicitTypeArguments = ImmutableArray<ITypeSymbol>.Empty;

        IMethodSymbol? bestMatch = null;
        int bestScore = int.MaxValue;
        ImmutableArray<IMethodSymbol>.Builder? ambiguous = null;
        bool bestIsExtension = false;

        foreach (var candidate in methods)
        {
            var candidateStatus = OverloadCandidateStatus.Applicable;
            int? candidateScore = null;
            IMethodSymbol? constructed = null;
            List<OverloadArgumentComparisonLog>? candidateComparisons = logger is null ? null : new();
            var method = ApplyTypeArgumentInference(
                candidate,
                receiver,
                arguments,
                compilation,
                explicitTypeArguments: !explicitTypeArguments.IsDefaultOrEmpty
                    ? explicitTypeArguments
                    : GetExplicitTypeArguments(callSyntax, compilation));
            if (method is null)
            {
                candidateStatus = OverloadCandidateStatus.TypeInferenceFailed;
                RecordCandidate(candidate, constructed, candidateStatus, candidateScore, isExtension: false, candidateComparisons);
                continue;
            }

            constructed = method;
            var parameters = method.Parameters;
            var treatAsExtension = method.IsExtensionMethod && receiver is not null;
            var providedCount = arguments.Length + (treatAsExtension ? 1 : 0);

            if (!HasSufficientArguments(parameters, providedCount))
            {
                candidateStatus = OverloadCandidateStatus.InsufficientArguments;
                RecordCandidate(candidate, constructed, candidateStatus, candidateScore, treatAsExtension, candidateComparisons);
                continue;
            }

            if (!TryMatch(method, arguments, receiver, treatAsExtension, compilation, canBindLambda, candidateComparisons, out var score))
            {
                candidateStatus = OverloadCandidateStatus.ArgumentMismatch;
                RecordCandidate(candidate, constructed, candidateStatus, candidateScore, treatAsExtension, candidateComparisons);
                continue;
            }

            candidateScore = score;
            RecordCandidate(candidate, constructed, candidateStatus, candidateScore, treatAsExtension, candidateComparisons);

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

        var ambiguousCandidates = ambiguous?.ToImmutable() ?? ImmutableArray<IMethodSymbol>.Empty;

        if (logger is not null && candidateLogs is not null)
        {
            var resolvedCandidates = MarkCandidates(candidateLogs, bestMatch, ambiguousCandidates);
            var argumentLogs = CreateArgumentLogs(arguments);

            logger.Log(new OverloadResolutionLogEntry(
                callSyntax,
                receiver?.Type,
                argumentLogs,
                resolvedCandidates,
                bestMatch,
                ambiguousCandidates));
        }

        if (ambiguous is { Count: > 0 })
            return OverloadResolutionResult.Ambiguous(ambiguous.ToImmutable());

        return new OverloadResolutionResult(bestMatch);

        void RecordCandidate(
            IMethodSymbol original,
            IMethodSymbol? constructedMethod,
            OverloadCandidateStatus status,
            int? score,
            bool isExtension,
            List<OverloadArgumentComparisonLog>? comparisons)
        {
            if (candidateLogs is null)
                return;

            candidateLogs.Add(new OverloadCandidateLog(
                original,
                constructedMethod,
                status,
                score,
                isExtension,
                IsBest: false,
                IsAmbiguous: false,
                comparisons is null
                    ? ImmutableArray<OverloadArgumentComparisonLog>.Empty
                    : comparisons.ToImmutableArray()));
        }
    }

    private static ImmutableArray<ITypeSymbol> GetExplicitTypeArguments(SyntaxNode? callSyntax, Compilation compilation)
    {
        // Fallback: Only used if the binder cannot provide explicit type arguments.
        // We only support explicit method type arguments from invocation receivers like:
        //   items.CountItems<double>(2)
        // where the member name is a GenericNameSyntax.
        if (callSyntax is not InvocationExpressionSyntax inv)
            return ImmutableArray<ITypeSymbol>.Empty;

        // InvocationExpressionSyntax.Expression can be IdentifierNameSyntax, MemberAccessExpressionSyntax, etc.
        // We only care about the right-most name being a GenericNameSyntax.
        GenericNameSyntax? genericName = null;

        if (inv.Expression is MemberAccessExpressionSyntax ma)
            genericName = ma.Name as GenericNameSyntax;
        else if (inv.Expression is IdentifierNameSyntax)
            genericName = null;
        else if (inv.Expression is GenericNameSyntax g)
            genericName = g;

        if (genericName is null)
            return ImmutableArray<ITypeSymbol>.Empty;

        // Bind the explicit type arguments using a binder-independent binder helper.
        // We can’t bind types here without a binder, so we only support predefined/identifier types
        // through the compilation’s type resolver via metadata/predefined lookup.
        // If a type arg can’t be resolved, we keep it as ErrorType to let resolution fail gracefully.
        var args = genericName.TypeArgumentList.Arguments;
        if (args.Count == 0)
            return ImmutableArray<ITypeSymbol>.Empty;

        var builder = ImmutableArray.CreateBuilder<ITypeSymbol>(args.Count);
        for (int i = 0; i < args.Count; i++)
        {
            var ts = args[i];
            var resolved = compilation.TryBindTypeSyntaxWithoutBinder(ts.Type);
            builder.Add(resolved ?? compilation.ErrorTypeSymbol);
        }

        return builder.ToImmutable();
    }

    internal static IMethodSymbol? ApplyTypeArgumentInference(
        IMethodSymbol method,
        BoundExpression? receiver,
        BoundArgument[] arguments,
        Compilation compilation,
        ImmutableArray<ITypeSymbol> explicitTypeArguments = default)
    {
        // Extension lookup may return a method already adjusted for the receiver
        // (e.g. from a constructed extension container type), but the method can
        // still have method-level type parameters (like <E>) that must be inferred.
        if (explicitTypeArguments.IsDefault)
            explicitTypeArguments = ImmutableArray<ITypeSymbol>.Empty;

        // If the method isn’t generic, nothing to do.
        if (!method.IsGenericMethod || method.TypeParameters.IsDefaultOrEmpty || method.TypeParameters.Length == 0)
            return method;

        // Already-constructed generic methods (common for extension members on generic extension containers)
        // should not go through method-level inference again.
        if (!method.TypeArguments.IsDefaultOrEmpty &&
            method.TypeArguments.Length == method.TypeParameters.Length &&
            method.TypeArguments.All(static t => t is not ITypeParameterSymbol))
        {
            return method;
        }

        var treatAsExtension = method.IsExtensionMethod && receiver is not null;

        // If explicit type args were provided, allow partial lists (Raven feature):
        // - if count == arity: construct directly
        // - if count < arity: right-align them to the last type parameters, infer the rest
        // - if count > arity: impossible
        if (!explicitTypeArguments.IsDefaultOrEmpty)
        {
            var constructed = TryConstructMethodWithExplicitAndInference(
                method,
                explicitTypeArguments,
                receiver,
                arguments,
                treatAsExtension,
                compilation);

            return constructed;
        }

        // Otherwise infer all method type parameters.
        return TryConstructMethodWithInference(method, receiver, arguments, treatAsExtension, compilation);
    }

    private static IMethodSymbol? TryConstructMethodWithExplicitAndInference(
    IMethodSymbol method,
    ImmutableArray<ITypeSymbol> explicitTypeArguments,
    BoundExpression? receiver,
    BoundArgument[] arguments,
    bool treatAsExtension,
    Compilation compilation)
    {
        if (explicitTypeArguments.IsDefaultOrEmpty)
            return TryConstructMethodWithInference(method, receiver, arguments, treatAsExtension, compilation) ?? method;

        var arity = method.TypeParameters.Length;
        if (explicitTypeArguments.Length > arity)
            return null;

        // Right-align explicit args to the last type parameters.
        var fixedArgs = new ITypeSymbol?[arity];
        var offset = arity - explicitTypeArguments.Length;
        for (int i = 0; i < explicitTypeArguments.Length; i++)
        {
            fixedArgs[offset + i] = NormalizeType(explicitTypeArguments[i]);
        }

        var substitutions = new Dictionary<ITypeParameterSymbol, ITypeSymbol>(SymbolEqualityComparer.Default);

        // Seed substitutions with the fixed args.
        for (int i = 0; i < arity; i++)
        {
            if (fixedArgs[i] is { } fixedType && fixedType.TypeKind != TypeKind.Error)
                substitutions[method.TypeParameters[i]] = fixedType;
        }

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

            if (expression is BoundMethodGroupExpression methodGroup)
            {
                if (!TryInferFromMethodGroup(compilation, parameters[parameterIndex].Type, methodGroup, substitutions, method))
                    return null;
                continue;
            }

            var argumentType = expression.Type;
            if (argumentType is null || argumentType.TypeKind == TypeKind.Error)
                continue;

            if (!TryInferFromTypes(compilation, parameters[parameterIndex].Type, argumentType, substitutions, method))
                return null;
        }

        // Produce final type arguments array: fixed args win; otherwise take inferred.
        var finalArgs = new ITypeSymbol[arity];
        for (int i = 0; i < arity; i++)
        {
            var tp = method.TypeParameters[i];

            if (fixedArgs[i] is { } fixedType)
            {
                // If inference produced something conflicting, fail.
                if (substitutions.TryGetValue(tp, out var inferred) &&
                    inferred.TypeKind != TypeKind.Error &&
                    fixedType.TypeKind != TypeKind.Error &&
                    !SymbolEqualityComparer.Default.Equals(NormalizeType(inferred), NormalizeType(fixedType)))
                {
                    // Allow implicit identity-ish compatibility by preferring the fixed type.
                    // If you later want to allow implicit conversions here, add a check.
                    return null;
                }

                finalArgs[i] = NormalizeType(fixedType);
                continue;
            }

            if (!substitutions.TryGetValue(tp, out var inferred2))
                return null;

            finalArgs[i] = NormalizeType(inferred2);
        }

        var immutableArguments = ImmutableArray.CreateRange(finalArgs);
        if (!SatisfiesMethodConstraints(method, immutableArguments))
            return null;

        return method.Construct(finalArgs);
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

            if (expression is BoundMethodGroupExpression methodGroup)
            {
                if (!TryInferFromMethodGroup(compilation, parameters[parameterIndex].Type, methodGroup, substitutions, method))
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

        static bool ContainsTypeParameter(ITypeSymbol type)
        {
            switch (type)
            {
                case ITypeParameterSymbol:
                    return true;
                case INamedTypeSymbol named when !named.TypeArguments.IsDefaultOrEmpty:
                    return named.TypeArguments.Any(ContainsTypeParameter);
                case IArrayTypeSymbol array:
                    return ContainsTypeParameter(array.ElementType);
                case RefTypeSymbol refType:
                    return ContainsTypeParameter(refType.ElementType);
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

    private static bool TryInferFromMethodGroup(
        Compilation compilation,
        ITypeSymbol parameterType,
        BoundMethodGroupExpression methodGroup,
        Dictionary<ITypeParameterSymbol, ITypeSymbol> substitutions,
        IMethodSymbol? inferenceMethod)
    {
        // Method groups only participate in inference when the target parameter is a delegate type.
        if (parameterType is not INamedTypeSymbol delegateType ||
            delegateType.TypeKind != TypeKind.Delegate)
        {
            return true;
        }

        var invoke = delegateType.GetDelegateInvokeMethod();
        if (invoke is null)
            return true;

        // Best-effort: pick the first candidate that can unify with the delegate signature
        // without producing conflicting substitutions.
        var candidates = methodGroup.Methods;
        if (candidates.IsDefaultOrEmpty)
            return false;

        foreach (var candidate in candidates)
        {
            if (candidate is null)
                continue;

            // Skip open generic method-group candidates for now.
            // (C# can sometimes infer these too, but this keeps Raven behavior predictable
            // until we add a dedicated inference pass for the group method's own type params.)
            if (candidate.IsGenericMethod && candidate.TypeParameters.Length > 0)
                continue;

            if (candidate.Parameters.Length != invoke.Parameters.Length)
                continue;

            // Backtrackable inference: try candidate against a copy, then commit.
            var temp = new Dictionary<ITypeParameterSymbol, ITypeSymbol>(substitutions, SymbolEqualityComparer.Default);

            var ok = true;

            for (int i = 0; i < invoke.Parameters.Length; i++)
            {
                var expectedParam = invoke.Parameters[i];
                var candidateParam = candidate.Parameters[i];

                // Ref-kind mismatch cannot be bridged by conversions.
                if (expectedParam.RefKind != candidateParam.RefKind)
                {
                    ok = false;
                    break;
                }

                if (!TryInferFromTypes(compilation, expectedParam.Type, candidateParam.Type, temp, inferenceMethod))
                {
                    ok = false;
                    break;
                }
            }

            if (!ok)
                continue;

            // Unify return types as well so we can infer TResult from a method group like `Compute`.
            if (!TryInferFromTypes(compilation, invoke.ReturnType, candidate.ReturnType, temp, inferenceMethod))
                continue;

            // Commit inferred substitutions.
            substitutions.Clear();
            foreach (var kv in temp)
                substitutions[kv.Key] = kv.Value;

            return true;
        }

        // No candidate could be used to infer the delegate conversion.
        return false;
    }

    private static ImmutableArray<OverloadCandidateLog> MarkCandidates(
        List<OverloadCandidateLog> candidates,
        IMethodSymbol? best,
        ImmutableArray<IMethodSymbol> ambiguous)
    {
        if (candidates.Count == 0)
            return ImmutableArray<OverloadCandidateLog>.Empty;

        var builder = ImmutableArray.CreateBuilder<OverloadCandidateLog>(candidates.Count);

        foreach (var candidate in candidates)
        {
            var constructed = candidate.ConstructedMethod ?? candidate.OriginalMethod;
            var isBest = best is not null && SymbolEqualityComparer.Default.Equals(constructed, best);
            var isAmbiguous = !ambiguous.IsDefaultOrEmpty && ambiguous.Any(a => SymbolEqualityComparer.Default.Equals(a, constructed));

            builder.Add(candidate with { IsBest = isBest, IsAmbiguous = isAmbiguous });
        }

        return builder.ToImmutable();
    }

    private static ImmutableArray<OverloadArgumentLog> CreateArgumentLogs(IReadOnlyList<BoundArgument> arguments)
    {
        if (arguments.Count == 0)
            return ImmutableArray<OverloadArgumentLog>.Empty;

        var builder = ImmutableArray.CreateBuilder<OverloadArgumentLog>(arguments.Count);

        foreach (var argument in arguments)
        {
            builder.Add(new OverloadArgumentLog(
                argument.Name,
                argument.RefKind,
                argument.Type,
                argument.Syntax));
        }

        return builder.ToImmutable();
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

            // A null literal does not provide a concrete type for type-parameter inference.
            // Keep existing substitutions (if any), but do not infer a new substitution from null.
            if (argumentType.TypeKind == TypeKind.Null)
                return true;

            if (substitutions.TryGetValue(typeParameter, out var existing))
            {
                existing = NormalizeType(existing);

                if (SymbolEqualityComparer.Default.Equals(existing, argumentType))
                    return true;

                // If the existing substitution is a fixed/explicit type arg, don’t override it.
                // Accept if the argument can convert to the fixed type.
                if (compilation.ClassifyConversion(argumentType, existing).IsImplicit)
                    return true;

                // Otherwise, if the fixed type can convert to the argument type, keep the fixed one.
                if (compilation.ClassifyConversion(existing, argumentType).IsImplicit)
                    return true;

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

            if ((constraintKind & TypeParameterConstraintKind.NotNull) != 0 &&
                !SemanticFacts.SatisfiesNotNullConstraint(typeArgument))
            {
                return false;
            }

            if ((constraintKind & TypeParameterConstraintKind.Constructor) != 0 &&
                !SemanticFacts.SatisfiesConstructorConstraint(typeArgument))
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

            // Prefer a concrete delegate type (e.g. RequestDelegate) over the catch-all
            // `System.Delegate` / `System.MulticastDelegate` overload when both are applicable.
            // IMPORTANT: Decide immediately so later heuristics (like inheritance distance)
            // do not accidentally prefer `System.Delegate`.
            if (candParamType is INamedTypeSymbol candNamed && currentParamType is INamedTypeSymbol currentNamed)
            {
                var candIsSystemDelegate = IsSystemDelegateLike(candNamed);
                var currentIsSystemDelegate = IsSystemDelegateLike(currentNamed);

                if (candIsSystemDelegate != currentIsSystemDelegate)
                {
                    // Candidate wins iff it is NOT the System.Delegate-like parameter.
                    return !candIsSystemDelegate;
                }
            }

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

            // Tie-breaker (C#-like): if one parameter type implicitly converts to the other (but not vice versa),
            // prefer the more specific type. This is essential for numeric overload resolution where inheritance
            // distance is not informative (e.g. byte -> int vs byte -> double/decimal).
            {
                // Prefer candidates that can use a standard implicit conversion from the actual argument.
                // This avoids ambiguous picks where one candidate only works through user-defined conversions.
                var candStandardFromArg = compilation.ClassifyConversion(argType, candParamType, includeUserDefined: false);
                var currentStandardFromArg = compilation.ClassifyConversion(argType, currentParamType, includeUserDefined: false);

                if (candStandardFromArg.Exists && candStandardFromArg.IsImplicit &&
                    (!currentStandardFromArg.Exists || !currentStandardFromArg.IsImplicit))
                {
                    better = true;
                    continue;
                }

                if ((!candStandardFromArg.Exists || !candStandardFromArg.IsImplicit) &&
                    currentStandardFromArg.Exists && currentStandardFromArg.IsImplicit)
                {
                    return false;
                }

                var candToCurrent = IsImplicitConversion(compilation, candParamType, currentParamType);
                var currentToCand = IsImplicitConversion(compilation, currentParamType, candParamType);

                if (candToCurrent && !currentToCand)
                {
                    better = true;
                }
                else if (!candToCurrent && currentToCand)
                {
                    return false;
                }
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

    private static void LogComparison(
        List<OverloadArgumentComparisonLog>? log,
        IParameterSymbol parameter,
        ITypeSymbol? argumentType,
        OverloadArgumentComparisonResult result,
        string? detail)
    {
        if (log is null)
            return;

        log.Add(new OverloadArgumentComparisonLog(
            parameter.Name,
            parameter.RefKind,
            parameter.Type,
            argumentType,
            result,
            detail));
    }

    private static string DescribeConversion(Conversion conversion)
    {
        if (!conversion.Exists)
            return "conversion does not exist";

        var parts = new List<string>();

        if (conversion.IsIdentity)
            parts.Add("identity");
        if (conversion.IsNumeric)
            parts.Add("numeric");
        if (conversion.IsReference)
            parts.Add("reference");
        if (conversion.IsBoxing)
            parts.Add("boxing");
        if (conversion.IsUnboxing)
            parts.Add("unboxing");
        if (conversion.IsPointer)
            parts.Add("pointer");
        if (conversion.IsDiscriminatedUnion)
            parts.Add("union");
        if (conversion.IsUserDefined)
            parts.Add("user-defined");
        if (conversion.IsAlias)
            parts.Add("alias");

        var kind = parts.Count == 0 ? "implicit" : string.Join(", ", parts);
        return conversion.IsImplicit ? kind : $"explicit {kind}";
    }

    private static bool TryMatch(
        IMethodSymbol method,
        BoundArgument[] arguments,
        BoundExpression? receiver,
        bool treatAsExtension,
        Compilation compilation,
        Func<IParameterSymbol, BoundLambdaExpression, bool>? canBindLambda,
        List<OverloadArgumentComparisonLog>? comparisonLog,
        out int score)
    {
        score = 0;
        int parameterIndex = 0;
        var parameters = method.Parameters;

        if (treatAsExtension)
        {
            if (receiver is null || receiver.Type is null)
            {
                LogComparison(comparisonLog, parameters[parameterIndex], receiver?.Type, OverloadArgumentComparisonResult.NullArgumentType, "receiver is missing or has no type");
                return false;
            }

            if (!TryEvaluateArgument(parameters[parameterIndex], receiver, compilation, canBindLambda, comparisonLog, ref score))
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
                {
                    LogComparison(comparisonLog, parameters[parameterIndex], argumentType: null, OverloadArgumentComparisonResult.MissingArgument, "no argument supplied");
                    return false;
                }

                LogComparison(comparisonLog, parameters[parameterIndex], argumentType: null, OverloadArgumentComparisonResult.DefaultValueUsed, "default parameter value used");

                continue;
            }

            if (!TryEvaluateArgument(parameters[parameterIndex], mapped.Value.Expression, compilation, canBindLambda, comparisonLog, ref score))
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
        List<OverloadArgumentComparisonLog>? comparisonLog,
        ref int score)
    {
        var argType = argument.Type;
        if (parameter.RefKind is RefKind.Ref or RefKind.Out or RefKind.In)
        {
            if (argType is null)
            {
                LogComparison(comparisonLog, parameter, argument.Type, OverloadArgumentComparisonResult.NullArgumentType, "argument type is null");
                return false;
            }

            if (argType.SpecialType == SpecialType.System_Void)
            {
                LogComparison(comparisonLog, parameter, argType, OverloadArgumentComparisonResult.VoidArgument, "argument type is void");
                return false;
            }

            if (argument is not BoundAddressOfExpression ||
                argType is not IAddressTypeSymbol addressType ||
                argType.SpecialType == SpecialType.System_Void)
            {
                LogComparison(comparisonLog, parameter, argType, OverloadArgumentComparisonResult.RefKindMismatch, "argument is not an address to match ref/out/in");
                return false;
            }

            var parameterType = parameter.Type;
            var referencedType = addressType.ReferencedType;

            var expectedByRefElementType = parameter.GetByRefElementType();
            if (!SymbolEqualityComparer.Default.Equals(referencedType, expectedByRefElementType))
            {
                LogComparison(comparisonLog, parameter, referencedType, OverloadArgumentComparisonResult.RefKindMismatch, "address type does not match parameter type");
                return false;
            }

            LogComparison(comparisonLog, parameter, referencedType, OverloadArgumentComparisonResult.Success, "address argument matches ref/out/in parameter");

            return true;
        }

        // Method group conversions must be validated against the target delegate type.
        // Without this, a method group can appear applicable for unrelated delegate types
        // (e.g. RequestDelegate) which breaks overload resolution for APIs that have both
        // RequestDelegate and System.Delegate overloads (like ASP.NET Minimal APIs).
        if (argument is BoundMethodGroupExpression methodGroup && parameter.Type is INamedTypeSymbol target)
        {
            if (target.TypeKind == TypeKind.Delegate)
            {
                if (!IsMethodGroupCompatibleWithDelegate(methodGroup, target, compilation))
                {
                    LogComparison(comparisonLog, parameter, target, OverloadArgumentComparisonResult.ConversionFailed, "method group is incompatible with delegate");
                    return false;
                }

                // Compatible method group => treat as a successful match without further conversion scoring.
                LogComparison(comparisonLog, parameter, target, OverloadArgumentComparisonResult.Success, "method group compatible with delegate");
                return true;
            }

            // Allow method groups to match System.Delegate / System.MulticastDelegate parameters.
            if (IsSystemDelegateLike(target))
            {
                LogComparison(comparisonLog, parameter, target, OverloadArgumentComparisonResult.Success, "method group accepted for System.Delegate-like parameter");
                return true;
            }
        }

        bool lambdaCompatible = false;
        if (argument is BoundLambdaExpression lambda && parameter.Type is INamedTypeSymbol delegateType)
        {
            if (delegateType.TypeKind == TypeKind.Delegate)
            {
                if (lambda.Symbol is ILambdaSymbol { IsAsync: true })
                {
                    var invoke = delegateType.GetDelegateInvokeMethod();
                    string? asyncDetail = null;
                    var asyncCompatible = invoke is not null && IsAsyncDelegateCompatible(lambda, invoke.ReturnType, compilation, out asyncDetail);

                    if (!asyncCompatible)
                    {
                        LogComparison(comparisonLog, parameter, delegateType, OverloadArgumentComparisonResult.LambdaIncompatible, asyncDetail ?? "async delegate mismatch");
                        return false;
                    }
                }

                if (delegateType.IsGenericType && delegateType.TypeArguments.Any(static t => t is ITypeParameterSymbol))
                {
                    lambdaCompatible = true;
                    LogComparison(comparisonLog, parameter, delegateType, OverloadArgumentComparisonResult.Success, "lambda retained for generic delegate binding");
                }
                else if (canBindLambda is not null)
                {
                    if (!canBindLambda(parameter, lambda))
                    {
                        LogComparison(comparisonLog, parameter, delegateType, OverloadArgumentComparisonResult.LambdaIncompatible, "lambda rejected by binder callback");
                        return false;
                    }

                    lambdaCompatible = true;
                    LogComparison(comparisonLog, parameter, delegateType, OverloadArgumentComparisonResult.Success, "lambda accepted via binder callback");
                }
                else if (!lambda.IsCompatibleWithDelegate(delegateType, compilation))
                {
                    LogComparison(comparisonLog, parameter, delegateType, OverloadArgumentComparisonResult.LambdaIncompatible, "lambda signature is incompatible with delegate");
                    return false;
                }
                else
                {
                    lambdaCompatible = true;
                    LogComparison(comparisonLog, parameter, delegateType, OverloadArgumentComparisonResult.Success, "lambda compatible with delegate");
                }

                argType = lambda.DelegateType ?? argType;
            }
            else if (IsSystemDelegateLike(delegateType))
            {
                var inferredDelegate = lambda.DelegateType as INamedTypeSymbol;
                if (inferredDelegate is null || inferredDelegate.TypeKind != TypeKind.Delegate)
                {
                    LogComparison(comparisonLog, parameter, delegateType, OverloadArgumentComparisonResult.LambdaIncompatible, "lambda does not have an inferred delegate type");
                    return false;
                }

                lambdaCompatible = true;
                argType = inferredDelegate;
                LogComparison(comparisonLog, parameter, delegateType, OverloadArgumentComparisonResult.Success, "lambda accepted for System.Delegate-like parameter");
            }
        }

        if (argType is null)
        {
            LogComparison(comparisonLog, parameter, argument.Type, OverloadArgumentComparisonResult.NullArgumentType, "argument type is null");
            return false;
        }

        if (argType.SpecialType == SpecialType.System_Void)
        {
            LogComparison(comparisonLog, parameter, argType, OverloadArgumentComparisonResult.VoidArgument, "argument type is void");
            return false;
        }

        if (argument is BoundAddressOfExpression)
        {
            var conversion = compilation.ClassifyConversion(argType, parameter.Type);
            if (!conversion.IsImplicit)
            {
                LogComparison(comparisonLog, parameter, argType, OverloadArgumentComparisonResult.ConversionFailed, DescribeConversion(conversion));
                return false;
            }

            score += GetConversionScore(conversion);
            LogComparison(comparisonLog, parameter, argType, OverloadArgumentComparisonResult.Success, DescribeConversion(conversion));
            return true;
        }

        if (!lambdaCompatible)
        {
            var conversion = compilation.ClassifyConversion(argType, parameter.Type);
            if (!conversion.IsImplicit)
            {
                LogComparison(comparisonLog, parameter, argType, OverloadArgumentComparisonResult.ConversionFailed, DescribeConversion(conversion));
                return false;
            }

            var conversionScore = GetConversionScore(conversion);

            if (parameter.Type is NullableTypeSymbol nullableParam && !Conversion.IsNullable(argType))
            {
                var liftedConversion = compilation.ClassifyConversion(argType, nullableParam.UnderlyingType);
                if (liftedConversion.Exists)
                    conversionScore = GetConversionScore(liftedConversion);

                conversionScore++;
            }

            score += conversionScore;
            LogComparison(comparisonLog, parameter, argType, OverloadArgumentComparisonResult.Success, DescribeConversion(conversion));
        }
        else
        {
            LogComparison(comparisonLog, parameter, argType, OverloadArgumentComparisonResult.Success, "lambda compatibility accepted");
        }
        return true;

        static bool IsAsyncDelegateCompatible(
            BoundLambdaExpression lambda,
            ITypeSymbol delegateReturnType,
            Compilation compilation,
            out string? failureDetail)
        {
            failureDetail = null;
            var lambdaAsyncReturn = AsyncReturnTypeUtilities.InferAsyncReturnType(compilation, lambda.Body);
            var lambdaAsyncResult = AsyncReturnTypeUtilities.ExtractAsyncResultType(compilation, lambdaAsyncReturn)
                ?? lambdaAsyncReturn;

            delegateReturnType = delegateReturnType.GetPlainType();

            if (delegateReturnType.SpecialType == SpecialType.System_Void)
            {
                var unitType = compilation.GetSpecialType(SpecialType.System_Unit);
                var compatible = SymbolEqualityComparer.Default.Equals(lambdaAsyncResult, unitType);
                if (!compatible)
                    failureDetail = $"async lambda result {lambdaAsyncResult?.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)} does not map to void";

                return compatible;
            }

            var result = IsAsyncReturn(delegateReturnType);
            if (!result)
                failureDetail = $"delegate return {delegateReturnType.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)} is not task-shaped";

            return result;

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

    private static bool IsMethodGroupCompatibleWithDelegate(
        BoundMethodGroupExpression methodGroup,
        INamedTypeSymbol delegateType,
        Compilation compilation)
    {
        if (delegateType.TypeKind != TypeKind.Delegate)
            return false;

        var invoke = delegateType.GetDelegateInvokeMethod();
        if (invoke is null)
            return false;

        var candidates = methodGroup.Methods;
        if (candidates.IsDefaultOrEmpty)
            return false;

        foreach (var candidate in candidates)
        {
            if (candidate is null)
                continue;

            // Skip open generic candidates; they are not directly convertible without type inference.
            if (candidate.IsGenericMethod && candidate.TypeParameters.Length > 0)
                continue;

            // Delegate conversion requires the method to be invokable with the delegate's parameter list.
            // For now we require equal parameter counts (no optional/params-array bridging).
            if (candidate.Parameters.Length != invoke.Parameters.Length)
                continue;

            var ok = true;

            for (int i = 0; i < invoke.Parameters.Length; i++)
            {
                var invokeParam = invoke.Parameters[i];
                var methodParam = candidate.Parameters[i];

                if (invokeParam.RefKind != methodParam.RefKind)
                {
                    ok = false;
                    break;
                }

                // Delegate parameters are contravariant: the delegate-provided value must be
                // implicitly convertible to the method parameter type.
                var conv = compilation.ClassifyConversion(invokeParam.Type, methodParam.Type);
                if (!conv.Exists || !conv.IsImplicit)
                {
                    ok = false;
                    break;
                }
            }

            if (!ok)
                continue;

            // Returns are covariant: the method return must be implicitly convertible to the delegate return.
            var delegateReturn = invoke.ReturnType;
            var methodReturn = candidate.ReturnType;

            if (delegateReturn.SpecialType == SpecialType.System_Void)
            {
                // Accept a Unit-returning method for a void-returning delegate.
                if (methodReturn.SpecialType != SpecialType.System_Void &&
                    methodReturn.SpecialType != SpecialType.System_Unit)
                {
                    continue;
                }

                return true;
            }

            var retConv = compilation.ClassifyConversion(methodReturn, delegateReturn);
            if (!retConv.Exists || !retConv.IsImplicit)
                continue;

            return true;
        }

        return false;
    }

    private static bool IsSystemDelegateLike(INamedTypeSymbol type)
    {
        var definition = type.OriginalDefinition ?? type;
        if (definition.ContainingNamespace is null)
            return false;
        if (!string.Equals(definition.ContainingNamespace.ToDisplayString(), "System", StringComparison.Ordinal))
            return false;

        return string.Equals(definition.MetadataName, "Delegate", StringComparison.Ordinal) ||
               string.Equals(definition.MetadataName, "MulticastDelegate", StringComparison.Ordinal);
    }

    private static ITypeSymbol GetUnderlying(ITypeSymbol type) => type switch
    {
        NullableTypeSymbol nt => nt.UnderlyingType,
        LiteralTypeSymbol lt => lt.UnderlyingType,
        ITypeUnionSymbol ut => TypeSymbolExtensionsForCodeGen.FindCommonDenominator(ut.Types) ?? type,
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
