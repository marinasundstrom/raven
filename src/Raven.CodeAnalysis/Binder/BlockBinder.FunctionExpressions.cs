using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

partial class BlockBinder
{
    private BoundExpression BindLambdaExpression(FunctionExpressionSyntax syntax)
    {
        var parameterSyntaxes = syntax switch
        {
            SimpleFunctionExpressionSyntax s => new[] { s.Parameter },
            ParenthesizedFunctionExpressionSyntax p => p.ParameterList.Parameters.ToArray(),
            _ => throw new NotSupportedException("Unknown lambda syntax")
        };

        var hasAnyExplicitParameterType = parameterSyntaxes.Any(p => p.TypeAnnotation?.Type is not null);

        SyntaxToken? asyncKeywordToken = syntax switch
        {
            SimpleFunctionExpressionSyntax simple => simple.AsyncKeyword,
            ParenthesizedFunctionExpressionSyntax parenthesized => parenthesized.AsyncKeyword,
            _ => null
        };
        SyntaxToken? staticKeywordToken = syntax switch
        {
            SimpleFunctionExpressionSyntax simple => simple.StaticKeyword,
            ParenthesizedFunctionExpressionSyntax parenthesized => parenthesized.StaticKeyword,
            _ => null
        };

        var isAsyncLambda = asyncKeywordToken.HasValue &&
            asyncKeywordToken.Value.Kind == SyntaxKind.AsyncKeyword &&
            !asyncKeywordToken.Value.IsMissing;
        var isStaticLambda = staticKeywordToken.HasValue &&
            staticKeywordToken.Value.Kind == SyntaxKind.StaticKeyword &&
            !staticKeywordToken.Value.IsMissing;

        var targetType = GetTargetType(syntax);
        if (targetType is NullableTypeSymbol nullableTargetType)
            targetType = nullableTargetType.UnderlyingType;

        // Minimal APIs have overloads that accept `RequestDelegate` and `Delegate`.
        // If the lambda has explicit parameter type annotations, do not let the argument-binding
        // phase force the lambda into `RequestDelegate` (which would rewrite parameters to HttpContext
        // and hide real diagnostics like unknown identifiers inside the body).
        // Leave it unshaped here; overload resolution will still be able to pick the correct overload.
        if (hasAnyExplicitParameterType &&
            targetType is INamedTypeSymbol tNamed &&
            tNamed.TypeKind == TypeKind.Delegate &&
            tNamed.Name == "RequestDelegate" &&
            string.Equals(tNamed.ContainingNamespace?.ToDisplayString(), "Microsoft.AspNetCore.Http", StringComparison.Ordinal))
        {
            targetType = null;
        }

        var candidateDelegates = GetLambdaDelegateTargets(syntax);
        if (candidateDelegates.IsDefaultOrEmpty)
            candidateDelegates = ComputeLambdaDelegateTargets(syntax);

        // If the target context is `System.Delegate` / `System.MulticastDelegate` (common when an API
        // accepts an untyped handler), do NOT force the lambda to match any cached candidate delegate
        // (like RequestDelegate). We only want those candidates for optional parameter-type inference,
        // not for shaping the lambda's return type.
        if (targetType is INamedTypeSymbol namedTarget &&
            namedTarget.ContainingNamespace?.ToDisplayString() == "System" &&
            (namedTarget.Name == "Delegate" || namedTarget.Name == "MulticastDelegate"))
        {
            candidateDelegates = ImmutableArray<INamedTypeSymbol>.Empty;
        }

        var suppressedDiagnostics = ImmutableArray.CreateBuilder<SuppressedLambdaDiagnostic>();

        INamedTypeSymbol? targetDelegate = targetType as INamedTypeSymbol;
        if (targetDelegate?.TypeKind != TypeKind.Delegate)
            targetDelegate = null;

        // Stage 1 expression trees: for Expression<TDelegate> targets, infer lambda shape
        // from the inner delegate the same way we do for direct delegate targets.
        if (targetDelegate is null &&
            TryGetLambdaTargetDelegateFromContext(syntax, targetType, out var contextDelegate))
        {
            targetDelegate = contextDelegate;
        }

        if (targetDelegate is not null && candidateDelegates.IsDefault)
        {
            candidateDelegates = ImmutableArray.Create(targetDelegate);
        }
        else if (targetDelegate is not null && !candidateDelegates.Contains(targetDelegate, SymbolEqualityComparer.Default))
        {
            candidateDelegates = candidateDelegates.Add(targetDelegate);
        }

        if (candidateDelegates.IsDefault)
            candidateDelegates = ImmutableArray<INamedTypeSymbol>.Empty;

        INamedTypeSymbol? primaryDelegate = targetDelegate;
        var targetSignature = primaryDelegate?.GetDelegateInvokeMethod();

        TypeParameterListSyntax? lambdaTypeParameterList = syntax switch
        {
            ParenthesizedFunctionExpressionSyntax p => p.TypeParameterList,
            _ => null
        };

        var lambdaConstraintClauses = syntax switch
        {
            ParenthesizedFunctionExpressionSyntax p => p.ConstraintClauses,
            _ => SyntaxList<TypeParameterConstraintClauseSyntax>.Empty
        };

        var initialReturnType = Compilation.ErrorTypeSymbol;
        var lambdaSymbol = new SourceLambdaSymbol(
            parameters: [],
            initialReturnType,
            _containingSymbol,
            _containingSymbol.ContainingType as INamedTypeSymbol,
            _containingSymbol.ContainingNamespace,
            [syntax.GetLocation()],
            [syntax.GetReference()],
            isAsync: isAsyncLambda);
        var lambdaBinder = new FunctionExpressionBinder(lambdaSymbol, this);

        if (syntax is ParenthesizedFunctionExpressionSyntax
            {
                Identifier: { Kind: not SyntaxKind.None } identifierToken
            })
        {
            lambdaBinder.DeclareFunction(identifierToken.ValueText, lambdaSymbol);
        }

        var hasScopedFunctionName = false;
        string? scopedFunctionName = null;
        var hadPreviousScopedFunction = false;
        IMethodSymbol? previousScopedFunction = null;

        if (syntax is ParenthesizedFunctionExpressionSyntax
            {
                Identifier: { Kind: not SyntaxKind.None } scopedIdentifierToken
            })
        {
            hasScopedFunctionName = true;
            scopedFunctionName = scopedIdentifierToken.ValueText;
            hadPreviousScopedFunction = _functions.TryGetValue(scopedFunctionName, out previousScopedFunction);
            _functions[scopedFunctionName] = lambdaSymbol;
        }

        if (lambdaTypeParameterList is not null && lambdaTypeParameterList.Parameters.Count > 0)
        {
            var containingType = _containingSymbol.ContainingType as INamedTypeSymbol
                ?? Compilation.GetSpecialType(SpecialType.System_Object) as INamedTypeSymbol;

            if (containingType is not null)
            {
                TypeParameterInitializer.InitializeLambdaTypeParameters(
                    lambdaSymbol,
                    containingType,
                    lambdaTypeParameterList,
                    lambdaConstraintClauses,
                    syntax.SyntaxTree,
                    _diagnostics);

                lambdaBinder.EnsureTypeParameterConstraintTypesResolved(lambdaSymbol.TypeParameters);
            }
        }

        var parameterSymbols = new List<IParameterSymbol>();
        var seenOptionalParameter = false;
        for (int index = 0; index < parameterSyntaxes.Length; index++)
        {
            var parameterSyntax = parameterSyntaxes[index];
            var annotation = parameterSyntax.TypeAnnotation;
            var typeSyntax = annotation?.Type;
            var refKind = RefKind.None;
            var refKindTokenKind = parameterSyntax.RefKindKeyword.Kind;

            if (typeSyntax is ByRefTypeSyntax &&
                refKindTokenKind is SyntaxKind.RefKeyword or SyntaxKind.OutKeyword or SyntaxKind.InKeyword)
            {
                _diagnostics.ReportParameterModifierCannotBeCombinedWithByRefType(
                    GetLambdaParameterNameForDiagnostics(parameterSyntax, index),
                    parameterSyntax.RefKindKeyword.Text,
                    typeSyntax.GetLocation());
            }

            if (typeSyntax is ByRefTypeSyntax)
            {
                refKind = refKindTokenKind switch
                {
                    SyntaxKind.OutKeyword => RefKind.Out,
                    SyntaxKind.InKeyword => RefKind.In,
                    SyntaxKind.RefKeyword => RefKind.Ref,
                    _ => RefKind.Ref,
                };
            }

            var targetParam = targetSignature is { } invoke && invoke.Parameters.Length > index
                ? invoke.Parameters[index]
                : null;

            ITypeSymbol parameterType;
            if (typeSyntax is not null)
            {
                var boundTypeSyntax = refKind.IsByRef && typeSyntax is ByRefTypeSyntax byRefType
                    ? byRefType.ElementType
                    : typeSyntax;
                parameterType = lambdaBinder.ResolveTypeSyntaxOrError(boundTypeSyntax);
            }
            else if (targetParam is not null)
            {
                parameterType = targetParam.Type;
                if (parameterType is ITypeParameterSymbol targetTypeParameter)
                    lambdaBinder.EnsureTypeParameterConstraintTypesResolved(ImmutableArray.Create(targetTypeParameter));
                if (refKind == RefKind.None)
                    refKind = targetParam.RefKind;
            }
            else
            {
                parameterType = TryInferLambdaParameterType(candidateDelegates, index, out var inferredRefKind)
                    ?? Compilation.ErrorTypeSymbol;

                if (refKind == RefKind.None && inferredRefKind is not null)
                    refKind = inferredRefKind.Value;

                if (parameterType.TypeKind == TypeKind.Error)
                {
                    var diagnosticParameterName = GetLambdaParameterNameForDiagnostics(parameterSyntax, index);
                    var diagnosticParameterLocation = GetLambdaParameterLocation(parameterSyntax);

                    if (candidateDelegates.IsDefaultOrEmpty)
                    {
                        _diagnostics.ReportLambdaParameterTypeCannotBeInferred(
                            diagnosticParameterName,
                            diagnosticParameterLocation);
                    }
                    else
                    {
                        suppressedDiagnostics.Add(new SuppressedLambdaDiagnostic(diagnosticParameterName, diagnosticParameterLocation));
                    }
                }
            }

            var isMutable = refKind is RefKind.Ref or RefKind.Out;

            var hasExplicitDefaultValue = false;
            object explicitDefaultValue = null;

            // Lambda parameter types can be unresolved here; preserve syntactic constants anyway.
            if (parameterSyntax?.DefaultValue?.Value is not null)
            {
                if (ConstantValueEvaluator.TryEvaluate(parameterSyntax.DefaultValue.Value, out var evaluated))
                {
                    hasExplicitDefaultValue = true;
                    explicitDefaultValue = evaluated;
                    seenOptionalParameter = true;
                }
            }

            var parameterName = GetLambdaParameterSymbolName(parameterSyntax, index);
            var parameterLocation = GetLambdaParameterLocation(parameterSyntax);

            var symbol = new SourceParameterSymbol(
                parameterName,
                parameterType,
                lambdaSymbol,
                lambdaSymbol.ContainingType,
                lambdaSymbol.ContainingNamespace,
                [parameterLocation],
                [parameterSyntax.GetReference()],
                refKind,
                hasExplicitDefaultValue,
                explicitDefaultValue,
                isMutable: isMutable
            );

            parameterSymbols.Add(symbol);
        }

        lambdaSymbol.SetParameters(parameterSymbols);

        TypeSyntax? returnTypeSyntax = syntax switch
        {
            SimpleFunctionExpressionSyntax s => s.ReturnType?.Type,
            ParenthesizedFunctionExpressionSyntax p => p.ReturnType?.Type,
            _ => null
        };

        ITypeSymbol? annotatedReturnType = returnTypeSyntax is not null
            ? lambdaBinder.ResolveTypeSyntaxOrError(returnTypeSyntax)
            : null;

        var hasInvalidAsyncReturnType = false;

        if (isAsyncLambda && returnTypeSyntax is not null &&
            annotatedReturnType is not null && !IsValidAsyncReturnType(annotatedReturnType))
        {
            var display = annotatedReturnType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);
            var suggestedReturnType = AsyncReturnTypeUtilities.GetSuggestedAsyncReturnTypeDisplay(Compilation, annotatedReturnType);
            _diagnostics.ReportAsyncReturnTypeMustBeTaskLike(display, suggestedReturnType, returnTypeSyntax.GetLocation());
            annotatedReturnType = Compilation.GetSpecialType(SpecialType.System_Threading_Tasks_Task);
            hasInvalidAsyncReturnType = true;
        }

        initialReturnType = annotatedReturnType ?? Compilation.ErrorTypeSymbol;
        lambdaSymbol.SetReturnType(initialReturnType);

        if (hasInvalidAsyncReturnType)
            lambdaSymbol.MarkAsyncReturnTypeError();

        foreach (var param in parameterSymbols)
            lambdaBinder.DeclareParameter(param);

        var destructuringPrologue = BindLambdaDestructuringPrologue(lambdaBinder, parameterSyntaxes, parameterSymbols);

        ITypeSymbol? lambdaBodyTargetType = targetSignature?.ReturnType;
        if (isAsyncLambda && lambdaBodyTargetType is not null)
            lambdaBodyTargetType = AsyncReturnTypeUtilities.ExtractAsyncResultType(Compilation, lambdaBodyTargetType) ?? lambdaBodyTargetType;

        var lambdaBodySyntax = GetLambdaBodyExpression(syntax);
        var lambdaBodySyntaxNode = GetLambdaBodySyntaxNode(syntax);

        BoundExpression bodyExpr;
        try
        {
            if (lambdaBodyTargetType is not null)
            {
                using var _ = lambdaBinder.PushTargetType(lambdaBodyTargetType);
                bodyExpr = lambdaBinder.BindExpression(lambdaBodySyntax, allowReturn: true);
            }
            else
            {
                bodyExpr = lambdaBinder.BindExpression(lambdaBodySyntax, allowReturn: true);
            }

            if (lambdaBodyTargetType is not null)
                bodyExpr = RetargetLambdaBodyUnionCase(bodyExpr, lambdaBodyTargetType);

            ReportLambdaBodyDiagnostics(lambdaBinder);
        }
        finally
        {
            if (hasScopedFunctionName && scopedFunctionName is not null)
            {
                if (hadPreviousScopedFunction && previousScopedFunction is not null)
                    _functions[scopedFunctionName] = previousScopedFunction;
                else
                    _functions.Remove(scopedFunctionName);
            }
        }

        if (destructuringPrologue.Count > 0)
        {
            destructuringPrologue.Add(new BoundExpressionStatement(bodyExpr));
            bodyExpr = new BoundBlockExpression(destructuringPrologue, Compilation.GetSpecialType(SpecialType.System_Unit));
        }

        var inferred = bodyExpr.Type;
        var collectedReturn = ReturnTypeCollector.Infer(bodyExpr);
        var collectedAsyncReturn = isAsyncLambda
            ? AsyncReturnTypeUtilities.InferAsyncReturnType(Compilation, collectedReturn)
            : null;

        if (returnTypeSyntax is null &&
            inferred is not null &&
            inferred.TypeKind != TypeKind.Error)
        {
            inferred = TypeSymbolNormalization.NormalizeForInference(inferred);
        }
        var unitType = Compilation.GetSpecialType(SpecialType.System_Unit);

        if (inferred is null || SymbolEqualityComparer.Default.Equals(inferred, unitType))
        {
            if (collectedReturn is not null)
                inferred = collectedReturn;
        }

        if (isAsyncLambda && inferred is ITypeUnionSymbol union)
        {
            var unionTypes = union.Types.ToImmutableArray();
            if (!unionTypes.IsDefaultOrEmpty)
            {
                var nonUnitTypes = unionTypes
                    .Where(t => !SymbolEqualityComparer.Default.Equals(t, unitType))
                    .ToImmutableArray();

                if (!nonUnitTypes.IsDefaultOrEmpty)
                {
                    inferred = nonUnitTypes.Length == 1
                        ? nonUnitTypes[0]
                        : TypeSymbolNormalization.NormalizeUnion(nonUnitTypes);
                }
            }
        }

        var inferredAsyncReturnInput = inferred;

        if (isAsyncLambda)
        {
            if (collectedReturn is { TypeKind: not TypeKind.Error })
            {
                inferredAsyncReturnInput = collectedReturn;
            }
            else if (inferredAsyncReturnInput is null ||
                     inferredAsyncReturnInput.TypeKind == TypeKind.Error ||
                     SymbolEqualityComparer.Default.Equals(inferredAsyncReturnInput, unitType))
            {
                if (collectedReturn is not null)
                    inferredAsyncReturnInput = collectedReturn;
            }
        }

        var inferredAsyncReturn = isAsyncLambda
            ? collectedAsyncReturn ?? AsyncReturnTypeUtilities.InferAsyncReturnType(Compilation, inferredAsyncReturnInput)
            : null;

        var inferredAsyncResult = inferredAsyncReturn is not null
            ? AsyncReturnTypeUtilities.ExtractAsyncResultType(Compilation, inferredAsyncReturn)
            : null;

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

        INamedTypeSymbol? SelectBestDelegate(ITypeSymbol? inferredReturn)
        {
            if (isAsyncLambda)
            {
                var taskGeneric = candidateDelegates.FirstOrDefault(candidate =>
                {
                    var invoke = candidate.GetDelegateInvokeMethod();
                    var candidateReturn = invoke?.ReturnType;

                    if (candidateReturn is NullableTypeSymbol nullable)
                        candidateReturn = nullable.UnderlyingType;

                    return candidateReturn is INamedTypeSymbol named &&
                        named.OriginalDefinition.SpecialType == SpecialType.System_Threading_Tasks_Task_T;
                });

                if (taskGeneric is not null)
                    return taskGeneric;
            }

            var asyncResult = isAsyncLambda
                ? inferredAsyncResult ?? inferred
                : null;

            var returnForSelection = inferredReturn ?? asyncResult;

            if (primaryDelegate is null)
                return null;

            if (candidateDelegates.IsDefaultOrEmpty)
                return primaryDelegate;

            if (returnForSelection is null || returnForSelection.TypeKind == TypeKind.Error)
                return primaryDelegate;

            if (returnForSelection is null || returnForSelection.TypeKind == TypeKind.Error)
                return primaryDelegate ?? candidateDelegates.FirstOrDefault();

            bool IsAsyncDelegateReturn(ITypeSymbol? candidateReturn)
            {
                if (!isAsyncLambda || candidateReturn is null)
                    return false;

                if (candidateReturn is NullableTypeSymbol nullable)
                    candidateReturn = nullable.UnderlyingType;

                return IsValidAsyncReturnType(candidateReturn);
            }

            bool IsConvertibleToExpected(ITypeSymbol expectedBody)
            {
                var conversion = Compilation.ClassifyConversion(
                    returnForSelection,
                    expectedBody);

                return conversion.Exists && conversion.IsImplicit;
            }

            INamedTypeSymbol? asyncMatch = null;
            INamedTypeSymbol? syncMatch = null;
            INamedTypeSymbol? bestAsyncByResult = null;
            Conversion bestAsyncConversion = default;

            foreach (var candidate in candidateDelegates)
            {
                var invoke = candidate.GetDelegateInvokeMethod();
                if (invoke is null)
                    continue;

                var candidateReturn = invoke.ReturnType;
                if (isAsyncLambda &&
                    candidateReturn.SpecialType != SpecialType.System_Void &&
                    !IsAsyncDelegateReturn(candidateReturn))
                {
                    continue;
                }

                var expectedBody = isAsyncLambda
                    ? AsyncReturnTypeUtilities.ExtractAsyncResultType(Compilation, candidateReturn) ?? candidateReturn
                    : candidateReturn;

                if (expectedBody is null || expectedBody.TypeKind == TypeKind.Error)
                    continue;

                if (asyncResult is { TypeKind: not TypeKind.Error })
                {
                    var conversion = Compilation.ClassifyConversion(asyncResult, expectedBody);
                    if (conversion.Exists && conversion.IsImplicit)
                    {
                        var prefer = bestAsyncByResult is null ||
                                     (!bestAsyncConversion.IsIdentity && conversion.IsIdentity);

                        if (prefer)
                        {
                            bestAsyncByResult = candidate;
                            bestAsyncConversion = conversion;
                        }
                    }
                }

                if (expectedBody is ITypeParameterSymbol)
                {
                    // Keep generic async delegates (e.g., Task<T> Run(Func<Task<T>?>)) eligible even
                    // when the expected body is a type parameter. If we already inferred an async
                    // result, prefer this candidate up front instead of discarding it due to the
                    // unconstrained type parameter comparison.
                    if (asyncResult is { TypeKind: not TypeKind.Error })
                    {
                        bestAsyncByResult ??= candidate;
                        asyncMatch ??= candidate;
                    }
                    else if (IsAsyncDelegateReturn(candidateReturn))
                    {
                        asyncMatch ??= candidate;
                    }
                    else
                    {
                        syncMatch ??= candidate;
                    }

                    continue;
                }

                if (!IsConvertibleToExpected(expectedBody))
                    continue;

                if (IsAsyncDelegateReturn(candidateReturn))
                    asyncMatch ??= candidate;
                else
                    syncMatch ??= candidate;
            }

            if (bestAsyncByResult is not null)
                return bestAsyncByResult;

            if (asyncMatch is not null)
                return asyncMatch;

            if (syncMatch is not null)
                return syncMatch;

            return primaryDelegate;
        }

        var delegateSelectionType = isAsyncLambda
            ? inferredAsyncResult ?? inferred
            : inferred;

        primaryDelegate = SelectBestDelegate(delegateSelectionType);
        targetSignature = primaryDelegate?.GetDelegateInvokeMethod();

        var targetReturn = targetSignature?.ReturnType;
        if (targetReturn is not null && targetReturn.TypeKind == TypeKind.Error)
            targetReturn = null;

        ITypeSymbol returnType;
        bool IsAsyncReturnCompatibleWithLambda(ITypeSymbol? candidateReturn)
        {
            if (!isAsyncLambda || candidateReturn is null)
                return false;

            if (candidateReturn.SpecialType == SpecialType.System_Void)
                return SymbolEqualityComparer.Default.Equals(inferredAsyncResult, unitType);

            if (candidateReturn is NullableTypeSymbol nullable)
                candidateReturn = nullable.UnderlyingType;

            return IsValidAsyncReturnType(candidateReturn);
        }

        if (annotatedReturnType is { TypeKind: not TypeKind.Error })
        {
            returnType = annotatedReturnType;
        }
        else if (isAsyncLambda)
        {
            if (inferredAsyncReturn is { TypeKind: not TypeKind.Error })
            {
                returnType = inferredAsyncReturn;
            }
            else if (targetReturn is not null && ContainsTypeParameter(targetReturn))
            {
                // Avoid flowing type-parameterized delegate returns into the lambda's
                // return type when we already have async inference inputs. Doing so
                // keeps the lambda shaped like its async body (e.g., Task<int>)
                // instead of inheriting Task<T> and re-wrapping to Task<Task<T>>
                // during replay.
                var inferredAsync = AsyncReturnTypeUtilities.InferAsyncReturnType(
                    Compilation,
                    inferredAsyncReturnInput ?? inferred ?? collectedReturn ?? targetReturn);

                returnType = inferredAsync;
            }
            else if (IsAsyncReturnCompatibleWithLambda(targetReturn))
            {
                returnType = targetReturn!;
            }
            else
            {
                returnType = Compilation.ErrorTypeSymbol;
            }
        }
        else if (targetReturn is not null)
        {
            returnType = ContainsTypeParameter(targetReturn)
                ? inferred ?? targetReturn
                : targetReturn;
        }
        else
        {
            returnType = inferred ?? Compilation.ErrorTypeSymbol;
        }

        ITypeSymbol? expectedBodyType = returnType;
        if (isAsyncLambda)
        {
            expectedBodyType = AsyncReturnTypeUtilities.ExtractAsyncResultType(Compilation, returnType) ?? expectedBodyType;
        }

        if (expectedBodyType is not null &&
            expectedBodyType is not ITypeParameterSymbol &&
            inferred is not null &&
            inferred.TypeKind != TypeKind.Error &&
            expectedBodyType.TypeKind != TypeKind.Error &&
            !hasInvalidAsyncReturnType)
        {
            if (!IsAssignable(expectedBodyType, inferred, out var conversion))
            {
                ReportCannotConvertFromTypeToType(
                    inferred.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    expectedBodyType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    lambdaBodySyntaxNode.GetLocation());
            }
            else
            {
                bodyExpr = ApplyConversion(bodyExpr, expectedBodyType, conversion, lambdaBodySyntax);
            }
        }

        lambdaBinder.SetLambdaBody(bodyExpr);

        var capturedVariables = lambdaBinder.AnalyzeCapturedVariables();

        if (isStaticLambda)
        {
            foreach (var captured in capturedVariables)
            {
                var location = captured.Locations.FirstOrDefault() ?? syntax.GetLocation();
                _diagnostics.ReportStaticFunctionExpressionCannotCapture(captured.Name, location);
            }
        }

        if (lambdaSymbol is SourceLambdaSymbol sourceLambdaSymbol)
        {
            sourceLambdaSymbol.SetCapturedVariables(capturedVariables);
            if (capturedVariables.Count != 0 && sourceLambdaSymbol.ClosureFrameType is null)
                sourceLambdaSymbol.SetClosureFrameType(ClosureFrameSymbolFactory.Create(sourceLambdaSymbol));
        }

        var lambdaHasBodyErrors = HasExpressionErrors(bodyExpr);

        var delegateType = primaryDelegate is not null &&
            targetSignature is not null &&
            targetSignature.Parameters.Length == parameterSymbols.Count &&
            (hasInvalidAsyncReturnType || SymbolEqualityComparer.Default.Equals(returnType, targetSignature.ReturnType)) &&
            parameterSymbols
                .Zip(targetSignature.Parameters, (parameter, target) =>
                    SymbolEqualityComparer.Default.Equals(parameter.Type, target.Type) && parameter.RefKind == target.RefKind)
                .All(match => match)
            ? primaryDelegate
            : Compilation.CreateFunctionTypeSymbol(
                parameterSymbols.Select(p => p.Type).ToArray(),
                returnType
            );

        if (!lambdaHasBodyErrors &&
            targetDelegate is not null &&
            targetSignature is not null &&
            targetSignature.Parameters.Length != parameterSymbols.Count)
        {
            ReportCannotConvertFromTypeToType(
                delegateType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                targetDelegate.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                syntax.GetLocation());
        }

        if (lambdaSymbol is SourceLambdaSymbol mutable)
        {
            mutable.SetReturnType(returnType);
            mutable.SetDelegateType(delegateType);
        }

        if (isAsyncLambda && lambdaSymbol is SourceLambdaSymbol asyncLambda)
        {
            var containsAwait = AsyncLowerer.ContainsAwait(bodyExpr) ||
                Compilation.ContainsAwaitExpressionOutsideNestedFunctions(lambdaBodySyntaxNode);
            asyncLambda.SetContainsAwait(containsAwait);

            if (!containsAwait)
            {
                var description = AsyncDiagnosticUtilities.GetAsyncMemberDescription(asyncLambda);
                var location = asyncKeywordToken?.GetLocation() ?? syntax.GetLocation();
                _diagnostics.ReportAsyncLacksAwait(description, location);
            }
        }

        var boundLambda = new BoundFunctionExpression(parameterSymbols, returnType, bodyExpr, lambdaSymbol, delegateType, capturedVariables, candidateDelegates);

        var suppressed = suppressedDiagnostics.ToImmutable();
        if (lambdaSymbol is SourceLambdaSymbol source)
        {
            var parameters = parameterSymbols.ToImmutableArray();
            var unbound = new BoundUnboundFunctionExpression(source, syntax, parameters, candidateDelegates, suppressed);
            boundLambda.AttachUnbound(unbound);
        }

        // This bind can be provisional while overload resolution is still exploring delegate candidates.
        // Avoid leaking stale body-node bindings (e.g. x: Error) into the semantic cache; selected replay
        // will rebind and repopulate body nodes with the final delegate shape.
        var isProvisionalCandidateBind =
            syntax.Parent is ArgumentSyntax &&
            !candidateDelegates.IsDefaultOrEmpty &&
            candidateDelegates.Length > 1;

        if (!isProvisionalCandidateBind)
            CacheBoundNode(syntax, boundLambda);

        if (isProvisionalCandidateBind)
            ClearCachedLambdaBodyNodes(lambdaBodySyntaxNode);

        return boundLambda;
    }

    private bool TryGetLambdaTargetDelegateFromContext(
        FunctionExpressionSyntax syntax,
        ITypeSymbol? contextualTargetType,
        out INamedTypeSymbol delegateType)
    {
        delegateType = null!;

        if (contextualTargetType is INamedTypeSymbol directDelegate &&
            directDelegate.TypeKind == TypeKind.Delegate)
        {
            delegateType = directDelegate;
            return true;
        }

        if (contextualTargetType is not null &&
            TryGetExpressionTreeDelegateType(contextualTargetType, out var expressionDelegate))
        {
            delegateType = expressionDelegate;
            return true;
        }

        // Fallback for local declaration initializers where contextual target typing can be missing.
        if (syntax.Parent is EqualsValueClauseSyntax
            {
                Parent: VariableDeclaratorSyntax
                {
                    TypeAnnotation: { Type: { } annotatedTypeSyntax }
                }
            })
        {
            var annotatedType = ResolveTypeSyntaxOrError(annotatedTypeSyntax);

            if (annotatedType is INamedTypeSymbol annotatedDelegate &&
                annotatedDelegate.TypeKind == TypeKind.Delegate)
            {
                delegateType = annotatedDelegate;
                return true;
            }

            if (TryGetExpressionTreeDelegateType(annotatedType, out expressionDelegate))
            {
                delegateType = expressionDelegate;
                return true;
            }
        }

        return false;
    }

    private void ReportLambdaBodyDiagnostics(FunctionExpressionBinder lambdaBinder)
    {
        foreach (var diagnostic in lambdaBinder.Diagnostics.AsEnumerable())
            _diagnostics.Report(diagnostic);
    }

    private ImmutableArray<INamedTypeSymbol> GetLambdaDelegateTargets(FunctionExpressionSyntax syntax)
    {
        if (_lambdaDelegateTargets.TryGetValue(syntax, out var targets))
            return targets;

        return ImmutableArray<INamedTypeSymbol>.Empty;
    }

    private ImmutableArray<INamedTypeSymbol> ComputeLambdaDelegateTargets(FunctionExpressionSyntax syntax)
    {
        if (syntax.Parent is not ArgumentSyntax argument ||
            argument.Parent is not ArgumentListSyntax argumentList)
        {
            return ImmutableArray<INamedTypeSymbol>.Empty;
        }

        var invocation = argumentList.Parent as InvocationExpressionSyntax;
        if (invocation is null)
            return ImmutableArray<INamedTypeSymbol>.Empty;

        var argumentExpression = argument.Expression;
        var parameterIndex = 0;
        foreach (var candidate in argumentList.Arguments)
        {
            if (candidate == argument)
                break;
            parameterIndex++;
        }

        ImmutableArray<IMethodSymbol> methods = default;
        var extensionReceiverImplicit = false;
        ITypeSymbol? extensionReceiverType = null;
        var callSiteArgumentCount = argumentList.Arguments.Count;

        switch (invocation.Expression)
        {
            case MemberAccessExpressionSyntax memberAccess:
                {
                    // Suppress diagnostics: this is a speculative re-bind to discover
                    // candidate delegate types. Any lookup failures here are spurious.
                    using (_diagnostics.CreateNonReportingScope())
                    {
                        var boundMember = BindMemberAccessExpression(memberAccess);
                        if (boundMember is BoundMethodGroupExpression methodGroup)
                        {
                            extensionReceiverImplicit = methodGroup.Receiver is not null && IsExtensionReceiver(methodGroup.Receiver);
                            if (extensionReceiverImplicit)
                                extensionReceiverType = methodGroup.Receiver?.Type;
                            methods = FilterMethodsForLambda(methodGroup.Methods, parameterIndex, argumentExpression, extensionReceiverImplicit, callSiteArgumentCount);
                        }
                        else if (boundMember is BoundMemberAccessExpression { Receiver: var receiver, Member: IMethodSymbol method })
                        {
                            extensionReceiverImplicit = receiver is not null && IsExtensionReceiver(receiver);
                            if (extensionReceiverImplicit)
                                extensionReceiverType = receiver?.Type;
                            methods = ImmutableArray.Create(method);
                        }
                    }

                    break;
                }

            case MemberBindingExpressionSyntax memberBinding:
                {
                    // Suppress diagnostics: this is a speculative re-bind to discover
                    // candidate delegate types. Any lookup failures here are spurious.
                    using (_diagnostics.CreateNonReportingScope())
                    {
                        var boundMember = BindMemberBindingExpression(memberBinding);
                        if (boundMember is BoundMethodGroupExpression methodGroup)
                        {
                            extensionReceiverImplicit = methodGroup.Receiver is not null && IsExtensionReceiver(methodGroup.Receiver);
                            if (extensionReceiverImplicit)
                                extensionReceiverType = methodGroup.Receiver?.Type;
                            methods = FilterMethodsForLambda(methodGroup.Methods, parameterIndex, argumentExpression, extensionReceiverImplicit, callSiteArgumentCount);
                        }
                        else if (boundMember is BoundMemberAccessExpression { Receiver: var receiver, Member: IMethodSymbol method })
                        {
                            extensionReceiverImplicit = receiver is not null && IsExtensionReceiver(receiver);
                            if (extensionReceiverImplicit)
                                extensionReceiverType = receiver?.Type;
                            methods = ImmutableArray.Create(method);
                        }
                    }

                    break;
                }

            case IdentifierNameSyntax identifier:
                {
                    var candidates = new SymbolQuery(identifier.Identifier.ValueText)
                        .LookupMethods(this)
                        .ToImmutableArray();

                    var accessible = GetAccessibleMethods(candidates, identifier.Identifier.GetLocation(), reportIfInaccessible: false);
                    if (!accessible.IsDefaultOrEmpty)
                    {
                        methods = FilterMethodsForLambda(accessible, parameterIndex, argumentExpression, extensionReceiverImplicit: false, callSiteArgumentCount);
                    }

                    break;
                }
        }

        if (methods.IsDefaultOrEmpty)
            return ImmutableArray<INamedTypeSymbol>.Empty;

        var delegates = ExtractLambdaDelegates(methods, parameterIndex, extensionReceiverImplicit, extensionReceiverType);
        if (!delegates.IsDefaultOrEmpty)
            _lambdaDelegateTargets[syntax] = delegates;
        else
            _lambdaDelegateTargets.Remove(syntax);
        return delegates;
    }
    private ImmutableArray<INamedTypeSymbol> ExtractLambdaDelegates(
        ImmutableArray<IMethodSymbol> methods,
        int parameterIndex,
        bool extensionReceiverImplicit,
        ITypeSymbol? receiverType = null)
    {
        if (methods.IsDefaultOrEmpty)
            return ImmutableArray<INamedTypeSymbol>.Empty;

        var builder = ImmutableArray.CreateBuilder<INamedTypeSymbol>();

        foreach (var method in methods)
        {
            if (!TryGetLambdaParameter(method, parameterIndex, extensionReceiverImplicit, out var parameter))
                continue;

            INamedTypeSymbol? delegateToAdd = null;
            if (parameter.Type is INamedTypeSymbol rawType)
            {
                if (rawType.TypeKind == TypeKind.Delegate)
                    delegateToAdd = rawType;
                else if (TryGetExpressionTreeDelegateType(rawType, out var innerDelegate))
                    delegateToAdd = innerDelegate;
            }

            // For generic extension methods, partially substitute type parameters that
            // can be inferred from the receiver. For example, Func<TSource, TResult> becomes
            // Func<User, TResult> when the receiver is DbSet<User> (implements IQueryable<User>).
            if (delegateToAdd is not null && receiverType is not null &&
                extensionReceiverImplicit && method.IsExtensionMethod &&
                !method.TypeParameters.IsDefaultOrEmpty)
            {
                var substitutions = TryInferTypeParametersFromReceiver(method, receiverType);
                if (substitutions is not null)
                {
                    var substituted = SubstituteTypeParameters(delegateToAdd, substitutions);
                    if (substituted is INamedTypeSymbol substitutedNamed && substitutedNamed.TypeKind == TypeKind.Delegate)
                        delegateToAdd = substitutedNamed;
                }
            }
            else if (delegateToAdd is not null && receiverType is not null &&
                !method.IsExtensionMethod &&
                method.ContainingType is INamedTypeSymbol containingType &&
                !TypeSubstitution.GetShallowTypeArguments(containingType).IsDefaultOrEmpty)
            {
                var substitutions = new Dictionary<ITypeParameterSymbol, ITypeSymbol>(SymbolEqualityComparer.Default);
                if (TryUnifyExtensionReceiverType(containingType, receiverType, substitutions))
                {
                    var substituted = SubstituteTypeParameters(delegateToAdd, substitutions);
                    if (substituted is INamedTypeSymbol substitutedNamed && substitutedNamed.TypeKind == TypeKind.Delegate)
                        delegateToAdd = substitutedNamed;
                }
            }

            if (delegateToAdd is not null &&
                !builder.Any(existing => SymbolEqualityComparer.Default.Equals(existing, delegateToAdd)))
            {
                builder.Add(delegateToAdd);
            }
        }

        return builder.ToImmutable();
    }

    private void RecordLambdaTargets(
        ExpressionSyntax argumentExpression,
        ImmutableArray<IMethodSymbol> methods,
        int parameterIndex,
        bool extensionReceiverImplicit = false,
        ITypeSymbol? receiverType = null,
        bool pipeReceiverImplicit = false)
    {
        if (argumentExpression is not FunctionExpressionSyntax lambda)
        {
            if (argumentExpression is FunctionExpressionSyntax lambdaExpression)
                _lambdaDelegateTargets.Remove(lambdaExpression);
            return;
        }

        if (methods.IsDefaultOrEmpty)
        {
            // No viable method set => clear any previously recorded targets for this lambda.
            _lambdaDelegateTargets.Remove(lambda);
            return;
        }

        var builder = ImmutableArray.CreateBuilder<INamedTypeSymbol>();
        var seen = new HashSet<INamedTypeSymbol>(SymbolEqualityComparer.Default);

        if (_lambdaDelegateTargets.TryGetValue(lambda, out var existing) && !existing.IsDefaultOrEmpty)
        {
            foreach (var candidate in existing)
            {
                if (seen.Add(candidate))
                    builder.Add(candidate);
            }
        }

        foreach (var method in methods)
        {
            if (!TryGetLambdaParameter(method, parameterIndex, extensionReceiverImplicit, out var parameter))
                continue;

            INamedTypeSymbol? delegateToAdd = null;
            if (parameter.Type is INamedTypeSymbol rawType)
            {
                if (rawType.TypeKind == TypeKind.Delegate)
                    delegateToAdd = rawType;
                else if (TryGetExpressionTreeDelegateType(rawType, out var innerDelegate))
                    delegateToAdd = innerDelegate;
            }

            // For generic extension methods, partially substitute type parameters inferrable from the receiver.
            if (delegateToAdd is not null && receiverType is not null &&
                extensionReceiverImplicit && method.IsExtensionMethod &&
                !method.TypeParameters.IsDefaultOrEmpty)
            {
                var substitutions = TryInferTypeParametersFromReceiver(method, receiverType);
                if (substitutions is not null)
                {
                    var substituted = SubstituteTypeParameters(delegateToAdd, substitutions);
                    if (substituted is INamedTypeSymbol substitutedNamed && substitutedNamed.TypeKind == TypeKind.Delegate)
                        delegateToAdd = substitutedNamed;
                }
            }
            else if (delegateToAdd is not null && receiverType is not null &&
                !method.IsExtensionMethod &&
                method.ContainingType is INamedTypeSymbol containingType &&
                !TypeSubstitution.GetShallowTypeArguments(containingType).IsDefaultOrEmpty)
            {
                var substitutions = new Dictionary<ITypeParameterSymbol, ITypeSymbol>(SymbolEqualityComparer.Default);
                if (TryUnifyExtensionReceiverType(containingType, receiverType, substitutions))
                {
                    var substituted = SubstituteTypeParameters(delegateToAdd, substitutions);
                    if (substituted is INamedTypeSymbol substitutedNamed && substitutedNamed.TypeKind == TypeKind.Delegate)
                        delegateToAdd = substitutedNamed;
                }
            }
            // For generic non-extension methods in pipe expressions, infer type parameters from
            // the pipe source (which occupies parameter 0 implicitly).
            else if (delegateToAdd is not null && receiverType is not null &&
                pipeReceiverImplicit && !method.IsExtensionMethod &&
                !method.TypeParameters.IsDefaultOrEmpty)
            {
                var substitutions = TryInferTypeParametersFromFirstArg(method, receiverType);
                if (substitutions is not null)
                {
                    var substituted = SubstituteTypeParameters(delegateToAdd, substitutions);
                    if (substituted is INamedTypeSymbol substitutedNamed && substitutedNamed.TypeKind == TypeKind.Delegate)
                        delegateToAdd = substitutedNamed;
                }
            }

            if (delegateToAdd is not null && seen.Add(delegateToAdd))
                builder.Add(delegateToAdd);
        }

        if (builder.Count > 0)
        {
            _lambdaDelegateTargets[lambda] = builder.ToImmutable();
        }
        else
        {
            // We ended up selecting an overload where the parameter is not a concrete delegate type
            // (e.g. `System.Delegate`). Clear any stale delegate targets (like RequestDelegate)
            // so lambda binding can fall back to normal inference (e.g. return `string`).
            _lambdaDelegateTargets.Remove(lambda);
        }
    }

    private ITypeSymbol? TryInferLambdaParameterType(ImmutableArray<INamedTypeSymbol> candidateDelegates, int parameterIndex, out RefKind? inferredRefKind)
    {
        inferredRefKind = null;

        if (candidateDelegates.IsDefaultOrEmpty)
            return null;

        ITypeSymbol? inferredType = null;

        foreach (var delegateType in candidateDelegates)
        {
            var invoke = delegateType.GetDelegateInvokeMethod();
            if (invoke is null || invoke.Parameters.Length <= parameterIndex)
                continue;

            var parameter = invoke.Parameters[parameterIndex];

            if (inferredType is null)
            {
                inferredType = parameter.Type;
                inferredRefKind = parameter.RefKind;
                continue;
            }

            if (!SymbolEqualityComparer.Default.Equals(inferredType, parameter.Type))
                return null;

            if (inferredRefKind != parameter.RefKind)
                return null;
        }

        return inferredType;
    }

    /// <summary>
    /// For generic extension methods, infer type parameters from the receiver type
    /// by matching the method's first parameter type against the concrete receiver type.
    /// For example, given <c>Select&lt;TSource, TResult&gt;(this IQueryable&lt;TSource&gt; source, ...)</c>
    /// and receiver type <c>DbSet&lt;User&gt;</c> (which implements <c>IQueryable&lt;User&gt;</c>),
    /// this infers <c>TSource = User</c>.
    /// </summary>
    private static Dictionary<ITypeParameterSymbol, ITypeSymbol>? TryInferTypeParametersFromReceiver(
        IMethodSymbol method,
        ITypeSymbol receiverType)
    {
        if (!method.IsExtensionMethod || method.TypeParameters.IsDefaultOrEmpty)
            return null;

        return TryInferTypeParametersFromFirstArg(method, receiverType);
    }

    private static Dictionary<ITypeParameterSymbol, ITypeSymbol>? TryInferTypeParametersFromFirstArg(
        IMethodSymbol method,
        ITypeSymbol argumentType)
    {
        if (method.TypeParameters.IsDefaultOrEmpty || method.Parameters.IsDefaultOrEmpty)
            return null;

        var firstParamType = method.Parameters[0].Type;
        if (firstParamType is null)
            return null;

        var substitutions = new Dictionary<ITypeParameterSymbol, ITypeSymbol>(SymbolEqualityComparer.Default);
        InferTypeParametersFromMatch(firstParamType, argumentType, substitutions, method);

        return substitutions.Count > 0 ? substitutions : null;
    }

    /// <summary>
    /// Recursively matches a parameter type (which may contain type parameters) against
    /// a concrete argument type, collecting substitutions. Walks named type arguments,
    /// arrays, and interface hierarchies.
    /// </summary>
    private static void InferTypeParametersFromMatch(
        ITypeSymbol parameterType,
        ITypeSymbol argumentType,
        Dictionary<ITypeParameterSymbol, ITypeSymbol> substitutions,
        IMethodSymbol method)
    {
        if (argumentType.TypeKind == TypeKind.Error)
            return;

        if (parameterType is ITypeParameterSymbol typeParam)
        {
            if (TryGetCanonicalMethodTypeParameter(typeParam, method, out var canonical))
            {
                if (!substitutions.ContainsKey(canonical))
                    substitutions[canonical] = argumentType;
            }
            return;
        }

        if (parameterType is INamedTypeSymbol paramNamed)
        {
            var parameterDefinition = TypeSubstitution.GetDefinitionForSubstitution(paramNamed);
            var parameterArguments = TypeSubstitution.GetShallowTypeArguments(paramNamed);
            if (parameterArguments.IsDefaultOrEmpty)
                return;

            // Try direct match (same OriginalDefinition)
            if (argumentType is INamedTypeSymbol argNamed &&
                SymbolEqualityComparer.Default.Equals(parameterDefinition, TypeSubstitution.GetDefinitionForSubstitution(argNamed)))
            {
                var argumentArguments = TypeSubstitution.GetShallowTypeArguments(argNamed);
                if (!argumentArguments.IsDefaultOrEmpty &&
                    parameterArguments.Length == argumentArguments.Length)
                {
                    for (int i = 0; i < parameterArguments.Length; i++)
                        InferTypeParametersFromMatch(parameterArguments[i], argumentArguments[i], substitutions, method);
                    return;
                }
            }

            // Try interfaces (e.g., DbSet<User> implements IQueryable<User>)
            if (argumentType is INamedTypeSymbol argNamed2)
            {
                foreach (var iface in argNamed2.AllInterfaces)
                {
                    var interfaceArguments = TypeSubstitution.GetShallowTypeArguments(iface);
                    if (SymbolEqualityComparer.Default.Equals(parameterDefinition, TypeSubstitution.GetDefinitionForSubstitution(iface)) &&
                        !interfaceArguments.IsDefaultOrEmpty &&
                        parameterArguments.Length == interfaceArguments.Length)
                    {
                        for (int i = 0; i < parameterArguments.Length; i++)
                            InferTypeParametersFromMatch(parameterArguments[i], interfaceArguments[i], substitutions, method);
                        return;
                    }
                }

                // Try base types
                for (var baseType = argNamed2.BaseType; baseType is not null; baseType = baseType.BaseType)
                {
                    var baseArguments = TypeSubstitution.GetShallowTypeArguments(baseType);
                    if (SymbolEqualityComparer.Default.Equals(parameterDefinition, TypeSubstitution.GetDefinitionForSubstitution(baseType)) &&
                        !baseArguments.IsDefaultOrEmpty &&
                        parameterArguments.Length == baseArguments.Length)
                    {
                        for (int i = 0; i < parameterArguments.Length; i++)
                            InferTypeParametersFromMatch(parameterArguments[i], baseArguments[i], substitutions, method);
                        return;
                    }
                }
            }
        }

        if (parameterType is IArrayTypeSymbol paramArray && argumentType is IArrayTypeSymbol argArray)
        {
            InferTypeParametersFromMatch(paramArray.ElementType, argArray.ElementType, substitutions, method);
        }

        static bool TryGetCanonicalMethodTypeParameter(
            ITypeParameterSymbol typeParameter,
            IMethodSymbol method,
            out ITypeParameterSymbol canonical)
        {
            if (typeParameter.Ordinal >= 0 &&
                typeParameter.Ordinal < method.TypeParameters.Length)
            {
                var candidate = method.TypeParameters[typeParameter.Ordinal];
                if (string.Equals(candidate.Name, typeParameter.Name, StringComparison.Ordinal) ||
                    SymbolEqualityComparer.Default.Equals(candidate, typeParameter))
                {
                    canonical = candidate;
                    return true;
                }
            }

            canonical = null!;
            return false;
        }
    }

    private static bool TryGetLambdaParameter(
        IMethodSymbol method,
        int argumentIndex,
        bool extensionReceiverImplicit,
        out IParameterSymbol? parameter)
    {
        var parameterIndex = GetLambdaParameterIndex(method, argumentIndex, extensionReceiverImplicit);

        if (parameterIndex < 0 || parameterIndex >= method.Parameters.Length)
        {
            parameter = null;
            return false;
        }

        parameter = method.Parameters[parameterIndex];
        return true;
    }

    private static int GetLambdaParameterIndex(
        IMethodSymbol method,
        int argumentIndex,
        bool extensionReceiverImplicit)
    {
        if (method.IsExtensionMethod && extensionReceiverImplicit)
            return argumentIndex + 1;

        return argumentIndex;
    }

    private ITypeSymbol? TryGetCommonParameterType(
        ImmutableArray<IMethodSymbol> candidates,
        int index,
        bool extensionReceiverImplicit = false)
    {
        if (candidates.IsDefaultOrEmpty)
            return null;

        ITypeSymbol? parameterType = null;

        foreach (var candidate in candidates)
        {
            if (!TryGetLambdaParameter(candidate, index, extensionReceiverImplicit, out var parameter))
                return null;

            var candidateType = parameter.Type;

            if (parameterType is null)
            {
                parameterType = candidateType;
                continue;
            }

            if (!SymbolEqualityComparer.Default.Equals(parameterType, candidateType))
                return null;
        }

        return parameterType;
    }

    private ImmutableArray<IMethodSymbol> FilterMethodsForLambda(
        ImmutableArray<IMethodSymbol> methods,
        int parameterIndex,
        ExpressionSyntax argumentExpression,
        bool extensionReceiverImplicit,
        int callSiteArgumentCount = -1)
    {
        if (methods.IsDefaultOrEmpty)
            return methods;

        if (argumentExpression is not FunctionExpressionSyntax lambda)
            return methods;

        int parameterCount = lambda switch
        {
            SimpleFunctionExpressionSyntax => 1,
            ParenthesizedFunctionExpressionSyntax p => p.ParameterList.Parameters.Count,
            _ => 0
        };

        var builder = ImmutableArray.CreateBuilder<IMethodSymbol>();

        foreach (var method in methods)
        {
            // When we know the total number of call-site arguments, skip overloads
            // whose visible parameter count doesn't match. This prevents overloads
            // like F(predicate, errorFactory) from polluting inference when only
            // F(errorFactory) is being called with a single argument.
            if (callSiteArgumentCount >= 0)
            {
                var firstVisible = (method.IsExtensionMethod && extensionReceiverImplicit) ? 1 : 0;
                var visibleParams = method.Parameters.Length - firstVisible;

                var requiredParams = visibleParams;
                for (var i = method.Parameters.Length - 1; i >= firstVisible; i--)
                {
                    if (method.Parameters[i].HasExplicitDefaultValue || method.Parameters[i].IsVarParams)
                        requiredParams--;
                    else
                        break;
                }

                if (callSiteArgumentCount < requiredParams || callSiteArgumentCount > visibleParams)
                    continue;
            }

            if (!TryGetLambdaParameter(method, parameterIndex, extensionReceiverImplicit, out var parameter))
                continue;

            var parameterType = parameter.Type;
            INamedTypeSymbol? effectiveDelegateType = null;
            if (parameterType is INamedTypeSymbol namedParamType)
            {
                if (namedParamType.TypeKind == TypeKind.Delegate)
                    effectiveDelegateType = namedParamType;
                else if (TryGetExpressionTreeDelegateType(namedParamType, out var innerDelegate))
                    effectiveDelegateType = innerDelegate;
            }

            if (effectiveDelegateType is null)
            {
                // A lambda can only match a delegate-typed parameter.
                // Concrete non-delegate named types (e.g. int, string) are definitely
                // incompatible.  Unbound type parameters (T) are kept because the
                // concrete type might turn out to be a delegate after substitution.
                if (parameterType is not ITypeParameterSymbol)
                    continue;

                builder.Add(method);
                continue;
            }

            var invoke = effectiveDelegateType.GetDelegateInvokeMethod();
            if (invoke is null)
                continue;

            if (invoke.Parameters.Length != parameterCount)
                continue;

            builder.Add(method);
        }

        return builder.Count == methods.Length ? methods : builder.ToImmutable();
    }

    private bool EnsureLambdaCompatible(IParameterSymbol parameter, BoundFunctionExpression lambda)
    {
        if (parameter.Type is not INamedTypeSymbol delegateType)
            return false;

        if (ReplayLambda(lambda, delegateType) is not null)
            return true;

        // If lambda body binding already produced diagnostics (e.g., unknown identifier in body),
        // keep signature-compatible candidates so overload resolution can still pick a method and
        // avoid cascading "no overload" diagnostics.
        if (!HasFunctionExpressionErrors(lambda))
            return false;

        var invoke = delegateType.GetDelegateInvokeMethod();
        if (invoke is null)
            return false;

        var lambdaParameterCount = lambda.Parameters.Count();
        return invoke.Parameters.Length == lambdaParameterCount;
    }

    private BoundFunctionExpression? ReplayLambda(BoundFunctionExpression lambda, INamedTypeSymbol delegateType)
    {
        var instrumentation = Compilation.PerformanceInstrumentation.LambdaReplay;
        instrumentation.RecordReplayAttempt();

        // Unwrap Expression<TDelegate> so the lambda can be replayed against the inner delegate type.
        if (delegateType.TypeKind != TypeKind.Delegate)
        {
            if (TryGetExpressionTreeDelegateType(delegateType, out var innerDelegate))
                delegateType = innerDelegate;
            else
            {
                instrumentation.RecordReplayFailure();
                return null;
            }
        }

        var syntax = GetLambdaSyntax(lambda);

        if (syntax is not null)
        {
            var key = new FunctionExpressionRebindKey(syntax, delegateType);
            if (_reboundLambdaCache.TryGetValue(key, out var cached))
            {
                if (!HasCachedLambdaBodyBindings(syntax))
                {
                    _reboundLambdaCache.Remove(key);
                }
                else
                {
                    instrumentation.RecordCacheHit();
                    instrumentation.RecordReplaySuccess();
                    return cached;
                }
            }

            instrumentation.RecordCacheMiss();
        }

        BoundFunctionExpression? rebound;
        if (lambda.Unbound is { } unbound)
        {
            rebound = ReplayLambda(unbound, delegateType);
            if (rebound is null)
            {
                instrumentation.RecordReplayFailure();
                return null;
            }

            instrumentation.RecordReplaySuccess();
        }
        else if (lambda.IsCompatibleWithDelegate(delegateType, Compilation))
        {
            rebound = lambda;
            instrumentation.RecordReplaySuccess();
        }
        else
        {
            instrumentation.RecordReplayFailure();
            return null;
        }

        if (syntax is not null && rebound is not null)
            _reboundLambdaCache[new FunctionExpressionRebindKey(syntax, delegateType)] = rebound;

        return rebound;
    }

    private BoundFunctionExpression? ReplayLambda(BoundUnboundFunctionExpression unbound, INamedTypeSymbol delegateType)
    {
        var instrumentation = Compilation.PerformanceInstrumentation.LambdaReplay;
        instrumentation.RecordBindingInvocation();

        var invoke = delegateType.GetDelegateInvokeMethod();
        if (invoke is null)
        {
            instrumentation.RecordBindingFailure();
            return null;
        }

        var syntax = unbound.Syntax;
        var parameterSyntaxes = syntax switch
        {
            SimpleFunctionExpressionSyntax simple => new[] { simple.Parameter },
            ParenthesizedFunctionExpressionSyntax parenthesized => parenthesized.ParameterList.Parameters.ToArray(),
            _ => Array.Empty<ParameterSyntax>()
        };

        if (invoke.Parameters.Length != parameterSyntaxes.Length)
        {
            instrumentation.RecordBindingFailure();
            return null;
        }

        var parameterSymbols = new List<IParameterSymbol>(parameterSyntaxes.Length);
        var seenOptionalParameter = false;

        for (int index = 0; index < parameterSyntaxes.Length; index++)
        {
            var parameterSyntax = parameterSyntaxes[index];
            var delegateParameter = invoke.Parameters[index];
            var isMutable = delegateParameter.RefKind is RefKind.Ref or RefKind.Out;

            // Preserve any default values that were already extracted during the initial bind.
            // IMPORTANT: We still do NOT validate/convert these defaults during replay.
            // Validation/conversion (and diagnostics) should happen only once an overload is selected.
            var originalParam = unbound.LambdaSymbol.Parameters.Length > index
                ? unbound.LambdaSymbol.Parameters[index]
                : null;

            var hasExplicitDefaultValue = originalParam?.HasExplicitDefaultValue == true;
            var explicitDefaultValue = hasExplicitDefaultValue
                ? originalParam!.ExplicitDefaultValue
                : null;

            // If the lambda parameter has an explicit type annotation, respect it during replay.
            // If it doesn't match the candidate delegate's parameter type (or ref-kind), the
            // candidate is not applicable. Do not report diagnostics during replay.
            var typeSyntax = parameterSyntax.TypeAnnotation?.Type;
            var refKindTokenKind = parameterSyntax.RefKindKeyword.Kind;

            var refKind = delegateParameter.RefKind;
            ITypeSymbol parameterType;

            if (typeSyntax is not null)
            {
                var explicitRefKind = refKind;

                if (typeSyntax is ByRefTypeSyntax)
                {
                    explicitRefKind = refKindTokenKind switch
                    {
                        SyntaxKind.OutKeyword => RefKind.Out,
                        SyntaxKind.InKeyword => RefKind.In,
                        SyntaxKind.RefKeyword => RefKind.Ref,
                        _ => RefKind.Ref,
                    };
                }

                parameterType = ResolveTypeSyntaxOrError(typeSyntax, explicitRefKind);

                // Explicitly annotated type must match the candidate delegate parameter type.
                if (parameterType.TypeKind != TypeKind.Error &&
                    delegateParameter.Type.TypeKind != TypeKind.Error &&
                    !SymbolEqualityComparer.Default.Equals(parameterType, delegateParameter.Type))
                {
                    instrumentation.RecordBindingFailure();
                    return null;
                }

                // Explicitly annotated ref-kind must match the candidate delegate ref-kind.
                if (explicitRefKind != delegateParameter.RefKind)
                {
                    instrumentation.RecordBindingFailure();
                    return null;
                }

                refKind = explicitRefKind;
            }
            else
            {
                // No explicit annotation: use the candidate delegate parameter type.
                parameterType = delegateParameter.Type;
                refKind = delegateParameter.RefKind;
            }

            // IMPORTANT: Lambda replay is used for overload resolution / compatibility checks.
            // Do NOT validate or flow explicit default values here, because replay may bind the
            // lambda against an unrelated delegate candidate (e.g. RequestDelegate(HttpContext)->Task)
            // and we must not report diagnostics like "default cannot convert to HttpContext" for
            // a delegate type that will not be selected.
            var parameterName = GetLambdaParameterSymbolName(parameterSyntax, index);
            var parameterLocation = GetLambdaParameterLocation(parameterSyntax);

            var parameterSymbol = new SourceParameterSymbol(
                parameterName,
                parameterType,
                _containingSymbol,
                _containingSymbol.ContainingType as INamedTypeSymbol,
                _containingSymbol.ContainingNamespace,
                [parameterLocation],
                [parameterSyntax.GetReference()],
                refKind,
                hasExplicitDefaultValue: hasExplicitDefaultValue,
                explicitDefaultValue: explicitDefaultValue,
                isMutable: isMutable);

            // Keep the replay order rule: once we saw an optional parameter, later parameters must be optional.
            if (seenOptionalParameter && !hasExplicitDefaultValue)
            {
                instrumentation.RecordBindingFailure();
                return null;
            }

            if (hasExplicitDefaultValue)
                seenOptionalParameter = true;

            parameterSymbols.Add(parameterSymbol);
        }

        var lambdaSymbol = new SourceLambdaSymbol(
            parameterSymbols,
            invoke.ReturnType,
            _containingSymbol,
            _containingSymbol.ContainingType as INamedTypeSymbol,
            _containingSymbol.ContainingNamespace,
            [syntax.GetLocation()],
            [syntax.GetReference()],
            isAsync: unbound.LambdaSymbol.IsAsync);

        var lambdaBinder = new FunctionExpressionBinder(lambdaSymbol, this);

        if (syntax is ParenthesizedFunctionExpressionSyntax
            {
                Identifier: { Kind: not SyntaxKind.None } identifierToken
            })
        {
            lambdaBinder.DeclareFunction(identifierToken.ValueText, lambdaSymbol);
        }

        var hasScopedReplayFunctionName = false;
        string? scopedReplayFunctionName = null;
        var hadPreviousReplayFunction = false;
        IMethodSymbol? previousReplayFunction = null;

        if (syntax is ParenthesizedFunctionExpressionSyntax
            {
                Identifier: { Kind: not SyntaxKind.None } replayIdentifierToken
            })
        {
            hasScopedReplayFunctionName = true;
            scopedReplayFunctionName = replayIdentifierToken.ValueText;
            hadPreviousReplayFunction = _functions.TryGetValue(scopedReplayFunctionName, out previousReplayFunction);
            _functions[scopedReplayFunctionName] = lambdaSymbol;
        }

        foreach (var parameter in parameterSymbols)
            lambdaBinder.DeclareParameter(parameter);

        var destructuringPrologue = BindLambdaDestructuringPrologue(lambdaBinder, parameterSyntaxes, parameterSymbols);

        var lambdaBodySyntax = GetLambdaBodyExpression(syntax);
        BoundExpression body;
        try
        {
            body = lambdaBinder.BindExpression(lambdaBodySyntax, allowReturn: true);
            body = RetargetLambdaBodyUnionCase(body, invoke.ReturnType);

            if (destructuringPrologue.Count > 0)
            {
                destructuringPrologue.Add(new BoundExpressionStatement(body));
                body = new BoundBlockExpression(destructuringPrologue, Compilation.GetSpecialType(SpecialType.System_Unit));
            }
        }
        finally
        {
            if (hasScopedReplayFunctionName && scopedReplayFunctionName is not null)
            {
                if (hadPreviousReplayFunction && previousReplayFunction is not null)
                    _functions[scopedReplayFunctionName] = previousReplayFunction;
                else
                    _functions.Remove(scopedReplayFunctionName);
            }
        }

        // If the lambda has an explicit return type annotation, it must match the delegate candidate.
        // Do not report diagnostics during replay; just treat the candidate as not applicable.
        TypeSyntax? annotatedReturnTypeSyntax = syntax switch
        {
            SimpleFunctionExpressionSyntax s => s.ReturnType?.Type,
            ParenthesizedFunctionExpressionSyntax p => p.ReturnType?.Type,
            _ => null
        };

        if (annotatedReturnTypeSyntax is not null)
        {
            var annotatedReturnType = ResolveTypeSyntaxOrError(annotatedReturnTypeSyntax);

            if (annotatedReturnType.TypeKind != TypeKind.Error &&
                invoke.ReturnType.TypeKind != TypeKind.Error &&
                !SymbolEqualityComparer.Default.Equals(annotatedReturnType, invoke.ReturnType))
            {
                instrumentation.RecordBindingFailure();
                return null;
            }
        }
        var unitType = Compilation.GetSpecialType(SpecialType.System_Unit);
        var collectedReturn = unbound.LambdaSymbol.IsAsync
            ? ReturnTypeCollector.InferAsync(Compilation, body)
            : ReturnTypeCollector.Infer(body);

        var inferred = body.Type;
        if (inferred is null || SymbolEqualityComparer.Default.Equals(inferred, unitType))
            inferred = collectedReturn;

        if (inferred is null)
            inferred = collectedReturn;

        if (inferred is not null && inferred.TypeKind != TypeKind.Error)
            inferred = TypeSymbolNormalization.NormalizeForInference(inferred);

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

        var returnType = invoke.ReturnType;
        ITypeSymbol? inferredAsyncReturn = null;

        if (unbound.LambdaSymbol.IsAsync)
        {
            inferredAsyncReturn = collectedReturn ??
                AsyncReturnTypeUtilities.InferAsyncReturnType(
                    Compilation,
                    inferred ?? returnType);

            if (inferredAsyncReturn is { TypeKind: not TypeKind.Error } &&
                (returnType is null ||
                 returnType.TypeKind == TypeKind.Error ||
                 ContainsTypeParameter(returnType)))
            {
                returnType = inferredAsyncReturn;
            }
        }

        ITypeSymbol? ExtractAsyncResultTypeForReplay(ITypeSymbol? asyncReturnType)
        {
            if (asyncReturnType is null)
                return null;

            if (asyncReturnType is NullableTypeSymbol nullable)
                asyncReturnType = nullable.UnderlyingType;

            if (asyncReturnType.SpecialType == SpecialType.System_Threading_Tasks_Task)
                return unitType;

            if (asyncReturnType is INamedTypeSymbol named &&
                named.OriginalDefinition.SpecialType == SpecialType.System_Threading_Tasks_Task_T &&
                named.TypeArguments.Length == 1)
            {
                return named.TypeArguments[0];
            }

            return null;
        }

        var expectedBodyType = returnType;
        if (unbound.LambdaSymbol.IsAsync)
            expectedBodyType = ExtractAsyncResultTypeForReplay(returnType) ?? expectedBodyType;

        var conversionSource = inferred;

        if (unbound.LambdaSymbol.IsAsync && conversionSource is not null)
            conversionSource = ExtractAsyncResultTypeForReplay(conversionSource) ?? conversionSource;

        var shouldApplyConversion =
            conversionSource is not null &&
            conversionSource.TypeKind != TypeKind.Error &&
            expectedBodyType is not null &&
            expectedBodyType.TypeKind != TypeKind.Error &&
            expectedBodyType is not ITypeParameterSymbol;

        if (shouldApplyConversion)
        {
            if (!IsAssignable(expectedBodyType, conversionSource!, out var conversion))
            {
                instrumentation.RecordBindingFailure();
                return null;
            }

            body = ApplyConversion(body, expectedBodyType, conversion, lambdaBodySyntax);
        }

        lambdaBinder.SetLambdaBody(body);
        var captured = lambdaBinder.AnalyzeCapturedVariables();

        var capturedSet = new HashSet<ISymbol>(captured, SymbolEqualityComparer.Default);
        foreach (var nestedCapture in FunctionExpressionSelfCaptureCollector.Collect(body, lambdaSymbol))
            capturedSet.Add(nestedCapture);

        var capturedVariables = capturedSet.ToImmutableArray();

        lambdaSymbol.SetCapturedVariables(capturedVariables);
        if (capturedVariables.Length != 0 && lambdaSymbol.ClosureFrameType is null)
            lambdaSymbol.SetClosureFrameType(ClosureFrameSymbolFactory.Create(lambdaSymbol));
        lambdaSymbol.SetReturnType(returnType);
        lambdaSymbol.SetDelegateType(delegateType);

        var candidateDelegates = unbound.CandidateDelegates;
        if (candidateDelegates.IsDefaultOrEmpty)
        {
            candidateDelegates = ImmutableArray.Create(delegateType);
        }
        else if (!candidateDelegates.Contains(delegateType, SymbolEqualityComparer.Default))
        {
            candidateDelegates = candidateDelegates.Add(delegateType);
        }

        _lambdaDelegateTargets[syntax] = candidateDelegates;

        var rebound = new BoundFunctionExpression(
            parameterSymbols,
            returnType,
            body,
            lambdaSymbol,
            delegateType,
            capturedVariables,
            candidateDelegates);
        rebound.AttachUnbound(unbound);
        CacheBoundNode(syntax, rebound);
        instrumentation.RecordBindingSuccess();
        return rebound;
    }

    private BoundExpression RetargetLambdaBodyUnionCase(BoundExpression expression, ITypeSymbol targetType)
    {
        targetType = UnwrapAlias(targetType);
        targetType = UnwrapTaskLikeTargetType(targetType);

        if (expression is not BoundUnionCaseExpression unionCase ||
            targetType is not INamedTypeSymbol targetUnion ||
            !SymbolEqualityComparer.Default.Equals(unionCase.UnionType.OriginalDefinition, targetUnion.OriginalDefinition) ||
            SymbolEqualityComparer.Default.Equals(unionCase.UnionType, targetUnion))
        {
            return expression;
        }

        return new BoundUnionCaseExpression(
            targetUnion,
            unionCase.CaseType,
            unionCase.CaseConstructor,
            unionCase.Arguments);
    }

    private FunctionExpressionSyntax? GetLambdaSyntax(BoundFunctionExpression lambda)
    {
        if (lambda.Unbound is { Syntax: { } syntax })
            return syntax;

        if (lambda.Symbol is ILambdaSymbol lambdaSymbol)
        {
            foreach (var reference in lambdaSymbol.DeclaringSyntaxReferences)
            {
                if (reference.GetSyntax() is FunctionExpressionSyntax lambdaSyntax)
                    return lambdaSyntax;
            }
        }

        return null;
    }

    private static string GetLambdaParameterSymbolName(ParameterSyntax parameterSyntax, int index)
    {
        var name = parameterSyntax.Identifier.ValueText;
        return string.IsNullOrEmpty(name) ? $"__destructuredArg{index}" : name;
    }

    private static string GetLambdaParameterNameForDiagnostics(ParameterSyntax parameterSyntax, int index)
    {
        var name = parameterSyntax.Identifier.ValueText;
        return string.IsNullOrEmpty(name) ? $"parameter{index + 1}" : name;
    }

    private static Location GetLambdaParameterLocation(ParameterSyntax parameterSyntax)
        => parameterSyntax.Pattern?.GetLocation() ?? parameterSyntax.Identifier.GetLocation();

    private List<BoundStatement> BindLambdaDestructuringPrologue(
        FunctionExpressionBinder lambdaBinder,
        ParameterSyntax[] parameterSyntaxes,
        List<IParameterSymbol> parameterSymbols)
    {
        var prologue = new List<BoundStatement>();

        for (var index = 0; index < parameterSyntaxes.Length; index++)
        {
            var parameterSyntax = parameterSyntaxes[index];
            if (parameterSyntax.Pattern is not PatternSyntax pattern)
                continue;

            var declarationBindingKeywordKind = parameterSyntax.BindingKeyword.Kind switch
            {
                SyntaxKind.VarKeyword => SyntaxKind.VarKeyword,
                SyntaxKind.LetKeyword => SyntaxKind.LetKeyword,
                _ => SyntaxKind.ValKeyword
            };

            var assignment = lambdaBinder.BindPatternAssignment(
                pattern,
                new BoundParameterAccess(parameterSymbols[index]),
                pattern,
                declarationBindingKeywordKind);

            prologue.Add(new BoundExpressionStatement(assignment));
        }

        return prologue;
    }

    private static ExpressionSyntax GetLambdaBodyExpression(FunctionExpressionSyntax syntax)
        => syntax.Body ?? syntax.ExpressionBody?.Expression
            ?? throw new InvalidOperationException("Function expression is missing both block and expression bodies.");

    private static SyntaxNode GetLambdaBodySyntaxNode(FunctionExpressionSyntax syntax)
        => (SyntaxNode?)syntax.Body ?? (SyntaxNode?)syntax.ExpressionBody ?? syntax;

    private bool HasCachedLambdaBodyBindings(FunctionExpressionSyntax syntax)
    {
        var bodyNode = GetLambdaBodySyntaxNode(syntax);
        if (TryGetCachedBoundNode(bodyNode) is null)
            return false;

        // Ensure semantic queries for leaf expressions (like `x` in `x.Result`) can
        // resolve from cache after replay cache hits.
        foreach (var child in bodyNode.DescendantNodes())
        {
            if (child is IdentifierNameSyntax or MemberAccessExpressionSyntax)
            {
                if (TryGetCachedBoundNode(child) is null)
                    return false;
            }
        }

        return true;
    }

    private void ClearCachedLambdaBodyNodes(SyntaxNode bodyNode)
    {
        RemoveCachedBoundNode(bodyNode);
        RemoveCachedBinder(bodyNode);
        foreach (var child in bodyNode.DescendantNodes())
        {
            RemoveCachedBoundNode(child);
            RemoveCachedBinder(child);
        }
    }

    private void ReportSuppressedLambdaDiagnostics(IEnumerable<BoundArgument> arguments)
    {
        foreach (var argument in arguments)
        {
            if (argument.Expression is BoundFunctionExpression { Unbound: { } unbound })
                unbound.ReportSuppressedDiagnostics(_diagnostics);
        }
    }

    private static bool HasArgumentBindingErrors(IEnumerable<BoundArgument> arguments)
    {
        foreach (var argument in arguments)
        {
            if (HasExpressionErrors(argument.Expression))
                return true;
        }

        return false;
    }

    private bool HasExistingArgumentErrors(SeparatedSyntaxList<ArgumentSyntax> arguments)
    {
        foreach (var argument in arguments)
        {
            var expression = argument.Expression;
            if (expression is null)
                continue;

            var span = expression.Span;
            foreach (var diagnostic in _diagnostics.AsEnumerable())
            {
                if (diagnostic.Severity != DiagnosticSeverity.Error ||
                    diagnostic.Location is null)
                {
                    continue;
                }

                if (diagnostic.Location.SourceSpan.IntersectsWith(span))
                    return true;
            }
        }

        return false;
    }
}
