using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

partial class BlockBinder
{
    private BoundExpression BindLambdaExpression(LambdaExpressionSyntax syntax)
    {
        var parameterSyntaxes = syntax switch
        {
            SimpleLambdaExpressionSyntax s => new[] { s.Parameter },
            ParenthesizedLambdaExpressionSyntax p => p.ParameterList.Parameters.ToArray(),
            _ => throw new NotSupportedException("Unknown lambda syntax")
        };

        SyntaxToken? asyncKeywordToken = syntax switch
        {
            SimpleLambdaExpressionSyntax simple => simple.AsyncKeyword,
            ParenthesizedLambdaExpressionSyntax parenthesized => parenthesized.AsyncKeyword,
            _ => null
        };

        var isAsyncLambda = asyncKeywordToken.HasValue &&
            asyncKeywordToken.Value.Kind == SyntaxKind.AsyncKeyword &&
            !asyncKeywordToken.Value.IsMissing;

        var targetType = GetTargetType(syntax);
        var candidateDelegates = GetLambdaDelegateTargets(syntax);
        if (candidateDelegates.IsDefaultOrEmpty)
            candidateDelegates = ComputeLambdaDelegateTargets(syntax);
        var suppressedDiagnostics = ImmutableArray.CreateBuilder<SuppressedLambdaDiagnostic>();

        INamedTypeSymbol? targetDelegate = targetType as INamedTypeSymbol;
        if (targetDelegate?.TypeKind != TypeKind.Delegate)
            targetDelegate = null;

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

        INamedTypeSymbol? primaryDelegate = targetDelegate ?? candidateDelegates.FirstOrDefault();
        var targetSignature = primaryDelegate?.GetDelegateInvokeMethod();

        var parameterSymbols = new List<IParameterSymbol>();
        for (int index = 0; index < parameterSyntaxes.Length; index++)
        {
            var parameterSyntax = parameterSyntaxes[index];
            var annotation = parameterSyntax.TypeAnnotation;
            var typeSyntax = annotation?.Type;
            var refKind = RefKind.None;
            var refKindTokenKind = parameterSyntax.RefKindKeyword?.Kind;

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
                var refKindForType = refKind == RefKind.None && typeSyntax is ByRefTypeSyntax ? RefKind.Ref : refKind;
                parameterType = refKindForType is RefKind.Ref or RefKind.Out or RefKind.In or RefKind.RefReadOnly or RefKind.RefReadOnlyParameter
                    ? ResolveType(typeSyntax, refKindForType)
                    : ResolveType(typeSyntax);
            }
            else if (targetParam is not null)
            {
                parameterType = targetParam.Type;
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
                    var parameterName = parameterSyntax.Identifier.ValueText;
                    var parameterLocation = parameterSyntax.Identifier.GetLocation();

                    if (candidateDelegates.IsDefaultOrEmpty)
                    {
                        _diagnostics.ReportLambdaParameterTypeCannotBeInferred(
                            parameterName,
                            parameterLocation);
                    }
                    else
                    {
                        suppressedDiagnostics.Add(new SuppressedLambdaDiagnostic(parameterName, parameterLocation));
                    }
                }
            }

            var isMutable = parameterSyntax.BindingKeyword?.Kind == SyntaxKind.VarKeyword;

            var symbol = new SourceParameterSymbol(
                parameterSyntax.Identifier.ValueText,
                parameterType,
                _containingSymbol,
                _containingSymbol.ContainingType as INamedTypeSymbol,
                _containingSymbol.ContainingNamespace,
                [parameterSyntax.GetLocation()],
                [parameterSyntax.GetReference()],
                refKind,
                isMutable: isMutable
            );

            parameterSymbols.Add(symbol);
        }

        TypeSyntax? returnTypeSyntax = syntax switch
        {
            SimpleLambdaExpressionSyntax s => s.ReturnType?.Type,
            ParenthesizedLambdaExpressionSyntax p => p.ReturnType?.Type,
            _ => null
        };

        ITypeSymbol? annotatedReturnType = returnTypeSyntax is not null
            ? ResolveType(returnTypeSyntax)
            : null;

        var hasInvalidAsyncReturnType = false;

        if (isAsyncLambda && returnTypeSyntax is not null &&
            annotatedReturnType is not null && !IsValidAsyncReturnType(annotatedReturnType))
        {
            var display = annotatedReturnType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);
            _diagnostics.ReportAsyncReturnTypeMustBeTaskLike(display, returnTypeSyntax.GetLocation());
            annotatedReturnType = Compilation.GetSpecialType(SpecialType.System_Threading_Tasks_Task);
            hasInvalidAsyncReturnType = true;
        }

        var initialReturnType = annotatedReturnType ?? Compilation.ErrorTypeSymbol;

        var lambdaSymbol = new SourceLambdaSymbol(
            parameterSymbols,
            initialReturnType,
            _containingSymbol,
            _containingSymbol.ContainingType as INamedTypeSymbol,
            _containingSymbol.ContainingNamespace,
            [syntax.GetLocation()],
            [syntax.GetReference()],
            isAsync: isAsyncLambda);

        if (hasInvalidAsyncReturnType)
            lambdaSymbol.MarkAsyncReturnTypeError();

        var lambdaBinder = new LambdaBinder(lambdaSymbol, this);

        foreach (var param in parameterSymbols)
            lambdaBinder.DeclareParameter(param);

        var bodyExpr = lambdaBinder.BindExpression(syntax.ExpressionBody, allowReturn: true);

        var inferred = bodyExpr.Type;
        var collectedReturn = ReturnTypeCollector.Infer(bodyExpr);

        if (returnTypeSyntax is null &&
            inferred is not null &&
            inferred.TypeKind != TypeKind.Error)
        {
            inferred = TypeSymbolNormalization.NormalizeForInference(inferred);
        }
        var unitType = Compilation.GetSpecialType(SpecialType.System_Unit);

        ITypeSymbol InferAsyncReturnType(ITypeSymbol? bodyType, ITypeSymbol unit)
        {
            var taskType = Compilation.GetSpecialType(SpecialType.System_Threading_Tasks_Task);

            if (bodyType is null ||
                bodyType.TypeKind == TypeKind.Error ||
                SymbolEqualityComparer.Default.Equals(bodyType, unit) ||
                bodyType.SpecialType == SpecialType.System_Void)
            {
                return taskType;
            }

            if (Compilation.GetSpecialType(SpecialType.System_Threading_Tasks_Task_T) is INamedTypeSymbol taskGeneric)
                return taskGeneric.Construct(bodyType);

            return taskType;
        }

        ITypeSymbol? ExtractAsyncResultType(ITypeSymbol asyncReturnType)
        {
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
        if (inferred is null || SymbolEqualityComparer.Default.Equals(inferred, unitType))
        {
            if (collectedReturn is not null)
                inferred = collectedReturn;
        }

        if (isAsyncLambda && inferred is IUnionTypeSymbol union)
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

        if (isAsyncLambda && (inferredAsyncReturnInput is null ||
                              inferredAsyncReturnInput.TypeKind == TypeKind.Error ||
                              SymbolEqualityComparer.Default.Equals(inferredAsyncReturnInput, unitType)))
        {
            if (collectedReturn is not null && collectedReturn.TypeKind != TypeKind.Error)
                inferredAsyncReturnInput = collectedReturn;
        }

        var inferredAsyncReturn = isAsyncLambda
            ? InferAsyncReturnType(inferredAsyncReturnInput, unitType)
            : null;

        var inferredAsyncResult = inferredAsyncReturn is not null
            ? ExtractAsyncResultType(inferredAsyncReturn)
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

            if (candidateDelegates.IsDefaultOrEmpty)
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
                    ? ExtractAsyncResultType(candidateReturn) ?? candidateReturn
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
                    if (IsAsyncDelegateReturn(candidateReturn))
                        asyncMatch ??= candidate;
                    else
                        syncMatch ??= candidate;

                    continue;
                }

                if (!IsConvertibleToExpected(expectedBody))
                    continue;

                if (IsAsyncDelegateReturn(candidateReturn))
                    asyncMatch ??= candidate;
                else
                    syncMatch ??= candidate;
            }

            if (asyncMatch is not null)
                return asyncMatch;

            if (bestAsyncByResult is not null)
                return bestAsyncByResult;

            if (syncMatch is not null)
                return syncMatch;

            return primaryDelegate ?? candidateDelegates.FirstOrDefault();
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
            expectedBodyType = ExtractAsyncResultType(returnType) ?? expectedBodyType;
        }

        if (expectedBodyType is not null &&
            expectedBodyType is not ITypeParameterSymbol &&
            inferred is not null &&
            inferred.TypeKind != TypeKind.Error &&
            expectedBodyType.TypeKind != TypeKind.Error)
        {
            if (!IsAssignable(expectedBodyType, inferred, out var conversion))
            {
                _diagnostics.ReportCannotConvertFromTypeToType(
                    inferred.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    expectedBodyType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    syntax.ExpressionBody.GetLocation());
            }
            else
            {
                bodyExpr = ApplyConversion(bodyExpr, expectedBodyType, conversion, syntax.ExpressionBody);
            }
        }

        lambdaBinder.SetLambdaBody(bodyExpr);

        var capturedVariables = lambdaBinder.AnalyzeCapturedVariables();

        if (lambdaSymbol is SourceLambdaSymbol sourceLambdaSymbol)
            sourceLambdaSymbol.SetCapturedVariables(capturedVariables);

        var delegateType = primaryDelegate is not null &&
            targetSignature is not null &&
            targetSignature.Parameters.Length == parameterSymbols.Count &&
            SymbolEqualityComparer.Default.Equals(returnType, targetSignature.ReturnType) &&
            parameterSymbols
                .Zip(targetSignature.Parameters, (parameter, target) =>
                    SymbolEqualityComparer.Default.Equals(parameter.Type, target.Type) && parameter.RefKind == target.RefKind)
                .All(match => match)
            ? primaryDelegate
            : Compilation.CreateFunctionTypeSymbol(
                parameterSymbols.Select(p => p.Type).ToArray(),
                returnType
            );

        if (lambdaSymbol is SourceLambdaSymbol mutable)
        {
            mutable.SetReturnType(returnType);
            mutable.SetDelegateType(delegateType);
        }

        if (isAsyncLambda && lambdaSymbol is SourceLambdaSymbol asyncLambda)
        {
            var containsAwait = AsyncLowerer.ContainsAwait(bodyExpr);
            asyncLambda.SetContainsAwait(containsAwait);

            if (!containsAwait)
            {
                var description = AsyncDiagnosticUtilities.GetAsyncMemberDescription(asyncLambda);
                var location = asyncKeywordToken?.GetLocation() ?? syntax.GetLocation();
                _diagnostics.ReportAsyncLacksAwait(description, location);
            }
        }

        var boundLambda = new BoundLambdaExpression(parameterSymbols, returnType, bodyExpr, lambdaSymbol, delegateType, capturedVariables, candidateDelegates);

        var suppressed = suppressedDiagnostics.ToImmutable();
        if ((!candidateDelegates.IsDefaultOrEmpty || suppressed.Length > 0) && lambdaSymbol is SourceLambdaSymbol source)
        {
            var parameters = parameterSymbols.ToImmutableArray();
            var unbound = new BoundUnboundLambda(source, syntax, parameters, candidateDelegates, suppressed);
            boundLambda.AttachUnbound(unbound);
        }

        return boundLambda;
    }

    private ImmutableArray<INamedTypeSymbol> GetLambdaDelegateTargets(LambdaExpressionSyntax syntax)
    {
        if (_lambdaDelegateTargets.TryGetValue(syntax, out var targets))
            return targets;

        return ImmutableArray<INamedTypeSymbol>.Empty;
    }

    private ImmutableArray<INamedTypeSymbol> ComputeLambdaDelegateTargets(LambdaExpressionSyntax syntax)
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

        switch (invocation.Expression)
        {
            case MemberAccessExpressionSyntax memberAccess:
                {
                    var boundMember = BindMemberAccessExpression(memberAccess);
                    if (boundMember is BoundMethodGroupExpression methodGroup)
                    {
                        extensionReceiverImplicit = methodGroup.Receiver is not null && IsExtensionReceiver(methodGroup.Receiver);
                        methods = FilterMethodsForLambda(methodGroup.Methods, parameterIndex, argumentExpression, extensionReceiverImplicit);
                    }
                    else if (boundMember is BoundMemberAccessExpression { Receiver: var receiver, Member: IMethodSymbol method })
                    {
                        extensionReceiverImplicit = receiver is not null && IsExtensionReceiver(receiver);
                        methods = ImmutableArray.Create(method);
                    }

                    break;
                }

            case MemberBindingExpressionSyntax memberBinding:
                {
                    var boundMember = BindMemberBindingExpression(memberBinding);
                    if (boundMember is BoundMethodGroupExpression methodGroup)
                    {
                        extensionReceiverImplicit = methodGroup.Receiver is not null && IsExtensionReceiver(methodGroup.Receiver);
                        methods = FilterMethodsForLambda(methodGroup.Methods, parameterIndex, argumentExpression, extensionReceiverImplicit);
                    }
                    else if (boundMember is BoundMemberAccessExpression { Receiver: var receiver, Member: IMethodSymbol method })
                    {
                        extensionReceiverImplicit = receiver is not null && IsExtensionReceiver(receiver);
                        methods = ImmutableArray.Create(method);
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
                        methods = FilterMethodsForLambda(accessible, parameterIndex, argumentExpression, extensionReceiverImplicit: false);
                    }

                    break;
                }
        }

        if (methods.IsDefaultOrEmpty)
            return ImmutableArray<INamedTypeSymbol>.Empty;

        var delegates = ExtractLambdaDelegates(methods, parameterIndex, extensionReceiverImplicit);
        if (!delegates.IsDefaultOrEmpty)
            _lambdaDelegateTargets[syntax] = delegates;
        else
            _lambdaDelegateTargets.Remove(syntax);
        return delegates;
    }
    private ImmutableArray<INamedTypeSymbol> ExtractLambdaDelegates(
        ImmutableArray<IMethodSymbol> methods,
        int parameterIndex,
        bool extensionReceiverImplicit)
    {
        if (methods.IsDefaultOrEmpty)
            return ImmutableArray<INamedTypeSymbol>.Empty;

        var builder = ImmutableArray.CreateBuilder<INamedTypeSymbol>();

        foreach (var method in methods)
        {
            if (!TryGetLambdaParameter(method, parameterIndex, extensionReceiverImplicit, out var parameter))
                continue;

            if (parameter.Type is INamedTypeSymbol delegateType && delegateType.TypeKind == TypeKind.Delegate)
            {
                if (!builder.Any(existing => SymbolEqualityComparer.Default.Equals(existing, delegateType)))
                    builder.Add(delegateType);
            }
        }

        return builder.ToImmutable();
    }

    private void RecordLambdaTargets(
        ExpressionSyntax argumentExpression,
        ImmutableArray<IMethodSymbol> methods,
        int parameterIndex,
        bool extensionReceiverImplicit = false)
    {
        if (argumentExpression is not LambdaExpressionSyntax lambda)
        {
            if (argumentExpression is LambdaExpressionSyntax lambdaExpression)
                _lambdaDelegateTargets.Remove(lambdaExpression);
            return;
        }

        if (methods.IsDefaultOrEmpty)
            return;

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

            if (parameter.Type is INamedTypeSymbol delegateType && delegateType.TypeKind == TypeKind.Delegate)
            {
                if (seen.Add(delegateType))
                    builder.Add(delegateType);
            }
        }

        if (builder.Count > 0)
            _lambdaDelegateTargets[lambda] = builder.ToImmutable();
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
        bool extensionReceiverImplicit)
    {
        if (methods.IsDefaultOrEmpty)
            return methods;

        if (argumentExpression is not LambdaExpressionSyntax lambda)
            return methods;

        int parameterCount = lambda switch
        {
            SimpleLambdaExpressionSyntax => 1,
            ParenthesizedLambdaExpressionSyntax p => p.ParameterList.Parameters.Count,
            _ => 0
        };

        var builder = ImmutableArray.CreateBuilder<IMethodSymbol>();

        foreach (var method in methods)
        {
            if (!TryGetLambdaParameter(method, parameterIndex, extensionReceiverImplicit, out var parameter))
                continue;

            var parameterType = parameter.Type;
            if (parameterType is INamedTypeSymbol delegateType && delegateType.TypeKind == TypeKind.Delegate)
            {
                var invoke = delegateType.GetDelegateInvokeMethod();
                if (invoke is null)
                    continue;

                if (invoke.Parameters.Length != parameterCount)
                    continue;
            }

            builder.Add(method);
        }

        return builder.Count == methods.Length ? methods : builder.ToImmutable();
    }

    private bool EnsureLambdaCompatible(IParameterSymbol parameter, BoundLambdaExpression lambda)
    {
        if (parameter.Type is not INamedTypeSymbol delegateType)
            return false;

        return ReplayLambda(lambda, delegateType) is not null;
    }

    private BoundLambdaExpression? ReplayLambda(BoundLambdaExpression lambda, INamedTypeSymbol delegateType)
    {
        var instrumentation = Compilation.PerformanceInstrumentation.LambdaReplay;
        instrumentation.RecordReplayAttempt();

        if (delegateType.TypeKind != TypeKind.Delegate)
        {
            instrumentation.RecordReplayFailure();
            return null;
        }

        var syntax = GetLambdaSyntax(lambda);

        if (syntax is not null)
        {
            var key = new LambdaRebindKey(syntax, delegateType);
            if (_reboundLambdaCache.TryGetValue(key, out var cached))
            {
                instrumentation.RecordCacheHit();
                instrumentation.RecordReplaySuccess();
                return cached;
            }

            instrumentation.RecordCacheMiss();
        }

        BoundLambdaExpression? rebound;
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
            _reboundLambdaCache[new LambdaRebindKey(syntax, delegateType)] = rebound;

        return rebound;
    }

    private BoundLambdaExpression? ReplayLambda(BoundUnboundLambda unbound, INamedTypeSymbol delegateType)
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
            SimpleLambdaExpressionSyntax simple => new[] { simple.Parameter },
            ParenthesizedLambdaExpressionSyntax parenthesized => parenthesized.ParameterList.Parameters.ToArray(),
            _ => Array.Empty<ParameterSyntax>()
        };

        if (invoke.Parameters.Length != parameterSyntaxes.Length)
        {
            instrumentation.RecordBindingFailure();
            return null;
        }

        var parameterSymbols = new List<IParameterSymbol>(parameterSyntaxes.Length);

        for (int index = 0; index < parameterSyntaxes.Length; index++)
        {
            var parameterSyntax = parameterSyntaxes[index];
            var delegateParameter = invoke.Parameters[index];
            var isMutable = parameterSyntax.BindingKeyword?.Kind == SyntaxKind.VarKeyword;
            var parameterSymbol = new SourceParameterSymbol(
                parameterSyntax.Identifier.ValueText,
                delegateParameter.Type,
                _containingSymbol,
                _containingSymbol.ContainingType as INamedTypeSymbol,
                _containingSymbol.ContainingNamespace,
                [parameterSyntax.GetLocation()],
                [parameterSyntax.GetReference()],
                delegateParameter.RefKind,
                isMutable: isMutable);

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

        var lambdaBinder = new LambdaBinder(lambdaSymbol, this);

        foreach (var parameter in parameterSymbols)
            lambdaBinder.DeclareParameter(parameter);

        var body = lambdaBinder.BindExpression(syntax.ExpressionBody, allowReturn: true);
        var inferred = body.Type ?? ReturnTypeCollector.Infer(body);
        if (inferred is not null && inferred.TypeKind != TypeKind.Error)
            inferred = TypeSymbolNormalization.NormalizeForInference(inferred);
        var returnType = invoke.ReturnType;
        var unitType = Compilation.GetSpecialType(SpecialType.System_Unit);

        ITypeSymbol? ExtractAsyncResultTypeForReplay(ITypeSymbol asyncReturnType)
        {
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

        var shouldApplyConversion =
            inferred is not null &&
            inferred.TypeKind != TypeKind.Error &&
            expectedBodyType is not null &&
            expectedBodyType.TypeKind != TypeKind.Error &&
            expectedBodyType is not ITypeParameterSymbol;

        if (shouldApplyConversion)
        {
            if (!IsAssignable(expectedBodyType, inferred, out var conversion))
            {
                instrumentation.RecordBindingFailure();
                return null;
            }

            body = ApplyConversion(body, expectedBodyType, conversion, syntax.ExpressionBody);
        }

        lambdaBinder.SetLambdaBody(body);
        var captured = lambdaBinder.AnalyzeCapturedVariables();

        var capturedSet = new HashSet<ISymbol>(captured, SymbolEqualityComparer.Default);
        foreach (var nestedCapture in LambdaSelfCaptureCollector.Collect(body, lambdaSymbol))
            capturedSet.Add(nestedCapture);

        var capturedVariables = capturedSet.ToImmutableArray();

        lambdaSymbol.SetCapturedVariables(capturedVariables);
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

        var rebound = new BoundLambdaExpression(
            parameterSymbols,
            returnType,
            body,
            lambdaSymbol,
            delegateType,
            capturedVariables,
            candidateDelegates);
        rebound.AttachUnbound(unbound);
        instrumentation.RecordBindingSuccess();
        return rebound;
    }

    private LambdaExpressionSyntax? GetLambdaSyntax(BoundLambdaExpression lambda)
    {
        if (lambda.Unbound is { Syntax: { } syntax })
            return syntax;

        if (lambda.Symbol is ILambdaSymbol lambdaSymbol)
        {
            foreach (var reference in lambdaSymbol.DeclaringSyntaxReferences)
            {
                if (reference.GetSyntax() is LambdaExpressionSyntax lambdaSyntax)
                    return lambdaSyntax;
            }
        }

        return null;
    }

    private void ReportSuppressedLambdaDiagnostics(IEnumerable<BoundArgument> arguments)
    {
        foreach (var argument in arguments)
        {
            if (argument.Expression is BoundLambdaExpression { Unbound: { } unbound })
                unbound.ReportSuppressedDiagnostics(_diagnostics);
        }
    }
}
