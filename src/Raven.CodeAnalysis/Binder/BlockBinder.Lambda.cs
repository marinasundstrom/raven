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

            if (typeSyntax is ByRefTypeSyntax)
                refKind = parameterSyntax.Modifiers.Any(m => m.Kind == SyntaxKind.OutKeyword) ? RefKind.Out : RefKind.Ref;

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

            var symbol = new SourceParameterSymbol(
                parameterSyntax.Identifier.ValueText,
                parameterType,
                _containingSymbol,
                _containingSymbol.ContainingType as INamedTypeSymbol,
                _containingSymbol.ContainingNamespace,
                [parameterSyntax.GetLocation()],
                [parameterSyntax.GetReference()],
                refKind
            );

            parameterSymbols.Add(symbol);
        }

        TypeSyntax? returnTypeSyntax = syntax switch
        {
            SimpleLambdaExpressionSyntax s => s.ReturnType?.Type,
            ParenthesizedLambdaExpressionSyntax p => p.ReturnType?.Type,
            _ => null
        };

        var inferredReturnType = returnTypeSyntax is not null
            ? ResolveType(returnTypeSyntax)
            : Compilation.ErrorTypeSymbol;

        var lambdaSymbol = new SourceLambdaSymbol(
            parameterSymbols,
            inferredReturnType,
            _containingSymbol,
            _containingSymbol.ContainingType as INamedTypeSymbol,
            _containingSymbol.ContainingNamespace,
            [syntax.GetLocation()],
            [syntax.GetReference()]);

        var lambdaBinder = new LambdaBinder(lambdaSymbol, this);

        foreach (var param in parameterSymbols)
            lambdaBinder.DeclareParameter(param);

        var bodyExpr = lambdaBinder.BindExpression(syntax.ExpressionBody, allowReturn: true);

        var inferred = bodyExpr.Type;
        if (returnTypeSyntax is null &&
            inferred is not null &&
            inferred.TypeKind != TypeKind.Error)
        {
            inferred = TypeSymbolNormalization.NormalizeForInference(inferred);
        }
        var unitType = Compilation.GetSpecialType(SpecialType.System_Unit);
        if (inferred is null || SymbolEqualityComparer.Default.Equals(inferred, unitType))
        {
            var collected = ReturnTypeCollector.Infer(bodyExpr);
            if (collected is not null)
                inferred = collected;
        }

        ITypeSymbol returnType;
        if (returnTypeSyntax is not null)
        {
            returnType = inferredReturnType;
            if (inferred is not null &&
                inferred.TypeKind != TypeKind.Error &&
                returnType.TypeKind != TypeKind.Error)
            {
                if (!IsAssignable(returnType, inferred, out var conversion))
                {
                    _diagnostics.ReportCannotConvertFromTypeToType(
                        inferred.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        returnType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        syntax.ExpressionBody.GetLocation());
                }
                else
                {
                    bodyExpr = ApplyConversion(bodyExpr, returnType, conversion, syntax.ExpressionBody);
                }
            }
        }
        else if (targetSignature is { ReturnType: { } targetReturn } && targetReturn.TypeKind != TypeKind.Error)
        {
            returnType = targetReturn;
            if (inferred is not null &&
                inferred.TypeKind != TypeKind.Error &&
                returnType.TypeKind != TypeKind.Error)
            {
                if (!IsAssignable(returnType, inferred, out var conversion))
                {
                    _diagnostics.ReportCannotConvertFromTypeToType(
                        inferred.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        returnType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        syntax.ExpressionBody.GetLocation());
                }
                else
                {
                    bodyExpr = ApplyConversion(bodyExpr, returnType, conversion, syntax.ExpressionBody);
                }
            }
        }
        else
        {
            returnType = inferred ?? Compilation.ErrorTypeSymbol;
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
            var parameterSymbol = new SourceParameterSymbol(
                parameterSyntax.Identifier.ValueText,
                delegateParameter.Type,
                _containingSymbol,
                _containingSymbol.ContainingType as INamedTypeSymbol,
                _containingSymbol.ContainingNamespace,
                [parameterSyntax.GetLocation()],
                [parameterSyntax.GetReference()],
                delegateParameter.RefKind);

            parameterSymbols.Add(parameterSymbol);
        }

        var lambdaSymbol = new SourceLambdaSymbol(
            parameterSymbols,
            invoke.ReturnType,
            _containingSymbol,
            _containingSymbol.ContainingType as INamedTypeSymbol,
            _containingSymbol.ContainingNamespace,
            [syntax.GetLocation()],
            [syntax.GetReference()]);

        var lambdaBinder = new LambdaBinder(lambdaSymbol, this);

        foreach (var parameter in parameterSymbols)
            lambdaBinder.DeclareParameter(parameter);

        var body = lambdaBinder.BindExpression(syntax.ExpressionBody, allowReturn: true);
        var inferred = body.Type ?? ReturnTypeCollector.Infer(body);
        if (inferred is not null && inferred.TypeKind != TypeKind.Error)
            inferred = TypeSymbolNormalization.NormalizeForInference(inferred);
        var returnType = invoke.ReturnType;

        if (inferred is not null &&
            inferred.TypeKind != TypeKind.Error &&
            returnType.TypeKind != TypeKind.Error)
        {
            if (!IsAssignable(returnType, inferred, out var conversion))
            {
                instrumentation.RecordBindingFailure();
                return null;
            }

            body = ApplyConversion(body, returnType, conversion, syntax.ExpressionBody);
        }

        lambdaBinder.SetLambdaBody(body);
        var captured = lambdaBinder.AnalyzeCapturedVariables();

        lambdaSymbol.SetCapturedVariables(captured);
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
            captured,
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

    private void ReportSuppressedLambdaDiagnostics(IEnumerable<BoundExpression> arguments)
    {
        foreach (var argument in arguments)
        {
            if (argument is BoundLambdaExpression { Unbound: { } unbound })
                unbound.ReportSuppressedDiagnostics(_diagnostics);
        }
    }
}
