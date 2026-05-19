using System.Collections.Immutable;
using System.Diagnostics.CodeAnalysis;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Operations;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.LanguageServer;

internal static partial class SymbolResolver
{
    // Invocation-oriented resolution handles:
    // - direct call targets
    // - constructor/type projection at call sites
    // - contextual union-case invocations such as Ok(...) / Error(...)
    private static bool TryResolveInvocationTargetSymbol(
        SemanticModel semanticModel,
        SyntaxNode node,
        SyntaxToken token,
        out ISymbol? symbol)
    {
        symbol = null;

        var invocation = node switch
        {
            InvocationExpressionSyntax direct => direct,
            IdentifierNameSyntax { Parent: InvocationExpressionSyntax direct } => direct,
            MemberBindingExpressionSyntax { Parent: InvocationExpressionSyntax direct } => direct,
            IdentifierNameSyntax identifier
                when identifier.Parent is MemberBindingExpressionSyntax memberBinding &&
                     HaveEquivalentSpan(memberBinding.Name, identifier) &&
                     memberBinding.Parent is InvocationExpressionSyntax parent => parent,
            MemberAccessExpressionSyntax { Parent: InvocationExpressionSyntax parent } => parent,
            IdentifierNameSyntax identifier
                when identifier.Parent is MemberAccessExpressionSyntax memberAccess &&
                     HaveEquivalentSpan(memberAccess.Name, identifier) &&
                     memberAccess.Parent is InvocationExpressionSyntax parent => parent,
            _ => null
        };

        if (invocation is null)
            return false;

        // Only resolve invocation targets when hovering the call target itself,
        // not arguments/parameter lists/lambda bodies.
        if (node is InvocationExpressionSyntax && !invocation.Expression.Span.Contains(token.Span))
            return false;

        if (!IsInvocationTargetMatch(invocation.Expression, node, token))
            return false;

        if (TryResolvePipeBoundInvocationTargetSymbol(semanticModel, invocation, out var pipeBoundSymbol))
        {
            symbol = pipeBoundSymbol;
            return true;
        }

        if (invocation.Expression is MemberBindingExpressionSyntax invocationMemberBinding &&
            semanticModel.GetSymbolInfo(invocationMemberBinding) is var memberBindingInfo &&
            (memberBindingInfo.Symbol is not null || !memberBindingInfo.CandidateSymbols.IsDefaultOrEmpty))
        {
            if (memberBindingInfo.Symbol is INamedTypeSymbol memberBindingType &&
                TryChooseConstructorForInvocation(memberBindingType, invocation, out var memberBindingConstructor))
            {
                symbol = memberBindingConstructor;
                return true;
            }

            if (memberBindingInfo.Symbol is not null)
            {
                var projectedSymbol = ProjectInvocationSymbolForDisplay(memberBindingInfo.Symbol, semanticModel, invocation);
                if (projectedSymbol is not ILocalSymbol &&
                    !IsUnitTypeSymbol(projectedSymbol))
                {
                    symbol = projectedSymbol;
                    return true;
                }
            }

            if (!memberBindingInfo.CandidateSymbols.IsDefaultOrEmpty)
            {
                foreach (var candidateType in memberBindingInfo.CandidateSymbols.OfType<INamedTypeSymbol>())
                {
                    if (TryChooseConstructorForInvocation(candidateType, invocation, out var candidateConstructor))
                    {
                        symbol = candidateConstructor;
                        return true;
                    }
                }

                var methodCandidates = memberBindingInfo.CandidateSymbols.OfType<IMethodSymbol>().ToImmutableArray();
                var candidate = methodCandidates.Length == 1
                    ? methodCandidates[0]
                    : SemanticModel.TryChooseInvocationMethodCandidate(
                        methodCandidates,
                        invocation,
                        SemanticModel.InvocationCandidateFallback.None);
                if (candidate is not null &&
                    ProjectInvocationSymbolForDisplay(candidate, semanticModel, invocation) is { } projectedCandidate &&
                    projectedCandidate is not ILocalSymbol &&
                    !IsUnitTypeSymbol(projectedCandidate))
                {
                    symbol = projectedCandidate;
                    return true;
                }
            }
        }

        if (TryResolveUnionCaseFromInvocationContext(semanticModel, invocation, out var unionCaseSymbol))
        {
            symbol = unionCaseSymbol;
            return true;
        }

        if (TryGetSymbolInfo(semanticModel, invocation, out var symbolInfo))
        {
            if (symbolInfo.Symbol is INamedTypeSymbol invocationType &&
                TryChooseConstructorForInvocation(invocationType, invocation, out var invocationConstructor))
            {
                symbol = invocationConstructor;
                return true;
            }

            if (symbolInfo.Symbol is not null)
            {
                var projectedSymbol = ProjectInvocationSymbolForDisplay(symbolInfo.Symbol, semanticModel, invocation);
                if (projectedSymbol is not ILocalSymbol &&
                    !IsUnitTypeSymbol(projectedSymbol))
                {
                    symbol = projectedSymbol;
                    return true;
                }
            }

            if (!symbolInfo.CandidateSymbols.IsDefaultOrEmpty)
            {
                foreach (var candidateType in symbolInfo.CandidateSymbols.OfType<INamedTypeSymbol>())
                {
                    if (TryChooseConstructorForInvocation(candidateType, invocation, out var candidateConstructor))
                    {
                        symbol = candidateConstructor;
                        return true;
                    }
                }

                var methodCandidates = symbolInfo.CandidateSymbols.OfType<IMethodSymbol>().ToImmutableArray();
                var candidate = methodCandidates.Length == 1
                    ? methodCandidates[0]
                    : SemanticModel.TryChooseInvocationMethodCandidate(
                        methodCandidates,
                        invocation,
                        SemanticModel.InvocationCandidateFallback.None);
                if (candidate is not null &&
                    ProjectInvocationSymbolForDisplay(candidate, semanticModel, invocation) is { } projectedCandidate &&
                    projectedCandidate is not ILocalSymbol &&
                    !IsUnitTypeSymbol(projectedCandidate))
                {
                    symbol = projectedCandidate;
                    return true;
                }
            }
        }

        if (TryResolveConstructorFromInvocationExpressionSymbolInfo(semanticModel, invocation, out var constructorSymbol))
        {
            symbol = constructorSymbol;
            return true;
        }

        if (TryResolveInvocationTargetTypeFromTypeInfo(semanticModel, invocation, out var targetType))
        {
            symbol = TryResolveConstructorFromTargetType(invocation, targetType, out var targetTypeConstructor)
                ? targetTypeConstructor
                : targetType;
            return true;
        }

        if (TryResolveInvocationTargetTypeFromArgumentContext(semanticModel, invocation, out targetType))
        {
            symbol = TryResolveConstructorFromTargetType(invocation, targetType, out var targetTypeConstructor)
                ? targetTypeConstructor
                : targetType;
            return true;
        }

        // When this invocation is the right-hand side of a pipe expression the binder
        // resolves overloads using the piped value as an implicit first argument and
        // stores the constructed symbol on the BinaryExpression node, not on the
        // InvocationExpression itself.
        return false;
    }

    private static bool TryResolveConstructorFromTargetType(
        InvocationExpressionSyntax invocation,
        ITypeSymbol targetType,
        [NotNullWhen(true)] out IMethodSymbol? constructor)
    {
        constructor = null;

        if (targetType is not INamedTypeSymbol namedType)
            return false;

        var invokedName = invocation.Expression switch
        {
            SimpleNameSyntax simpleName => simpleName.Identifier.ValueText,
            MemberAccessExpressionSyntax { Name: SimpleNameSyntax simpleName } => simpleName.Identifier.ValueText,
            _ => null
        };

        if (!string.Equals(invokedName, namedType.Name, StringComparison.Ordinal))
            return false;

        return TryChooseConstructorForInvocation(namedType, invocation, out constructor);
    }

    private static bool TryChooseConstructorForInvocation(
        INamedTypeSymbol type,
        InvocationExpressionSyntax invocation,
        [NotNullWhen(true)] out IMethodSymbol? constructor)
    {
        constructor = type.Constructors.FirstOrDefault(candidate =>
            candidate.Parameters.Length == invocation.ArgumentList.Arguments.Count);
        constructor ??= type.Constructors.FirstOrDefault();
        return constructor is not null;
    }

    private static bool IsUnitTypeSymbol(ISymbol symbol)
        => symbol is ITypeSymbol type &&
           (type.SpecialType == SpecialType.System_Unit ||
            string.Equals(type.Name, "Unit", StringComparison.Ordinal));

    private static bool TryResolvePipeBoundInvocationTargetSymbol(
        SemanticModel semanticModel,
        InvocationExpressionSyntax invocation,
        [NotNullWhen(true)] out ISymbol? symbol)
    {
        symbol = null;

        var pipeExpr = invocation.Ancestors()
            .OfType<InfixOperatorExpressionSyntax>()
            .FirstOrDefault(pipe => pipe.Kind == SyntaxKind.PipeExpression && IsPipeRightExpressionForInvocation(pipe.Right, invocation));
        if (pipeExpr is null)
            return false;

        if (TryGetSymbolInfo(semanticModel, pipeExpr, out var pipeInfo))
        {
            if (pipeInfo.Symbol is not null)
            {
                var projectedSymbol = ProjectInvocationSymbolForDisplay(pipeInfo.Symbol, semanticModel, invocation);
                if (projectedSymbol is not ILocalSymbol &&
                    !IsUnitTypeSymbol(projectedSymbol))
                {
                    symbol = projectedSymbol;
                    return true;
                }
            }

            if (!pipeInfo.CandidateSymbols.IsDefaultOrEmpty)
            {
                var methodCandidates = pipeInfo.CandidateSymbols.OfType<IMethodSymbol>().ToImmutableArray();
                var candidate = methodCandidates.Length == 1
                    ? methodCandidates[0]
                    : SemanticModel.TryChooseInvocationMethodCandidate(
                        methodCandidates,
                        invocation,
                        SemanticModel.InvocationCandidateFallback.None);
                if (candidate is not null &&
                    ProjectInvocationSymbolForDisplay(candidate, semanticModel, invocation) is { } projectedCandidate &&
                    projectedCandidate is not ILocalSymbol &&
                    !IsUnitTypeSymbol(projectedCandidate))
                {
                    symbol = projectedCandidate;
                    return true;
                }
            }
        }

        if (TryResolveContainingTypePipeTargetMethod(semanticModel, invocation, out var containingTypeMethod))
        {
            symbol = ProjectInvocationSymbolForDisplay(containingTypeMethod, semanticModel, invocation);
            return true;
        }

        return false;
    }

    private static bool TryResolveContainingTypePipeTargetMethod(
        SemanticModel semanticModel,
        InvocationExpressionSyntax invocation,
        [NotNullWhen(true)] out IMethodSymbol? method)
    {
        method = null;

        var methodName = invocation.Expression switch
        {
            IdentifierNameSyntax identifier => identifier.Identifier.ValueText,
            MemberAccessExpressionSyntax { Name: IdentifierNameSyntax identifier } => identifier.Identifier.ValueText,
            _ => null
        };
        if (string.IsNullOrWhiteSpace(methodName))
            return false;

        var containingTypeSyntax = invocation.Ancestors().OfType<TypeDeclarationSyntax>().FirstOrDefault();
        if (containingTypeSyntax is null ||
            semanticModel.GetDeclaredSymbol(containingTypeSyntax) is not INamedTypeSymbol containingType)
        {
            return false;
        }

        method = containingType
            .GetMembers(methodName)
            .OfType<IMethodSymbol>()
            .Where(candidate => candidate.Parameters.Length == invocation.ArgumentList.Arguments.Count + 1)
            .OrderByDescending(static candidate => candidate.IsStatic)
            .FirstOrDefault();
        return method is not null;
    }

    private static bool TryResolveInvocationExpressionSymbol(
        SemanticModel semanticModel,
        InvocationExpressionSyntax invocation,
        [NotNullWhen(true)] out ISymbol? symbol)
    {
        symbol = null;

        switch (invocation.Expression)
        {
            case IdentifierNameSyntax identifier:
                {
                    if (TryGetSymbolInfo(semanticModel, identifier, out var identifierInfo))
                    {
                        if (identifierInfo.Symbol is INamedTypeSymbol namedType &&
                            TryChooseConstructorForInvocation(namedType, invocation, out var constructor))
                        {
                            symbol = constructor;
                            return true;
                        }

                        foreach (var candidateType in identifierInfo.CandidateSymbols.OfType<INamedTypeSymbol>())
                        {
                            if (TryChooseConstructorForInvocation(candidateType, invocation, out var candidateConstructor))
                            {
                                symbol = candidateConstructor;
                                return true;
                            }
                        }

                        if (identifierInfo.Symbol is not null)
                        {
                            var projectedSymbol = ProjectInvocationSymbolForDisplay(identifierInfo.Symbol, semanticModel, invocation);
                            if (projectedSymbol is not ILocalSymbol &&
                                !IsUnitTypeSymbol(projectedSymbol))
                            {
                                symbol = projectedSymbol;
                                return true;
                            }
                        }

                        if (!identifierInfo.CandidateSymbols.IsDefaultOrEmpty)
                        {
                            var methodCandidates = identifierInfo.CandidateSymbols.OfType<IMethodSymbol>().ToImmutableArray();
                            var candidate = methodCandidates.Length == 1
                                ? methodCandidates[0]
                                : SemanticModel.TryChooseInvocationMethodCandidate(
                                    methodCandidates,
                                    invocation,
                                    SemanticModel.InvocationCandidateFallback.None);
                            if (candidate is not null &&
                                ProjectInvocationSymbolForDisplay(candidate, semanticModel, invocation) is { } projectedCandidate &&
                                projectedCandidate is not ILocalSymbol &&
                                !IsUnitTypeSymbol(projectedCandidate))
                            {
                                symbol = projectedCandidate;
                                return true;
                            }
                        }
                    }

                    break;
                }

            case MemberAccessExpressionSyntax memberAccess:
                {
                    if (TryGetSymbolInfo(semanticModel, memberAccess.Name, out var memberInfo))
                    {
                        if (memberInfo.Symbol is INamedTypeSymbol namedType &&
                            TryChooseConstructorForInvocation(namedType, invocation, out var constructor))
                        {
                            symbol = constructor;
                            return true;
                        }

                        foreach (var candidateType in memberInfo.CandidateSymbols.OfType<INamedTypeSymbol>())
                        {
                            if (TryChooseConstructorForInvocation(candidateType, invocation, out var candidateConstructor))
                            {
                                symbol = candidateConstructor;
                                return true;
                            }
                        }

                        if (memberInfo.Symbol is not null)
                        {
                            var projectedSymbol = ProjectInvocationSymbolForDisplay(memberInfo.Symbol, semanticModel, invocation);
                            if (projectedSymbol is not ILocalSymbol &&
                                !IsUnitTypeSymbol(projectedSymbol))
                            {
                                symbol = projectedSymbol;
                                return true;
                            }
                        }

                        if (!memberInfo.CandidateSymbols.IsDefaultOrEmpty)
                        {
                            var methodCandidates = memberInfo.CandidateSymbols.OfType<IMethodSymbol>().ToImmutableArray();
                            var candidate = methodCandidates.Length == 1
                                ? methodCandidates[0]
                                : SemanticModel.TryChooseInvocationMethodCandidate(
                                    methodCandidates,
                                    invocation,
                                    SemanticModel.InvocationCandidateFallback.None);
                            if (candidate is not null &&
                                ProjectInvocationSymbolForDisplay(candidate, semanticModel, invocation) is { } projectedCandidate &&
                                projectedCandidate is not ILocalSymbol &&
                                !IsUnitTypeSymbol(projectedCandidate))
                            {
                                symbol = projectedCandidate;
                                return true;
                            }
                        }
                    }

                    break;
                }
        }

        if (TryGetSymbolInfo(semanticModel, invocation.Expression, out var expressionInfo))
        {
            if (expressionInfo.Symbol is not null)
            {
                var projectedSymbol = ProjectInvocationSymbolForDisplay(expressionInfo.Symbol, semanticModel, invocation);
                if (projectedSymbol is not ILocalSymbol &&
                    !IsUnitTypeSymbol(projectedSymbol))
                {
                    symbol = projectedSymbol;
                    return true;
                }
            }

            if (!expressionInfo.CandidateSymbols.IsDefaultOrEmpty)
            {
                var methodCandidates = expressionInfo.CandidateSymbols.OfType<IMethodSymbol>().ToImmutableArray();
                var candidate = methodCandidates.Length == 1
                    ? methodCandidates[0]
                    : SemanticModel.TryChooseInvocationMethodCandidate(
                        methodCandidates,
                        invocation,
                        SemanticModel.InvocationCandidateFallback.None);
                if (candidate is not null &&
                    ProjectInvocationSymbolForDisplay(candidate, semanticModel, invocation) is { } projectedCandidate &&
                    projectedCandidate is not ILocalSymbol &&
                    !IsUnitTypeSymbol(projectedCandidate))
                {
                    symbol = projectedCandidate;
                    return true;
                }
            }
        }

        return false;
    }

    private static bool TryResolveConstructorFromInvocationExpressionSymbolInfo(
        SemanticModel semanticModel,
        InvocationExpressionSyntax invocation,
        [NotNullWhen(true)] out IMethodSymbol? constructor)
    {
        constructor = null;

        if (!TryGetSymbolInfo(semanticModel, invocation.Expression, out var expressionInfo))
            return false;

        if (expressionInfo.Symbol is INamedTypeSymbol namedType &&
            TryChooseConstructorForInvocation(namedType, invocation, out constructor))
        {
            return true;
        }

        foreach (var candidateType in expressionInfo.CandidateSymbols.OfType<INamedTypeSymbol>())
        {
            if (TryChooseConstructorForInvocation(candidateType, invocation, out constructor))
                return true;
        }

        return false;
    }

    private static ISymbol ProjectInvocationSymbolForDisplay(
        ISymbol symbol,
        SemanticModel semanticModel,
        InvocationExpressionSyntax invocation)
    {
        return symbol;
    }

    private static bool TryResolveUnionCaseFromInvocationContext(
        SemanticModel semanticModel,
        InvocationExpressionSyntax invocation,
        out ISymbol? symbol)
    {
        symbol = null;

        var invokedName = invocation.Expression switch
        {
            IdentifierNameSyntax identifier => identifier.Identifier.ValueText,
            MemberAccessExpressionSyntax { Name: IdentifierNameSyntax identifier } => identifier.Identifier.ValueText,
            _ => null
        };

        if (string.IsNullOrWhiteSpace(invokedName))
            return false;

        if (!TryResolveContextualUnionTargetType(invocation, semanticModel, out var targetType))
        {
            return false;
        }

        if (targetType is IUnionSymbol union && targetType.IsUnion)
        {
            var unionCase = union.CaseTypes.FirstOrDefault(c => string.Equals(c.Name, invokedName, StringComparison.Ordinal));
            if (unionCase is not null)
            {
                symbol = unionCase;
                return true;
            }
        }

        if (targetType is IUnionCaseTypeSymbol caseType &&
            targetType.IsUnionCase &&
            string.Equals(caseType.Name, invokedName, StringComparison.Ordinal))
        {
            symbol = caseType;
            return true;
        }

        return false;
    }

    private static bool TryResolveContextualUnionTargetType(
        InvocationExpressionSyntax invocation,
        SemanticModel semanticModel,
        [NotNullWhen(true)] out ITypeSymbol? targetType)
    {
        targetType = null;

        if (invocation.AncestorsAndSelf().OfType<FunctionStatementSyntax>().FirstOrDefault() is { } functionStatement &&
            semanticModel.GetDeclaredSymbol(functionStatement) is IMethodSymbol { ReturnType: { } functionReturnType } &&
            (functionReturnType.IsUnion || functionReturnType.IsUnionCase))
        {
            targetType = functionReturnType;
            return true;
        }

        return false;
    }

    private static bool TryResolveInvocationTargetTypeFromTypeInfo(
        SemanticModel semanticModel,
        InvocationExpressionSyntax invocation,
        [NotNullWhen(true)] out ITypeSymbol? targetType)
    {
        targetType = null;

        var typeInfo = semanticModel.GetTypeInfo(invocation);
        var convertedType = typeInfo.ConvertedType;
        if (convertedType is null ||
            convertedType.TypeKind == TypeKind.Error)
        {
            return false;
        }

        if (IsTargetTypedConstructorBinding(invocation.Expression))
        {
            targetType = convertedType;
            return true;
        }

        if (SymbolEqualityComparer.Default.Equals(typeInfo.Type, convertedType))
            return false;

        var naturalUnionCase = typeInfo.Type?.TryGetUnionCase();
        var convertedUnion = convertedType.TryGetUnion();
        if (naturalUnionCase is null || convertedUnion is null)
            return false;

        targetType = convertedType;
        return true;
    }

    private static bool TryResolveInvocationTargetTypeFromArgumentContext(
        SemanticModel semanticModel,
        InvocationExpressionSyntax invocation,
        [NotNullWhen(true)] out ITypeSymbol? targetType)
    {
        targetType = null;

        if (invocation.Parent is not ArgumentSyntax argument ||
            argument.Parent is not ArgumentListSyntax argumentList ||
            argumentList.Parent is not InvocationExpressionSyntax outerInvocation)
        {
            return false;
        }

        var invokedName = invocation.Expression switch
        {
            IdentifierNameSyntax identifier => identifier.Identifier.ValueText,
            MemberBindingExpressionSyntax memberBinding => memberBinding.Name.Identifier.ValueText,
            _ => null
        };

        var parameter = TryGetInvocationParameter(semanticModel, outerInvocation, argumentList.Arguments, argument);
        if (parameter?.Type is not { TypeKind: not TypeKind.Error } parameterType)
            return false;

        var typeInfo = semanticModel.GetTypeInfo(invocation);
        if (typeInfo.ConvertedType is { TypeKind: not TypeKind.Error } convertedType &&
            !SymbolEqualityComparer.Default.Equals(typeInfo.Type, convertedType))
        {
            targetType = convertedType;
            return true;
        }

        if (IsTargetTypedConstructorBinding(invocation.Expression))
        {
            targetType = parameterType;
            return true;
        }

        if (string.IsNullOrWhiteSpace(invokedName))
            return false;

        if (!ContainsUnionCaseNamed(parameterType, invokedName))
            return false;

        targetType = parameterType;
        return true;
    }

    private static IParameterSymbol? TryGetInvocationParameter(
        SemanticModel semanticModel,
        InvocationExpressionSyntax invocation,
        SeparatedSyntaxList<ArgumentSyntax> arguments,
        ArgumentSyntax argument)
    {
        var symbol = semanticModel.GetSymbolInfo(invocation).Symbol;
        var method = symbol as IMethodSymbol;
        if (method is null &&
            invocation.Expression is TypeSyntax typeSyntax &&
            semanticModel.GetTypeInfo(typeSyntax).Type is INamedTypeSymbol namedType)
        {
            method = namedType.Constructors.FirstOrDefault();
        }
        if (method is null &&
            semanticModel.GetSymbolInfo(invocation.Expression).Symbol is INamedTypeSymbol expressionType)
        {
            method = expressionType.Constructors.FirstOrDefault();
        }

        if (method is null)
            return null;

        if (argument.NameColon?.Name.Identifier.ValueText is { Length: > 0 } argumentName)
        {
            return method.Parameters.FirstOrDefault(parameter =>
                string.Equals(parameter.Name, argumentName, StringComparison.OrdinalIgnoreCase));
        }

        for (var i = 0; i < arguments.Count; i++)
        {
            if (arguments[i].Span == argument.Span && i < method.Parameters.Length)
                return method.Parameters[i];
        }

        return null;
    }

    private static bool ContainsUnionCaseNamed(ITypeSymbol type, string caseName)
    {
        var union = type.TryGetUnion() ?? type.TryGetUnionCase()?.Union;
        return union?.CaseTypes.Any(caseType => string.Equals(caseType.Name, caseName, StringComparison.Ordinal)) == true;
    }

    private static bool IsTargetTypedConstructorBinding(ExpressionSyntax expression)
        => expression is MemberBindingExpressionSyntax memberBinding &&
           (memberBinding.Name.IsMissing ||
            memberBinding.Name.Identifier.IsMissing ||
            string.IsNullOrEmpty(memberBinding.Name.Identifier.ValueText));
}
