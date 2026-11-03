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
    private static bool TryResolveInvocationArgumentNameAtOffset(
        SemanticModel semanticModel,
        SyntaxNode root,
        int offset,
        [NotNullWhen(true)] out SymbolResolutionResult? resolution)
    {
        resolution = null;

        foreach (var normalizedOffset in NormalizeOffsets(offset, root.FullSpan.End))
        {
            SyntaxToken token;
            try
            {
                token = root.FindToken(normalizedOffset);
            }
            catch
            {
                continue;
            }

            if (token.Parent is not IdentifierNameSyntax identifier ||
                identifier.Parent is not NameColonSyntax nameColon ||
                nameColon.Parent is not ArgumentSyntax argument ||
                argument.Parent is not ArgumentListSyntax argumentList ||
                argumentList.Parent is not InvocationExpressionSyntax invocation ||
                !HaveEquivalentSpan(nameColon.Name, identifier))
            {
                continue;
            }

            if (!identifier.Identifier.Span.Contains(normalizedOffset) &&
                identifier.Identifier.Span.End != normalizedOffset)
            {
                continue;
            }

            if (!TryResolveInvocationParameter(
                    semanticModel,
                    invocation,
                    argumentList.Arguments,
                    argument,
                    out var parameter))
            {
                continue;
            }

            resolution = new SymbolResolutionResult(
                SymbolResolutionKind.ParameterDeclaration,
                parameter,
                identifier);
            return true;
        }

        return false;
    }

    private static bool TryResolveInvocationParameter(
        SemanticModel semanticModel,
        InvocationExpressionSyntax invocation,
        SeparatedSyntaxList<ArgumentSyntax> arguments,
        ArgumentSyntax argument,
        [NotNullWhen(true)] out ParameterSymbol? parameter)
    {
        parameter = null;

        if (semanticModel.TryGetAvailableInvocationCandidates(invocation, out var availableCandidates) &&
            TryChooseInvocationMethodForArgument(availableCandidates, invocation, arguments, argument, out var method) &&
            TryGetInvocationParameter(method, invocation, arguments, argument, out parameter))
        {
            return true;
        }

        if (semanticModel.TryGetInvocationTargetSymbolInfo(invocation, out var targetInfo) &&
            TryChooseInvocationMethodForArgument(targetInfo, invocation, arguments, argument, out method) &&
            TryGetInvocationParameter(method, invocation, arguments, argument, out parameter))
        {
            return true;
        }

        var invocationInfo = semanticModel.GetSymbolInfo(invocation);
        return TryChooseInvocationMethodForArgument(invocationInfo, invocation, arguments, argument, out method) &&
               TryGetInvocationParameter(method, invocation, arguments, argument, out parameter);
    }

    private static bool TryChooseInvocationMethodForArgument(
        SymbolInfo symbolInfo,
        InvocationExpressionSyntax invocation,
        SeparatedSyntaxList<ArgumentSyntax> arguments,
        ArgumentSyntax argument,
        [NotNullWhen(true)] out MethodSymbol? method)
    {
        method = null;

        if (symbolInfo.Symbol is MethodSymbol symbolMethod)
        {
            method = symbolMethod;
            return true;
        }

        if (symbolInfo.Symbol is NamedTypeSymbol symbolType &&
            TryChooseConstructorForInvocation(symbolType, invocation, out var constructor))
        {
            method = constructor;
            return true;
        }

        if (symbolInfo.CandidateSymbols.IsDefaultOrEmpty)
            return false;

        foreach (var candidateType in symbolInfo.CandidateSymbols.OfType<NamedTypeSymbol>())
        {
            if (TryChooseConstructorForInvocation(candidateType, invocation, out constructor))
            {
                method = constructor;
                return true;
            }
        }

        var candidateMethods = symbolInfo.CandidateSymbols.OfType<MethodSymbol>().ToImmutableArray();
        return TryChooseInvocationMethodForArgument(candidateMethods, invocation, arguments, argument, out method);
    }

    private static bool TryChooseInvocationMethodForArgument(
        ImmutableArray<MethodSymbol> methods,
        InvocationExpressionSyntax invocation,
        SeparatedSyntaxList<ArgumentSyntax> arguments,
        ArgumentSyntax argument,
        [NotNullWhen(true)] out MethodSymbol? method)
    {
        method = SemanticModel.TryChooseInvocationMethodCandidate(
            methods,
            invocation,
            SemanticModel.InvocationCandidateFallback.None);
        if (method is not null)
            return true;

        MethodSymbol? matchingMethod = null;
        foreach (var candidate in methods)
        {
            if (!TryGetInvocationParameter(candidate, invocation, arguments, argument, out _))
                continue;

            if (matchingMethod is not null)
            {
                method = null;
                return false;
            }

            matchingMethod = candidate;
        }

        method = matchingMethod;
        return method is not null;
    }

    private static bool TryGetInvocationParameter(
        MethodSymbol method,
        InvocationExpressionSyntax invocation,
        SeparatedSyntaxList<ArgumentSyntax> arguments,
        ArgumentSyntax argument,
        [NotNullWhen(true)] out ParameterSymbol? parameter)
    {
        parameter = null;

        var receiverOffset = method.IsExtensionMethod &&
                             invocation.Expression is MemberAccessExpressionSyntax
            ? 1
            : 0;

        if (argument.NameColon?.Name.Identifier.ValueText is { Length: > 0 } argumentName)
        {
            parameter = method.Parameters
                .Skip(receiverOffset)
                .FirstOrDefault(candidate => string.Equals(
                    candidate.Name,
                    argumentName,
                    StringComparison.OrdinalIgnoreCase));
            return parameter is not null;
        }

        for (var i = 0; i < arguments.Count; i++)
        {
            if (arguments[i].Span != argument.Span)
                continue;

            var parameterIndex = i + receiverOffset;
            if (parameterIndex >= 0 && parameterIndex < method.Parameters.Length)
            {
                parameter = method.Parameters[parameterIndex];
                return true;
            }

            return false;
        }

        return false;
    }

    private static bool TryResolveInvocationTargetSymbol(
        SemanticModel semanticModel,
        SyntaxNode node,
        SyntaxToken token,
        out Symbol? symbol)
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

        if (semanticModel.TryGetInvocationTargetSymbolInfo(invocation, out var invocationTargetInfo) &&
            TryResolveInvocationTargetSymbolFromSymbolInfo(semanticModel, invocation, invocationTargetInfo, out symbol))
        {
            return true;
        }

        if (TryResolvePipeBoundInvocationTargetSymbol(semanticModel, invocation, out var pipeBoundSymbol))
        {
            symbol = pipeBoundSymbol;
            return true;
        }

        if (invocation.Expression is MemberBindingExpressionSyntax invocationMemberBinding &&
            semanticModel.GetSymbolInfo(invocationMemberBinding) is var memberBindingInfo &&
            (memberBindingInfo.Symbol is not null || !memberBindingInfo.CandidateSymbols.IsDefaultOrEmpty))
        {
            if (memberBindingInfo.Symbol is NamedTypeSymbol memberBindingType &&
                TryChooseConstructorForInvocation(memberBindingType, invocation, out var memberBindingConstructor))
            {
                symbol = memberBindingConstructor;
                return true;
            }

            if (memberBindingInfo.Symbol is not null)
            {
                var projectedSymbol = ProjectInvocationSymbolForDisplay(memberBindingInfo.Symbol, semanticModel, invocation);
                if (projectedSymbol is not LocalSymbol &&
                    !IsUnitTypeSymbol(projectedSymbol))
                {
                    symbol = projectedSymbol;
                    return true;
                }
            }

            if (!memberBindingInfo.CandidateSymbols.IsDefaultOrEmpty)
            {
                foreach (var candidateType in memberBindingInfo.CandidateSymbols.OfType<NamedTypeSymbol>())
                {
                    if (TryChooseConstructorForInvocation(candidateType, invocation, out var candidateConstructor))
                    {
                        symbol = candidateConstructor;
                        return true;
                    }
                }

                var methodCandidates = memberBindingInfo.CandidateSymbols.OfType<MethodSymbol>().ToImmutableArray();
                var candidate = methodCandidates.Length == 1
                    ? methodCandidates[0]
                    : SemanticModel.TryChooseInvocationMethodCandidate(
                        methodCandidates,
                        invocation,
                        SemanticModel.InvocationCandidateFallback.None);
                if (candidate is not null &&
                    ProjectInvocationSymbolForDisplay(candidate, semanticModel, invocation) is { } projectedCandidate &&
                    projectedCandidate is not LocalSymbol &&
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
            if (symbolInfo.Symbol is NamedTypeSymbol invocationType &&
                TryChooseConstructorForInvocation(invocationType, invocation, out var invocationConstructor))
            {
                symbol = invocationConstructor;
                return true;
            }

            if (symbolInfo.Symbol is not null)
            {
                var projectedSymbol = ProjectInvocationSymbolForDisplay(symbolInfo.Symbol, semanticModel, invocation);
                if (projectedSymbol is not LocalSymbol &&
                    !IsUnitTypeSymbol(projectedSymbol))
                {
                    symbol = projectedSymbol;
                    return true;
                }
            }

            if (!symbolInfo.CandidateSymbols.IsDefaultOrEmpty)
            {
                foreach (var candidateType in symbolInfo.CandidateSymbols.OfType<NamedTypeSymbol>())
                {
                    if (TryChooseConstructorForInvocation(candidateType, invocation, out var candidateConstructor))
                    {
                        symbol = candidateConstructor;
                        return true;
                    }
                }

                var methodCandidates = symbolInfo.CandidateSymbols.OfType<MethodSymbol>().ToImmutableArray();
                var candidate = methodCandidates.Length == 1
                    ? methodCandidates[0]
                    : SemanticModel.TryChooseInvocationMethodCandidate(
                        methodCandidates,
                        invocation,
                        SemanticModel.InvocationCandidateFallback.None);
                if (candidate is not null &&
                    ProjectInvocationSymbolForDisplay(candidate, semanticModel, invocation) is { } projectedCandidate &&
                    projectedCandidate is not LocalSymbol &&
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

    private static bool TryResolveInvocationTargetSymbolFromSymbolInfo(
        SemanticModel semanticModel,
        InvocationExpressionSyntax invocation,
        SymbolInfo symbolInfo,
        [NotNullWhen(true)] out Symbol? symbol)
    {
        symbol = null;

        if (symbolInfo.Symbol is NamedTypeSymbol invocationType &&
            TryChooseConstructorForInvocation(invocationType, invocation, out var invocationConstructor))
        {
            symbol = invocationConstructor;
            return true;
        }

        if (symbolInfo.Symbol is not null)
        {
            var projectedSymbol = ProjectInvocationSymbolForDisplay(symbolInfo.Symbol, semanticModel, invocation);
            if (projectedSymbol is not LocalSymbol &&
                !IsUnitTypeSymbol(projectedSymbol))
            {
                symbol = projectedSymbol;
                return true;
            }
        }

        foreach (var candidateType in symbolInfo.CandidateSymbols.OfType<NamedTypeSymbol>())
        {
            if (TryChooseConstructorForInvocation(candidateType, invocation, out var candidateConstructor))
            {
                symbol = candidateConstructor;
                return true;
            }
        }

        if (!symbolInfo.CandidateSymbols.IsDefaultOrEmpty)
        {
            var methodCandidates = symbolInfo.CandidateSymbols.OfType<MethodSymbol>().ToImmutableArray();
            var candidate = methodCandidates.Length == 1
                ? methodCandidates[0]
                : SemanticModel.TryChooseInvocationMethodCandidate(
                    methodCandidates,
                    invocation,
                    SemanticModel.InvocationCandidateFallback.None);
            if (candidate is not null &&
                ProjectInvocationSymbolForDisplay(candidate, semanticModel, invocation) is { } projectedCandidate &&
                projectedCandidate is not LocalSymbol &&
                !IsUnitTypeSymbol(projectedCandidate))
            {
                symbol = projectedCandidate;
                return true;
            }
        }

        return false;
    }

    private static bool TryResolveConstructorFromTargetType(
        InvocationExpressionSyntax invocation,
        TypeSymbol targetType,
        [NotNullWhen(true)] out MethodSymbol? constructor)
    {
        constructor = null;

        if (targetType is not NamedTypeSymbol namedType)
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
        NamedTypeSymbol type,
        InvocationExpressionSyntax invocation,
        [NotNullWhen(true)] out MethodSymbol? constructor)
    {
        constructor = type.Constructors.FirstOrDefault(candidate =>
            candidate.Parameters.Length == invocation.ArgumentList.Arguments.Count);
        constructor ??= type.Constructors.FirstOrDefault();
        return constructor is not null;
    }

    private static bool IsUnitTypeSymbol(Symbol symbol)
        => symbol is TypeSymbol type &&
           (type.SpecialType == SpecialType.System_Unit ||
            string.Equals(type.Name, "Unit", StringComparison.Ordinal));

    private static bool TryResolvePipeBoundInvocationTargetSymbol(
        SemanticModel semanticModel,
        InvocationExpressionSyntax invocation,
        [NotNullWhen(true)] out Symbol? symbol)
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
                if (projectedSymbol is not LocalSymbol &&
                    !IsUnitTypeSymbol(projectedSymbol))
                {
                    symbol = projectedSymbol;
                    return true;
                }
            }

            if (!pipeInfo.CandidateSymbols.IsDefaultOrEmpty)
            {
                var methodCandidates = pipeInfo.CandidateSymbols.OfType<MethodSymbol>().ToImmutableArray();
                var candidate = methodCandidates.Length == 1
                    ? methodCandidates[0]
                    : SemanticModel.TryChooseInvocationMethodCandidate(
                        methodCandidates,
                        invocation,
                        SemanticModel.InvocationCandidateFallback.None);
                if (candidate is not null &&
                    ProjectInvocationSymbolForDisplay(candidate, semanticModel, invocation) is { } projectedCandidate &&
                    projectedCandidate is not LocalSymbol &&
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
        [NotNullWhen(true)] out MethodSymbol? method)
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
            semanticModel.GetDeclaredSymbol(containingTypeSyntax) is not NamedTypeSymbol containingType)
        {
            return false;
        }

        method = containingType
            .GetMembers(methodName)
            .OfType<MethodSymbol>()
            .Where(candidate => candidate.Parameters.Length == invocation.ArgumentList.Arguments.Count + 1)
            .OrderByDescending(static candidate => candidate.IsStatic)
            .FirstOrDefault();
        return method is not null;
    }

    private static bool TryResolveInvocationExpressionSymbol(
        SemanticModel semanticModel,
        InvocationExpressionSyntax invocation,
        [NotNullWhen(true)] out Symbol? symbol)
    {
        symbol = null;

        switch (invocation.Expression)
        {
            case IdentifierNameSyntax identifier:
                {
                    if (TryGetSymbolInfo(semanticModel, identifier, out var identifierInfo))
                    {
                        if (identifierInfo.Symbol is NamedTypeSymbol namedType &&
                            TryChooseConstructorForInvocation(namedType, invocation, out var constructor))
                        {
                            symbol = constructor;
                            return true;
                        }

                        foreach (var candidateType in identifierInfo.CandidateSymbols.OfType<NamedTypeSymbol>())
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
                            if (projectedSymbol is not LocalSymbol &&
                                !IsUnitTypeSymbol(projectedSymbol))
                            {
                                symbol = projectedSymbol;
                                return true;
                            }
                        }

                        if (!identifierInfo.CandidateSymbols.IsDefaultOrEmpty)
                        {
                            var methodCandidates = identifierInfo.CandidateSymbols.OfType<MethodSymbol>().ToImmutableArray();
                            var candidate = methodCandidates.Length == 1
                                ? methodCandidates[0]
                                : SemanticModel.TryChooseInvocationMethodCandidate(
                                    methodCandidates,
                                    invocation,
                                    SemanticModel.InvocationCandidateFallback.None);
                            if (candidate is not null &&
                                ProjectInvocationSymbolForDisplay(candidate, semanticModel, invocation) is { } projectedCandidate &&
                                projectedCandidate is not LocalSymbol &&
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
                        if (memberInfo.Symbol is NamedTypeSymbol namedType &&
                            TryChooseConstructorForInvocation(namedType, invocation, out var constructor))
                        {
                            symbol = constructor;
                            return true;
                        }

                        foreach (var candidateType in memberInfo.CandidateSymbols.OfType<NamedTypeSymbol>())
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
                            if (projectedSymbol is not LocalSymbol &&
                                !IsUnitTypeSymbol(projectedSymbol))
                            {
                                symbol = projectedSymbol;
                                return true;
                            }
                        }

                        if (!memberInfo.CandidateSymbols.IsDefaultOrEmpty)
                        {
                            var methodCandidates = memberInfo.CandidateSymbols.OfType<MethodSymbol>().ToImmutableArray();
                            var candidate = methodCandidates.Length == 1
                                ? methodCandidates[0]
                                : SemanticModel.TryChooseInvocationMethodCandidate(
                                    methodCandidates,
                                    invocation,
                                    SemanticModel.InvocationCandidateFallback.None);
                            if (candidate is not null &&
                                ProjectInvocationSymbolForDisplay(candidate, semanticModel, invocation) is { } projectedCandidate &&
                                projectedCandidate is not LocalSymbol &&
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
                if (projectedSymbol is not LocalSymbol &&
                    !IsUnitTypeSymbol(projectedSymbol))
                {
                    symbol = projectedSymbol;
                    return true;
                }
            }

            if (!expressionInfo.CandidateSymbols.IsDefaultOrEmpty)
            {
                var methodCandidates = expressionInfo.CandidateSymbols.OfType<MethodSymbol>().ToImmutableArray();
                var candidate = methodCandidates.Length == 1
                    ? methodCandidates[0]
                    : SemanticModel.TryChooseInvocationMethodCandidate(
                        methodCandidates,
                        invocation,
                        SemanticModel.InvocationCandidateFallback.None);
                if (candidate is not null &&
                    ProjectInvocationSymbolForDisplay(candidate, semanticModel, invocation) is { } projectedCandidate &&
                    projectedCandidate is not LocalSymbol &&
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
        [NotNullWhen(true)] out MethodSymbol? constructor)
    {
        constructor = null;

        if (!TryGetSymbolInfo(semanticModel, invocation.Expression, out var expressionInfo))
            return false;

        if (expressionInfo.Symbol is NamedTypeSymbol namedType &&
            TryChooseConstructorForInvocation(namedType, invocation, out constructor))
        {
            return true;
        }

        foreach (var candidateType in expressionInfo.CandidateSymbols.OfType<NamedTypeSymbol>())
        {
            if (TryChooseConstructorForInvocation(candidateType, invocation, out constructor))
                return true;
        }

        return false;
    }

    private static Symbol ProjectInvocationSymbolForDisplay(
        Symbol symbol,
        SemanticModel semanticModel,
        InvocationExpressionSyntax invocation)
    {
        return symbol;
    }

    private static bool TryResolveUnionCaseFromInvocationContext(
        SemanticModel semanticModel,
        InvocationExpressionSyntax invocation,
        out Symbol? symbol)
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

        if (targetType is UnionSymbol union && targetType.IsUnion)
        {
            var unionCase = union.Variants.FirstOrDefault(c => string.Equals(c.Name, invokedName, StringComparison.Ordinal));
            if (unionCase is not null)
            {
                symbol = unionCase;
                return true;
            }
        }

        if (targetType is UnionCaseTypeSymbol caseType &&
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
        [NotNullWhen(true)] out TypeSymbol? targetType)
    {
        targetType = null;

        if (invocation.AncestorsAndSelf().OfType<FunctionStatementSyntax>().FirstOrDefault() is { } functionStatement &&
            semanticModel.GetDeclaredSymbol(functionStatement) is MethodSymbol { ReturnType: { } functionReturnType } &&
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
        [NotNullWhen(true)] out TypeSymbol? targetType)
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
        [NotNullWhen(true)] out TypeSymbol? targetType)
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

    private static ParameterSymbol? TryGetInvocationParameter(
        SemanticModel semanticModel,
        InvocationExpressionSyntax invocation,
        SeparatedSyntaxList<ArgumentSyntax> arguments,
        ArgumentSyntax argument)
    {
        var symbol = semanticModel.GetSymbolInfo(invocation).Symbol;
        var method = symbol as MethodSymbol;
        if (method is null &&
            invocation.Expression is TypeSyntax typeSyntax &&
            semanticModel.GetTypeInfo(typeSyntax).Type is NamedTypeSymbol namedType)
        {
            method = namedType.Constructors.FirstOrDefault();
        }
        if (method is null &&
            semanticModel.GetSymbolInfo(invocation.Expression).Symbol is NamedTypeSymbol expressionType)
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

    private static bool ContainsUnionCaseNamed(TypeSymbol type, string caseName)
    {
        var union = type.TryGetUnion() ?? type.TryGetUnionCase()?.Union;
        return union?.Variants.Any(caseType => string.Equals(caseType.Name, caseName, StringComparison.Ordinal)) == true;
    }

    private static bool IsTargetTypedConstructorBinding(ExpressionSyntax expression)
        => expression is MemberBindingExpressionSyntax memberBinding &&
           (memberBinding.Name.IsMissing ||
            memberBinding.Name.Identifier.IsMissing ||
            string.IsNullOrEmpty(memberBinding.Name.Identifier.ValueText));
}
