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
        if (!IsInvocationTargetMatch(invocation.Expression, node, token))
            return false;

        if (TryResolvePipeBoundInvocationTargetSymbol(semanticModel, invocation, out var pipeBoundSymbol))
        {
            symbol = pipeBoundSymbol;
            return true;
        }

        if (TryResolveUnionCaseFromInvocationContext(semanticModel, invocation, out var unionCaseSymbol))
        {
            symbol = unionCaseSymbol;
            return true;
        }

        if (TryGetSymbolInfo(semanticModel, invocation, out var symbolInfo))
        {
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
                var projectedCandidate = ProjectInvocationSymbolForDisplay(symbolInfo.CandidateSymbols[0], semanticModel, invocation);
                if (projectedCandidate is not ILocalSymbol &&
                    !IsUnitTypeSymbol(projectedCandidate))
                {
                    symbol = projectedCandidate;
                    return true;
                }
            }
        }

        // When this invocation is the right-hand side of a pipe expression the binder
        // resolves overloads using the piped value as an implicit first argument and
        // stores the constructed symbol on the BinaryExpression node, not on the
        // InvocationExpression itself.
        return false;
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

        if (TryResolveInvocationExpressionSymbol(semanticModel, invocation, out var invocationSymbol))
        {
            symbol = invocationSymbol;
            return true;
        }

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
                var projectedCandidate = ProjectInvocationSymbolForDisplay(pipeInfo.CandidateSymbols[0], semanticModel, invocation);
                if (projectedCandidate is not ILocalSymbol &&
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
                            var projectedCandidate = ProjectInvocationSymbolForDisplay(identifierInfo.CandidateSymbols[0], semanticModel, invocation);
                            if (projectedCandidate is not ILocalSymbol &&
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
                            var projectedCandidate = ProjectInvocationSymbolForDisplay(memberInfo.CandidateSymbols[0], semanticModel, invocation);
                            if (projectedCandidate is not ILocalSymbol &&
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
                var projectedCandidate = ProjectInvocationSymbolForDisplay(expressionInfo.CandidateSymbols[0], semanticModel, invocation);
                if (projectedCandidate is not ILocalSymbol &&
                    !IsUnitTypeSymbol(projectedCandidate))
                {
                    symbol = projectedCandidate;
                    return true;
                }
            }
        }

        return false;
    }

    private static ISymbol ProjectInvocationSymbolForDisplay(
        ISymbol symbol,
        SemanticModel semanticModel,
        InvocationExpressionSyntax invocation)
    {
        if (symbol is IMethodSymbol { MethodKind: MethodKind.Constructor } constructor)
        {
            return constructor.ContainingType ?? symbol;
        }

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
}
