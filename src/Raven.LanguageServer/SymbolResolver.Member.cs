using System.Diagnostics.CodeAnalysis;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Operations;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.LanguageServer;

internal static partial class SymbolResolver
{
    private static bool TryResolveParameterDeclarationSymbol(
        SemanticModel semanticModel,
        SyntaxNode node,
        SyntaxToken token,
        out ISymbol? symbol)
    {
        symbol = null;

        var parameter = node as ParameterSyntax ?? node.AncestorsAndSelf().OfType<ParameterSyntax>().FirstOrDefault();
        if (parameter is null)
            return false;

        if (!parameter.Span.Contains(token.Span))
            return false;

        if (parameter.Pattern?.Span.Contains(token.Span) == true)
            return false;

        // Let nested syntax keep their own hover identity:
        // parameter type annotation should hover the type, and default values
        // should hover symbols inside the expression.
        if (parameter.TypeAnnotation?.Type.Span.Contains(token.Span) == true)
            return false;

        if (parameter.DefaultValue?.Value.Span.Contains(token.Span) == true)
            return false;

        symbol = semanticModel.GetFunctionExpressionParameterSymbol(parameter)
            ?? semanticModel.GetDeclaredSymbol(parameter);
        return symbol is not null;
    }

    private static bool TryResolveMemberReceiverSymbol(
        SemanticModel semanticModel,
        SyntaxNode node,
        SyntaxToken token,
        out ISymbol? symbol)
    {
        symbol = null;

        if (node is not IdentifierNameSyntax identifier ||
            identifier.Parent is not MemberAccessExpressionSyntax memberAccess ||
            !HaveEquivalentSpan(memberAccess.Expression, identifier) ||
            !identifier.Span.Contains(token.Span))
        {
            return false;
        }

        // Prime member-access binding first so receiver symbols are resolved in the
        // final overload-selected lambda scope.
        _ = TryGetSymbolInfo(semanticModel, memberAccess, out _);

        if (!TryGetSymbolInfo(semanticModel, identifier, out var receiverInfo))
            return false;
        var receiverCandidate = ChoosePreferredSymbol(receiverInfo.Symbol, receiverInfo.CandidateSymbols, identifier);
        if (receiverCandidate is IParameterSymbol parameter &&
            parameter.Type?.TypeKind != TypeKind.Error)
        {
            symbol = ProjectSymbolForDisplay(parameter);
            return symbol is not null;
        }

        var operation = semanticModel.GetOperation(memberAccess);
        var referenced = FindReferencedSymbolAtToken(operation, identifier.Identifier.Span);
        if (referenced is null)
            return false;

        symbol = ProjectSymbolForDisplay(referenced);
        return symbol is not null;
    }

    private static bool TryResolveMemberSegmentSymbol(
        SemanticModel semanticModel,
        SyntaxNode node,
        SyntaxToken token,
        out ISymbol? symbol)
    {
        symbol = null;

        SyntaxNode? targetNode = node switch
        {
            MemberBindingExpressionSyntax memberBinding
                when memberBinding.Name.Span.Contains(token.Span) || memberBinding.OperatorToken == token => memberBinding,
            IdentifierNameSyntax identifier
                when identifier.Parent is MemberBindingExpressionSyntax memberBinding &&
                     HaveEquivalentSpan(memberBinding.Name, identifier) => memberBinding,
            MemberAccessExpressionSyntax memberAccess
                when memberAccess.Name.Span.Contains(token.Span) || memberAccess.OperatorToken == token => memberAccess,
            IdentifierNameSyntax identifier
                when identifier.Parent is MemberAccessExpressionSyntax memberAccess &&
                     HaveEquivalentSpan(memberAccess.Name, identifier) => memberAccess,
            _ => null
        };

        if (targetNode is null)
            return false;

        if (targetNode is MemberAccessExpressionSyntax memberAccessTarget)
        {
            symbol = ResolveExplicitMemberAccessSymbol(semanticModel, memberAccessTarget, token.Span);
            if (symbol is not null)
                return true;
        }

        if (targetNode is MemberBindingExpressionSyntax memberBindingTarget)
        {
            if (TryGetSymbolInfo(semanticModel, memberBindingTarget.Name, out var nameInfo) &&
                (nameInfo.Symbol is not null || !nameInfo.CandidateSymbols.IsDefaultOrEmpty))
            {
                symbol = ChoosePreferredSymbol(nameInfo.Symbol, nameInfo.CandidateSymbols, memberBindingTarget.Name);
                symbol = ProjectSymbolForDisplay(symbol);
                if (symbol is not null)
                    return true;
            }
        }

        if (TryGetSymbolInfo(semanticModel, targetNode, out var directInfo) &&
            (directInfo.Symbol is not null || !directInfo.CandidateSymbols.IsDefaultOrEmpty))
        {
            symbol = ChoosePreferredSymbol(directInfo.Symbol, directInfo.CandidateSymbols, targetNode);
            symbol = ProjectSymbolForDisplay(symbol);
            return symbol is not null;
        }

        foreach (var conditionalAccess in targetNode.AncestorsAndSelf().OfType<ConditionalAccessExpressionSyntax>())
        {
            if (!conditionalAccess.WhenNotNull.Span.Contains(token.Span))
                continue;

            var operation = TryGetOperation(semanticModel, conditionalAccess);
            var referenced = FindReferencedSymbolAtToken(operation, token.Span);
            if (referenced is not null)
            {
                symbol = ProjectSymbolForDisplay(referenced);
                return true;
            }
        }

        var fallbackOperation = TryGetOperation(semanticModel, targetNode);
        var fallbackSymbol = FindReferencedSymbolAtToken(fallbackOperation, token.Span);
        if (fallbackSymbol is not null)
        {
            symbol = ProjectSymbolForDisplay(fallbackSymbol);
            return true;
        }

        var containingStatement = targetNode.AncestorsAndSelf().OfType<StatementSyntax>().FirstOrDefault();
        if (containingStatement is not null)
        {
            var statementOperation = TryGetOperation(semanticModel, containingStatement);
            var statementSymbol = FindReferencedSymbolAtToken(statementOperation, token.Span);
            if (statementSymbol is not null)
            {
                symbol = ProjectSymbolForDisplay(statementSymbol);
                return true;
            }
        }

        return false;
    }

    private static bool TryResolveParameterDeclarationTokenFastPath(
        SemanticModel semanticModel,
        SyntaxToken token,
        [NotNullWhen(true)] out ISymbol? symbol,
        [NotNullWhen(true)] out SyntaxNode? node)
    {
        symbol = null;
        node = null;

        var parameter = token.Parent?.AncestorsAndSelf().OfType<ParameterSyntax>().FirstOrDefault();
        if (parameter is null)
            return false;

        if (!parameter.Span.Contains(token.Span))
            return false;

        if (parameter.Pattern?.Span.Contains(token.Span) == true)
            return false;

        if (parameter.TypeAnnotation?.Type.Span.Contains(token.Span) == true)
            return false;

        if (parameter.DefaultValue?.Value.Span.Contains(token.Span) == true)
            return false;

        var resolved = semanticModel.GetFunctionExpressionParameterSymbol(parameter)
            ?? semanticModel.GetDeclaredSymbol(parameter);
        if (resolved is null)
            return false;

        symbol = resolved;
        node = parameter;
        return true;
    }

    private static bool TryResolveTypePositionSymbol(
        SemanticModel semanticModel,
        SyntaxNode node,
        SyntaxToken token,
        out ISymbol? symbol)
    {
        symbol = null;

        var typeSyntaxes = node.AncestorsAndSelf()
            .OfType<TypeSyntax>()
            .Where(typeSyntax => typeSyntax.Span.Contains(token.Span));

        foreach (var typeSyntax in typeSyntaxes)
        {
            if (!TryResolveTypeSymbolFromSyntax(semanticModel, typeSyntax, out var resolvedType))
                continue;

            symbol = resolvedType!.UnderlyingUnionType ?? resolvedType;
            return true;
        }

        return false;
    }

    private static bool TryResolveTypeSymbolFromSyntax(
        SemanticModel semanticModel,
        TypeSyntax typeSyntax,
        out ITypeSymbol? resolvedType)
    {
        var typeInfo = semanticModel.GetTypeInfo(typeSyntax);
        resolvedType = typeInfo.Type ?? typeInfo.ConvertedType;
        if (resolvedType is not null && resolvedType.TypeKind != TypeKind.Error)
            return true;

        var typeSymbol = TryGetSymbolInfo(semanticModel, typeSyntax, out var typeSymbolInfo)
            ? typeSymbolInfo.Symbol
            : null;
        resolvedType = typeSymbol switch
        {
            ITypeSymbol resolved => resolved,
            IAliasSymbol { UnderlyingSymbol: ITypeSymbol aliasedType } => aliasedType,
            _ => resolvedType
        };

        if (resolvedType is null || resolvedType.TypeKind == TypeKind.Error)
            return false;

        return true;
    }

    private static ISymbol? ResolveExplicitMemberAccessSymbol(
        SemanticModel semanticModel,
        MemberAccessExpressionSyntax memberAccess,
        TextSpan tokenSpan)
    {
        if (memberAccess.Parent is InvocationExpressionSyntax invocation &&
            memberAccess.Name.Span.Contains(tokenSpan.Start) &&
            TryResolveUnionCaseFromInvocationContext(semanticModel, invocation, out var unionCaseSymbol) &&
            unionCaseSymbol is not null)
        {
            return unionCaseSymbol;
        }

        if (TryGetSymbolInfo(semanticModel, memberAccess, out var memberAccessInfo))
        {
            var chosenMemberAccessSymbol = ProjectSymbolForDisplay(
                ChoosePreferredSymbol(memberAccessInfo.Symbol, memberAccessInfo.CandidateSymbols, memberAccess));
            if (chosenMemberAccessSymbol is not null)
                return chosenMemberAccessSymbol;
        }

        if (TryResolveMemberFromReceiverType(semanticModel, memberAccess, out var resolvedFromReceiver))
        {
            var projectedReceiverSymbol = ProjectSymbolForDisplay(resolvedFromReceiver);
            if (projectedReceiverSymbol is not null)
                return projectedReceiverSymbol;
        }

        if (TryGetSymbolInfo(semanticModel, memberAccess.Name, out var nameInfo))
        {
            var chosenNameSymbol = ProjectSymbolForDisplay(
                ChoosePreferredSymbol(nameInfo.Symbol, nameInfo.CandidateSymbols, memberAccess.Name));
            if (chosenNameSymbol is not null)
                return chosenNameSymbol;
        }

        var operationSymbol = ProjectSymbolForDisplay(
            FindReferencedSymbolAtToken(semanticModel.GetOperation(memberAccess), tokenSpan));
        if (operationSymbol is not null)
            return operationSymbol;

        return null;
    }

    private static bool TryResolveMemberFromReceiverType(
        SemanticModel semanticModel,
        MemberAccessExpressionSyntax memberAccess,
        out ISymbol? symbol)
    {
        symbol = null;

        if (memberAccess.Name is not IdentifierNameSyntax identifierName)
            return false;

        var memberName = identifierName.Identifier.ValueText;
        if (string.IsNullOrWhiteSpace(memberName))
            return false;

        var receiverType = semanticModel.GetTypeInfo(memberAccess.Expression).Type;
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
        {
            var receiverSymbol = TryGetSymbolInfo(semanticModel, memberAccess.Expression, out var receiverInfo)
                ? receiverInfo.Symbol
                : null;
            receiverType = receiverSymbol switch
            {
                ILocalSymbol local => local.Type,
                IParameterSymbol parameter => parameter.Type,
                IPropertySymbol property => property.Type,
                IFieldSymbol field => field.Type,
                _ => null
            };
        }

        if ((receiverType is null || receiverType.TypeKind == TypeKind.Error) &&
            memberAccess.Expression is IdentifierNameSyntax receiverIdentifier &&
            TryGetEnclosingFunctionExpression(memberAccess, out var enclosingFunctionExpression))
        {
            if (TryInferFunctionParameterTypeFromInvocationContext(
                    semanticModel,
                    enclosingFunctionExpression,
                    receiverIdentifier.Identifier.ValueText,
                    memberName,
                    out var inferredFromInvocation))
            {
                receiverType = inferredFromInvocation;
            }

            var functionType = semanticModel.GetTypeInfo(enclosingFunctionExpression).ConvertedType ??
                               semanticModel.GetTypeInfo(enclosingFunctionExpression).Type;
            if (functionType is INamedTypeSymbol { TypeKind: TypeKind.Delegate } delegateType &&
                delegateType.GetDelegateInvokeMethod() is { } invokeMethod)
            {
                IParameterSymbol? matchedParameter = null;

                if (invokeMethod.Parameters.Length == 1)
                {
                    matchedParameter = invokeMethod.Parameters[0];
                }
                else
                {
                    matchedParameter = invokeMethod.Parameters.FirstOrDefault(
                        p => string.Equals(p.Name, receiverIdentifier.Identifier.ValueText, StringComparison.Ordinal));
                }

                if (matchedParameter is not null)
                    receiverType = matchedParameter.Type;
            }

            if (TryGetSymbolInfo(semanticModel, enclosingFunctionExpression, out var functionInfo) &&
                functionInfo.Symbol is IMethodSymbol lambdaMethod)
            {
                var parameter = lambdaMethod.Parameters.FirstOrDefault(
                    p => string.Equals(p.Name, receiverIdentifier.Identifier.ValueText, StringComparison.Ordinal));

                if (parameter is not null)
                    receiverType = parameter.Type;
            }
        }

        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
            return false;

        var namedReceiverType = receiverType as INamedTypeSymbol;
        if (namedReceiverType is null)
            return false;

        foreach (var candidate in namedReceiverType.GetMembers(memberName))
        {
            if (candidate is IPropertySymbol or IFieldSymbol or IEventSymbol or IMethodSymbol)
            {
                symbol = candidate;
                return true;
            }
        }

        return false;
    }

    private static bool TryInferFunctionParameterTypeFromInvocationContext(
        SemanticModel semanticModel,
        FunctionExpressionSyntax functionExpression,
        string receiverIdentifierName,
        string requiredMemberName,
        out ITypeSymbol? inferredType)
    {
        inferredType = null;

        if (functionExpression.Parent is not ArgumentSyntax argument ||
            argument.Parent is not ArgumentListSyntax argumentList ||
            argumentList.Parent is not InvocationExpressionSyntax invocation)
        {
            return false;
        }

        var argumentIndex = 0;
        foreach (var candidate in argumentList.Arguments)
        {
            if (HaveEquivalentSpan(candidate, argument))
                break;

            argumentIndex++;
        }

        IEnumerable<IMethodSymbol> candidateMethods;
        if (invocation.Expression is MemberAccessExpressionSyntax invocationMemberAccess &&
            invocationMemberAccess.Name is IdentifierNameSyntax memberIdentifier)
        {
            var invocationReceiverType = semanticModel.GetTypeInfo(invocationMemberAccess.Expression).Type;
            if (invocationReceiverType is not INamedTypeSymbol invocationReceiverNamed)
                return false;

            candidateMethods = invocationReceiverNamed
                .GetMembers(memberIdentifier.Identifier.ValueText)
                .OfType<IMethodSymbol>();
        }
        else
        {
            return false;
        }

        ITypeSymbol? fallback = null;
        foreach (var method in candidateMethods)
        {
            if (method.Parameters.Length <= argumentIndex)
                continue;

            var delegateType = method.Parameters[argumentIndex].Type as INamedTypeSymbol;
            if (delegateType?.TypeKind != TypeKind.Delegate)
                continue;

            var invokeMethod = delegateType.GetDelegateInvokeMethod();
            if (invokeMethod is null || invokeMethod.Parameters.IsDefaultOrEmpty)
                continue;

            IParameterSymbol? lambdaParameter = invokeMethod.Parameters.Length == 1
                ? invokeMethod.Parameters[0]
                : invokeMethod.Parameters.FirstOrDefault(
                    p => string.Equals(p.Name, receiverIdentifierName, StringComparison.Ordinal));

            if (lambdaParameter is null)
                continue;

            var parameterType = lambdaParameter.Type is NullableTypeSymbol nullable
                ? nullable.UnderlyingType
                : lambdaParameter.Type;

            fallback ??= parameterType;

            if (parameterType is INamedTypeSymbol named &&
                named.GetMembers(requiredMemberName).Length > 0)
            {
                inferredType = parameterType;
                return true;
            }
        }

        inferredType = fallback;
        return inferredType is not null;
    }

    private static bool TryGetEnclosingFunctionExpression(SyntaxNode node, out FunctionExpressionSyntax functionExpression)
    {
        for (var current = node.Parent; current is not null; current = current.Parent)
        {
            if (current is FunctionExpressionSyntax candidate)
            {
                functionExpression = candidate;
                return true;
            }
        }

        functionExpression = null!;
        return false;
    }

    private static bool TryResolveEnclosingLambdaParameterReference(
        SemanticModel semanticModel,
        SyntaxNode node,
        SyntaxToken token,
        [NotNullWhen(true)] out ISymbol? symbol)
    {
        symbol = null;

        if (node is not IdentifierNameSyntax identifierName ||
            !identifierName.Identifier.Span.Contains(token.Span))
        {
            return false;
        }

        var functionExpression = identifierName.Ancestors().OfType<FunctionExpressionSyntax>().FirstOrDefault();
        if (functionExpression is null || !IsInsideFunctionExpressionBody(functionExpression, token))
            return false;

        if (functionExpression is ParenthesizedFunctionExpressionSyntax parenthesized)
        {
            foreach (var parameter in parenthesized.ParameterList.Parameters)
            {
                if (!string.Equals(parameter.Identifier.ValueText, identifierName.Identifier.ValueText, StringComparison.Ordinal))
                    continue;

                symbol = semanticModel.GetFunctionExpressionParameterSymbol(parameter);
                return symbol is not null;
            }
        }
        else if (functionExpression is SimpleFunctionExpressionSyntax simple &&
                 string.Equals(simple.Parameter.Identifier.ValueText, identifierName.Identifier.ValueText, StringComparison.Ordinal))
        {
            symbol = semanticModel.GetFunctionExpressionParameterSymbol(simple.Parameter);
            return symbol is not null;
        }

        return false;
    }
}
