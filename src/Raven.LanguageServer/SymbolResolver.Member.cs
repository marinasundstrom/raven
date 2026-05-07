using System.Diagnostics.CodeAnalysis;

using Raven.CodeAnalysis;
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

        if (!TryGetSymbolInfo(semanticModel, identifier, out var receiverInfo))
            return false;
        var receiverCandidate = ChoosePreferredSymbol(receiverInfo.Symbol, receiverInfo.CandidateSymbols, identifier);
        if (receiverCandidate is IParameterSymbol { Type.TypeKind: not TypeKind.Error } parameter)
        {
            symbol = ProjectSymbolForDisplay(parameter);
            return symbol is not null;
        }

        if (receiverCandidate is ILocalSymbol { Type.TypeKind: not TypeKind.Error } local)
        {
            symbol = ProjectSymbolForDisplay(local);
            return symbol is not null;
        }

        return false;
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
        if (!semanticModel.TryGetAvailableTypeInfo(typeSyntax, out var typeInfo))
        {
            resolvedType = null;
            return false;
        }

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

        if (memberAccess.Name.Span.Contains(tokenSpan) &&
            semanticModel.TryGetAvailableSymbolInfo(memberAccess, out var cachedMemberAccessInfo))
        {
            var chosenCachedMemberAccessSymbol = ProjectSymbolForDisplay(
                ChoosePreferredSymbol(cachedMemberAccessInfo.Symbol, cachedMemberAccessInfo.CandidateSymbols, memberAccess));
            if (chosenCachedMemberAccessSymbol is not null)
                return chosenCachedMemberAccessSymbol;
        }

        if (memberAccess.Name.Span.Contains(tokenSpan) &&
            semanticModel.TryGetAvailableSymbolInfo(memberAccess.Name, out var cachedNameInfo))
        {
            var chosenCachedNameSymbol = ProjectSymbolForDisplay(
                ChoosePreferredSymbol(cachedNameInfo.Symbol, cachedNameInfo.CandidateSymbols, memberAccess.Name));
            if (chosenCachedNameSymbol is not null)
                return chosenCachedNameSymbol;
        }

        if (TryGetSymbolInfo(semanticModel, memberAccess, out var memberAccessInfo))
        {
            var chosenMemberAccessSymbol = ProjectSymbolForDisplay(
                ChoosePreferredSymbol(memberAccessInfo.Symbol, memberAccessInfo.CandidateSymbols, memberAccess));
            if (chosenMemberAccessSymbol is not null)
                return chosenMemberAccessSymbol;
        }

        if (TryGetSymbolInfo(semanticModel, memberAccess.Name, out var nameInfo))
        {
            var chosenNameSymbol = ProjectSymbolForDisplay(
                ChoosePreferredSymbol(nameInfo.Symbol, nameInfo.CandidateSymbols, memberAccess.Name));
            if (chosenNameSymbol is not null)
                return chosenNameSymbol;
        }

        return null;
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
