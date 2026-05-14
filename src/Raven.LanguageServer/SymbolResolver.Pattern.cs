using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.LanguageServer;

internal static partial class SymbolResolver
{
    private static bool TryResolvePatternDeclaredSymbol(
        SemanticModel semanticModel,
        SyntaxNode node,
        SyntaxToken token,
        out ISymbol? symbol)
    {
        symbol = node switch
        {
            SingleVariableDesignationSyntax single when token == single.Identifier => TryGetPreferredDeclarationSymbol(semanticModel, single),
            VariablePatternSyntax { Designation: SingleVariableDesignationSyntax single } when token == single.Identifier => TryGetPreferredDeclarationSymbol(semanticModel, single),
            TypedVariableDesignationSyntax { Designation: SingleVariableDesignationSyntax single } when token == single.Identifier => TryGetPreferredDeclarationSymbol(semanticModel, single),
            _ => null
        };

        if (symbol is not null)
            return true;

        symbol = node.Parent switch
        {
            SingleVariableDesignationSyntax single when token == single.Identifier => TryGetPreferredDeclarationSymbol(semanticModel, single),
            VariablePatternSyntax { Designation: SingleVariableDesignationSyntax single } when token == single.Identifier => TryGetPreferredDeclarationSymbol(semanticModel, single),
            TypedVariableDesignationSyntax { Designation: SingleVariableDesignationSyntax single } when token == single.Identifier => TryGetPreferredDeclarationSymbol(semanticModel, single),
            _ => null
        };

        if (symbol is null &&
            node is IdentifierNameSyntax identifierName &&
            token == identifierName.Identifier &&
            identifierName.Parent is ConstantPatternSyntax &&
            TryResolveFunctionExpressionPatternLocalSymbol(semanticModel, identifierName, out var implicitPatternLocal))
        {
            symbol = implicitPatternLocal;
        }

        return symbol is not null;
    }

    private static ISymbol? TryGetPreferredDeclarationSymbol(SemanticModel semanticModel, SyntaxNode declarationNode)
    {
        if (TryGetSymbolInfo(semanticModel, declarationNode, out var info))
            return ChoosePreferredSymbol(info.Symbol, info.CandidateSymbols, declarationNode);

        try
        {
            return semanticModel.GetDeclaredSymbol(declarationNode);
        }
        catch
        {
            return null;
        }
    }

    private static bool TryResolveFromEnclosingBlockLocals(
        SemanticModel semanticModel,
        SyntaxNode node,
        SyntaxToken token,
        out ISymbol? symbol)
    {
        symbol = null;

        return false;
    }

    private static bool TryResolveFunctionExpressionPatternLocalSymbol(
        SemanticModel semanticModel,
        IdentifierNameSyntax identifierName,
        out ISymbol? symbol)
    {
        symbol = null;

        var parameter = identifierName.Ancestors().OfType<ParameterSyntax>().FirstOrDefault();
        if (parameter is null || parameter.Pattern is null)
            return false;

        var functionExpression = parameter.Ancestors().OfType<FunctionExpressionSyntax>().FirstOrDefault();
        if (functionExpression is null)
            return false;

        return false;
    }

    private static bool TryResolveByDeclaringSyntaxReference(
        SemanticModel semanticModel,
        SyntaxToken token,
        out ISymbol? symbol)
    {
        symbol = null;

        if (token.Kind != SyntaxKind.IdentifierToken || string.IsNullOrWhiteSpace(token.ValueText))
            return false;

        var root = token.SyntaxTree!.GetRoot();
        foreach (var candidate in root.DescendantNodes().OfType<IdentifierNameSyntax>())
        {
            if (!string.Equals(candidate.Identifier.ValueText, token.ValueText, StringComparison.Ordinal))
                continue;

            if (!TryGetSymbolInfo(semanticModel, candidate, out var info) ||
                info.Symbol is null && info.CandidateSymbols.IsDefaultOrEmpty)
            {
                continue;
            }

            if (TryMatchDeclaringSpan(info.Symbol, token))
            {
                symbol = info.Symbol;
                return true;
            }

            if (info.CandidateSymbols.IsDefaultOrEmpty)
                continue;

            foreach (var candidateSymbol in info.CandidateSymbols)
            {
                if (!TryMatchDeclaringSpan(candidateSymbol, token))
                    continue;

                symbol = candidateSymbol;
                return true;
            }
        }

        return false;
    }
}
