using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

namespace Raven.LanguageServer;

internal static class SymbolResolver
{
    public static SymbolResolutionResult? ResolveSymbolAtPosition(SemanticModel semanticModel, SyntaxNode root, int offset)
    {
        foreach (var candidateNode in GetCandidateNodes(root, offset))
        {
            var symbol = ResolveSymbolFromNode(semanticModel, candidateNode);
            if (symbol is not null)
                return new SymbolResolutionResult(symbol.UnderlyingSymbol, candidateNode);
        }

        return null;
    }

    private static IEnumerable<SyntaxNode> GetCandidateNodes(SyntaxNode root, int offset)
    {
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

            var current = token.Parent;
            while (current is not null)
            {
                yield return current;
                current = current.Parent;
            }
        }
    }

    private static IEnumerable<int> NormalizeOffsets(int offset, int maxOffset)
    {
        if (maxOffset < 0)
            yield break;

        var clamped = Math.Clamp(offset, 0, maxOffset);
        yield return clamped;

        if (clamped > 0)
            yield return clamped - 1;
    }

    private static ISymbol? ResolveSymbolFromNode(SemanticModel semanticModel, SyntaxNode node)
    {
        var symbolInfo = semanticModel.GetSymbolInfo(node);
        if (symbolInfo.Symbol is not null)
            return symbolInfo.Symbol;

        if (!symbolInfo.CandidateSymbols.IsDefaultOrEmpty)
            return symbolInfo.CandidateSymbols[0];

        return semanticModel.GetDeclaredSymbol(node);
    }
}

internal readonly record struct SymbolResolutionResult(ISymbol Symbol, SyntaxNode Node);
