using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

namespace Raven.LanguageServer;

internal static class SymbolResolutionHelpers
{
    public static bool TryGetPreferredSymbolInfo(SemanticModel semanticModel, SyntaxNode node, out SymbolInfo symbolInfo)
    {
        try
        {
            if (semanticModel.TryGetSymbolInfo(node, out symbolInfo))
                return true;
        }
        catch
        {
        }

        symbolInfo = default;
        return false;
    }

    public static Symbol GetNavigationTargetSymbol(SymbolResolutionResult resolution)
        => resolution.Kind switch
        {
            SymbolResolutionKind.InvocationTarget => NormalizeInvocationOrTypeSymbol(resolution.Symbol),
            SymbolResolutionKind.TypePosition => NormalizeInvocationOrTypeSymbol(resolution.Symbol),
            SymbolResolutionKind.MemberAccess => NormalizeMemberLikeSymbol(resolution.Symbol),
            SymbolResolutionKind.MemberSegment => NormalizeMemberLikeSymbol(resolution.Symbol),
            _ => resolution.Symbol
        };

    public static Symbol GetReferenceTargetSymbol(SymbolResolutionResult resolution)
    {
        return resolution.Kind switch
        {
            SymbolResolutionKind.InvocationTarget => NormalizeInvocationOrTypeSymbol(resolution.Symbol),
            SymbolResolutionKind.TypePosition => NormalizeInvocationOrTypeSymbol(resolution.Symbol),
            SymbolResolutionKind.MemberAccess => NormalizeMemberLikeSymbol(resolution.Symbol),
            SymbolResolutionKind.MemberSegment => NormalizeMemberLikeSymbol(resolution.Symbol),
            SymbolResolutionKind.MemberReceiver => NormalizeMemberLikeSymbol(resolution.Symbol),
            _ => ReferenceSearchService.NormalizeSymbol(resolution.Symbol)
        };
    }

    private static Symbol NormalizeInvocationOrTypeSymbol(Symbol symbol)
        => ReferenceSearchService.NormalizeSymbol(symbol);

    private static Symbol NormalizeMemberLikeSymbol(Symbol symbol)
        => ReferenceSearchService.NormalizeSymbol(symbol);
}
