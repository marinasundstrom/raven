using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

namespace Raven.LanguageServer;

internal static class SymbolResolutionHelpers
{
    public static bool TryGetPreferredSymbolInfo(SemanticModel semanticModel, SyntaxNode node, out SymbolInfo symbolInfo)
    {
        try
        {
            if (semanticModel.TryGetNodeInterestSymbolInfo(node, out symbolInfo) &&
                (symbolInfo.Symbol is not null || !symbolInfo.CandidateSymbols.IsDefaultOrEmpty))
            {
                return true;
            }

            symbolInfo = semanticModel.GetSymbolInfo(node);
            return true;
        }
        catch
        {
            symbolInfo = default;
            return false;
        }
    }

    public static ISymbol GetNavigationTargetSymbol(SymbolResolutionResult resolution)
        => resolution.Kind switch
        {
            SymbolResolutionKind.InvocationTarget => NormalizeInvocationOrTypeSymbol(resolution.Symbol),
            SymbolResolutionKind.TypePosition => NormalizeInvocationOrTypeSymbol(resolution.Symbol),
            SymbolResolutionKind.MemberAccess => NormalizeMemberLikeSymbol(resolution.Symbol),
            SymbolResolutionKind.MemberSegment => NormalizeMemberLikeSymbol(resolution.Symbol),
            _ => resolution.Symbol
        };

    public static ISymbol GetReferenceTargetSymbol(SymbolResolutionResult resolution)
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

    private static ISymbol NormalizeInvocationOrTypeSymbol(ISymbol symbol)
        => ReferenceSearchService.NormalizeSymbol(symbol);

    private static ISymbol NormalizeMemberLikeSymbol(ISymbol symbol)
        => ReferenceSearchService.NormalizeSymbol(symbol);
}
