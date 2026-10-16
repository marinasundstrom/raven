using System.Collections.Generic;

namespace Raven.CodeAnalysis.Syntax;

public static partial class SyntaxFacts
{
    private static readonly HashSet<SyntaxKind> s_overloadableOperatorTokens = new()
    {
        SyntaxKind.PlusEqualsToken,
        SyntaxKind.MinusEqualsToken,
        SyntaxKind.StarEqualsToken,
        SyntaxKind.SlashEqualsToken,
        SyntaxKind.PercentEqualsToken,
        SyntaxKind.LessThanLessThanEqualsToken,
        SyntaxKind.GreaterThanGreaterThanEqualsToken,
        SyntaxKind.GreaterThanGreaterThanGreaterThanEqualsToken,
        SyntaxKind.GreaterThanGreaterThanGreaterThanToken,
        SyntaxKind.AmpersandEqualsToken,
        SyntaxKind.BarEqualsToken,
        SyntaxKind.CaretEqualsToken,
        SyntaxKind.PlusToken,
        SyntaxKind.PlusPlusToken,
        SyntaxKind.MinusToken,
        SyntaxKind.MinusMinusToken,
        SyntaxKind.StarToken,
        SyntaxKind.SlashToken,
        SyntaxKind.PercentToken,
        SyntaxKind.CaretToken,
        SyntaxKind.AmpersandToken,
        SyntaxKind.AmpersandAmpersandToken,
        SyntaxKind.BarToken,
        SyntaxKind.BarBarToken,
        SyntaxKind.LessThanLessThanToken,
        SyntaxKind.GreaterThanGreaterThanToken,
        SyntaxKind.AndToken,
        SyntaxKind.OrToken,
        SyntaxKind.EqualsEqualsToken,
        SyntaxKind.NotEqualsToken,
        SyntaxKind.LessThanToken,
        SyntaxKind.LessThanOrEqualsToken,
        SyntaxKind.GreaterThanToken,
        SyntaxKind.GreaterThanOrEqualsToken,
        SyntaxKind.ExclamationToken,
        SyntaxKind.TildeToken,
        SyntaxKind.TrueKeyword,
        SyntaxKind.FalseKeyword,
    };

    internal static int LeadingGreaterThanCount(SyntaxKind kind) => kind switch
    {
        SyntaxKind.GreaterThanToken or SyntaxKind.GreaterThanOrEqualsToken => 1,
        SyntaxKind.GreaterThanGreaterThanToken or SyntaxKind.GreaterThanGreaterThanEqualsToken => 2,
        SyntaxKind.GreaterThanGreaterThanGreaterThanToken or SyntaxKind.GreaterThanGreaterThanGreaterThanEqualsToken => 3,
        _ => 0,
    };

    public static bool IsOverloadableOperatorToken(SyntaxKind kind) => s_overloadableOperatorTokens.Contains(kind);
}
