using System.Collections.Generic;

namespace Raven.CodeAnalysis.Syntax;

public static partial class SyntaxFacts
{
    private static readonly HashSet<SyntaxKind> s_overloadableOperatorTokens = new()
    {
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
        SyntaxKind.AndToken,
        SyntaxKind.OrToken,
        SyntaxKind.EqualsEqualsToken,
        SyntaxKind.NotEqualsToken,
        SyntaxKind.LessThanToken,
        SyntaxKind.LessThanOrEqualsToken,
        SyntaxKind.GreaterThanToken,
        SyntaxKind.GreaterThanOrEqualsToken,
        SyntaxKind.ExclamationToken,
    };

    public static bool IsOverloadableOperatorToken(SyntaxKind kind) => s_overloadableOperatorTokens.Contains(kind);
}
