using System;

namespace Raven.CodeAnalysis.Syntax;

public static partial class SyntaxFacts
{
    public static bool IsKeywordKind(SyntaxKind kind)
    {
        return kind.ToString().EndsWith("Keyword", StringComparison.Ordinal);
    }

    public static bool CanBeIdentifier(SyntaxKind kind)
    {
        return kind == SyntaxKind.IdentifierToken ||
               (IsKeywordKind(kind) && !IsReservedWordKind(kind));
    }
}
