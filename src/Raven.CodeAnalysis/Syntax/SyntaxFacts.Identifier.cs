using System;

namespace Raven.CodeAnalysis.Syntax;

public static partial class SyntaxFacts
{
    public static bool CanBeIdentifier(SyntaxKind kind)
    {
        return kind == SyntaxKind.IdentifierToken ||
               (IsKeywordKind(kind) && !IsReservedWordKind(kind));
    }
}
