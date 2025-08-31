namespace Raven.CodeAnalysis.Syntax;

public static class SyntaxTokenExtension
{
    public static bool IsKeyword(this SyntaxToken token)
    {
        return SyntaxFacts.IsKeywordKind(token.Kind);
    }

    public static bool IsReservedWord(this SyntaxToken token)
    {
        return SyntaxFacts.IsReservedWordKind(token.Kind);
    }

    public static bool CanBeIdentifier(this SyntaxToken token)
    {
        return SyntaxFacts.CanBeIdentifier(token.Kind);
    }
}
