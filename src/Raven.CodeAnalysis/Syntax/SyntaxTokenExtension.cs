namespace Raven.CodeAnalysis.Syntax;

public static class SyntaxTokenExtension
{
    public static bool IsKeyword(this SyntaxToken token)
    {
        return SyntaxFacts.IsKeywordKind(token.Kind);
    }
}