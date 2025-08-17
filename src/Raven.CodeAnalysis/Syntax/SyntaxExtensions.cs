namespace Raven.CodeAnalysis.Syntax;

public static class SyntaxExtensions
{
    public static bool IsKind(this SyntaxToken token, SyntaxKind kind) => token.Kind == kind;

    public static bool IsKind(this SyntaxNode node, SyntaxKind kind) => node.Kind == kind;
}