namespace Raven.CodeAnalysis.Syntax;

public static class SyntaxExtensions
{
    internal static string ToValueString(this NameSyntax name)
        => string.Concat(name.DescendantTokens().Select(token => token.ValueText));

    public static bool IsKind(this SyntaxToken token, SyntaxKind kind) => token.Kind == kind;

    public static bool IsKind(this SyntaxNode node, SyntaxKind kind) => node.Kind == kind;
}
