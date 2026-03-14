namespace Raven.CodeAnalysis.Syntax;

/// <summary>
/// Formats Raven syntax trees and fragments.
/// </summary>
public static class Formatter
{
    public static readonly SyntaxAnnotation Annotation = new("Formatter");

    public static TSyntax Format<TSyntax>(TSyntax node, int indentSize = 4)
        where TSyntax : SyntaxNode
    {
        return new SyntaxNormalizer(indentSize).Format(node, useFormatterAnnotation: true);
    }
}
