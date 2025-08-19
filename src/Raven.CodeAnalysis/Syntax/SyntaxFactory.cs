using System.Runtime.CompilerServices;

namespace Raven.CodeAnalysis.Syntax;

public static partial class SyntaxFactory
{
    public static SyntaxToken MissingToken(SyntaxKind kind) => (SyntaxToken)InternalSyntax.SyntaxFactory.MissingToken(kind);

    public static SyntaxToken Token(SyntaxKind kind) => (SyntaxToken)InternalSyntax.SyntaxFactory.Token(kind);

    public static SyntaxToken IdentifierToken(string text) => (SyntaxToken)InternalSyntax.SyntaxFactory.IdentifierToken(text);
    public static SyntaxToken NumericLiteral(int value) => (SyntaxToken)InternalSyntax.SyntaxFactory.NumericLiteral(value);
}