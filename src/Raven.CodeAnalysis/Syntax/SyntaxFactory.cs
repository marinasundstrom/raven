using System.Runtime.CompilerServices;

namespace Raven.CodeAnalysis.Syntax;

public static partial class SyntaxFactory
{
    public static SyntaxToken MissingToken(SyntaxKind kind) => (SyntaxToken)InternalSyntax.SyntaxFactory.MissingToken(kind);

    public static SyntaxToken Token(SyntaxKind kind) => (SyntaxToken)InternalSyntax.SyntaxFactory.Token(kind);

    public static SyntaxToken Identifier(string text) => (SyntaxToken)InternalSyntax.SyntaxFactory.IdentifierToken(text);

    public static SyntaxToken Literal(int value) => (SyntaxToken)InternalSyntax.SyntaxFactory.Literal(value);
    public static SyntaxToken Literal(string value) => (SyntaxToken)InternalSyntax.SyntaxFactory.Literal(value);

}
