namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

internal interface ILexer
{
    bool IsEndOfLine { get; }

    InternalSyntax.SyntaxToken ReadToken();

    InternalSyntax.SyntaxToken PeekToken();
}