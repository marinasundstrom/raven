namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

internal interface ILexer
{
    bool IsEndOfLine { get; }

    SyntaxToken ReadToken();

    SyntaxToken PeekToken(int index = 0);
}