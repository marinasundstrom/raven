namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

internal interface ILexer
{
    bool IsEndOfFile { get; }

    SyntaxToken ReadToken();

    SyntaxToken PeekToken(int index = 0);
}