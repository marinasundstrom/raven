namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

internal interface ILexer
{
    bool IsEndOfFile { get; }

    SyntaxToken ReadToken();

    IEnumerable<SyntaxToken> ReadTokens(int count);

    void ReadAndDiscardTokens(int count);

    SyntaxToken PeekToken(int index = 0);
}