namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

internal interface ITokenizer
{
    public int Position { get; }

    SyntaxToken ReadToken();

    SyntaxToken PeekToken(int index = 0);
}