namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

internal interface ITokenizer
{
    public int Position { get; }

    InternalSyntax.SyntaxToken ReadToken();

    InternalSyntax.SyntaxToken PeekToken();
}