namespace Raven.CodeAnalysis.Syntax.Parser;

internal interface ITokenizer
{
    SyntaxToken ReadToken();

    SyntaxToken PeekToken();
}