namespace Raven.CodeAnalysis.Syntax.Parser;

internal interface ILexer
{
    bool IsEndOfLine { get; }

    InternalSyntax.SyntaxToken ReadToken();

    InternalSyntax.SyntaxToken PeekToken();
}