namespace Raven.CodeAnalysis.Syntax;

public partial class SkippedTokensTrivia : StructuredTriviaSyntax
{
    internal SkippedTokensTrivia(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }

    public SkippedTokensTrivia(SyntaxTokenList tokens)
        : base(new InternalSyntax.SkippedTokensTrivia(tokens.Green), null)
    {

    }

    public partial SyntaxTokenList Tokens { get; }
}