namespace Raven.CodeAnalysis.Syntax;

public class SkippedTokensTrivia : StructuredTriviaSyntax
{
    internal SkippedTokensTrivia(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }

    public SkippedTokensTrivia(SyntaxTokenList tokens)
        : base(new InternalSyntax.SkippedTokensTrivia(tokens.Green), null)
    {

    }

    public SyntaxTokenList Tokens => new SyntaxTokenList((InternalSyntax.SyntaxList)Green.GetSlot(0), this, Position + Green.GetChildStartPosition(0));

    public override void Accept(SyntaxVisitor visitor)
    {
        visitor.Visit(this);
    }

    public override TResult Accept<TResult>(SyntaxVisitor<TResult> visitor)
    {
        return visitor.Visit(this);
    }
}