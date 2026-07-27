namespace Raven.CodeAnalysis.Syntax;

public enum ConditionalDirectiveKind
{
    If,
    Elif,
    Else,
    EndIf
}

public sealed class ConditionalDirectiveTriviaSyntax : StructuredTriviaSyntax
{
    internal ConditionalDirectiveTriviaSyntax(GreenNode greenNode, SyntaxNode? parent, int position)
        : base(greenNode, parent, position)
    {
    }

    private InternalSyntax.ConditionalDirectiveTriviaSyntax GreenDirective
        => (InternalSyntax.ConditionalDirectiveTriviaSyntax)Green;

    public SyntaxToken DirectiveToken
        => new(
            (InternalSyntax.SyntaxToken)Green.GetSlot(0),
            this,
            Position + Green.GetChildStartPosition(0));

    public ConditionalDirectiveKind DirectiveKind => GreenDirective.DirectiveKind;

    public string ConditionText => GreenDirective.ConditionText;

    public bool IsBranchActive => GreenDirective.IsBranchActive;

    public bool BranchTaken => GreenDirective.BranchTaken;

    public TextSpan KeywordSpan
        => new(Position + GreenDirective.KeywordOffset, GreenDirective.KeywordLength);

    public TextSpan ConditionSpan
        => new(Position + GreenDirective.ConditionOffset, GreenDirective.ConditionLength);

    public override void Accept(SyntaxVisitor visitor)
        => visitor.DefaultVisit(this);

    public override TResult Accept<TResult>(SyntaxVisitor<TResult> visitor)
        => visitor.DefaultVisit(this);
}
