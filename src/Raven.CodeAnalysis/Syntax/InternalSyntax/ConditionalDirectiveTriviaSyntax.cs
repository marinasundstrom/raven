namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal sealed class ConditionalDirectiveTriviaSyntax : SyntaxNode
{
    public ConditionalDirectiveTriviaSyntax(
        SyntaxKind kind,
        SyntaxToken directiveToken,
        ConditionalDirectiveKind directiveKind,
        string conditionText,
        bool isBranchActive,
        bool branchTaken,
        int keywordOffset,
        int keywordLength,
        int conditionOffset,
        int conditionLength,
        IEnumerable<DiagnosticInfo>? diagnostics = null,
        IEnumerable<SyntaxAnnotation>? annotations = null)
        : base(kind, [directiveToken], diagnostics, annotations)
    {
        DirectiveKind = directiveKind;
        ConditionText = conditionText;
        IsBranchActive = isBranchActive;
        BranchTaken = branchTaken;
        KeywordOffset = keywordOffset;
        KeywordLength = keywordLength;
        ConditionOffset = conditionOffset;
        ConditionLength = conditionLength;
    }

    public ConditionalDirectiveKind DirectiveKind { get; }

    public string ConditionText { get; }

    public bool IsBranchActive { get; }

    public bool BranchTaken { get; }

    public int KeywordOffset { get; }

    public int KeywordLength { get; }

    public int ConditionOffset { get; }

    public int ConditionLength { get; }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent, int position)
        => new Syntax.ConditionalDirectiveTriviaSyntax(this, parent, position);

    internal override void Accept(SyntaxVisitor visitor)
        => visitor.DefaultVisit(this);

    internal override TResult Accept<TResult>(SyntaxVisitor<TResult> visitor)
        => visitor.DefaultVisit(this);

    internal override GreenNode With(
        GreenNode[] children,
        DiagnosticInfo[]? diagnostics = null,
        SyntaxAnnotation[]? annotations = null)
        => new ConditionalDirectiveTriviaSyntax(
            Kind,
            (SyntaxToken)children[0],
            DirectiveKind,
            ConditionText,
            IsBranchActive,
            BranchTaken,
            KeywordOffset,
            KeywordLength,
            ConditionOffset,
            ConditionLength,
            diagnostics ?? _diagnostics,
            annotations ?? _annotations);
}
