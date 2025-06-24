namespace Raven.CodeAnalysis.Syntax;

public partial class AccessorDeclarationSyntax : SyntaxNode
{
    public override partial SyntaxKind Kind { get; }

    public partial SyntaxTokenList Modifiers { get; }

    public partial SyntaxToken Keyword { get; }

    public partial BlockSyntax Body { get; }

    public partial SyntaxToken TerminatorToken { get; }

    internal AccessorDeclarationSyntax(GreenNode greenNode, SyntaxNode parent, int position = 0) : base(greenNode, parent, position)
    {
    }

    public AccessorDeclarationSyntax(SyntaxKind kind, SyntaxTokenList modifiers, SyntaxToken keyword, BlockSyntax body, SyntaxToken terminatorToken)
        : this(new InternalSyntax.AccessorDeclarationSyntax(kind, modifiers.Green, keyword.Green, (InternalSyntax.BlockSyntax)body.Green, terminatorToken.Green), null, 0)
    {
    }
}