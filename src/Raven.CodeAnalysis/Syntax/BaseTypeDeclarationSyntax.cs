namespace Raven.CodeAnalysis.Syntax;

public abstract partial class BaseTypeDeclarationSyntax : MemberDeclarationSyntax
{
    internal BaseTypeDeclarationSyntax(GreenNode greenNode, SyntaxNode? parent, int position) : base(greenNode, parent, position)
    {
    }

    public abstract SyntaxToken Identifier { get; }

    public abstract SyntaxToken OpenBraceToken { get; }

    public abstract SyntaxToken CloseBraceToken { get; }

    public abstract SyntaxToken? TerminatorToken { get; }
}