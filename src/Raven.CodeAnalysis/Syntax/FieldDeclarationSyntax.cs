namespace Raven.CodeAnalysis.Syntax;

public partial class FieldDeclarationSyntax : MemberDeclarationSyntax
{
    public override partial SyntaxTokenList Modifiers { get; }
    public partial VariableDeclarationSyntax Declaration { get; }
    public partial SyntaxToken? TerminatorToken { get; }

    internal FieldDeclarationSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public FieldDeclarationSyntax(SyntaxTokenList modifiers, VariableDeclarationSyntax declaration, SyntaxToken? terminatorToken)
        : this(new InternalSyntax.FieldDeclarationSyntax(modifiers.Green, (InternalSyntax.VariableDeclarationSyntax)declaration.Green, terminatorToken?.Green, null))
    {
    }
}

public static partial class SyntaxFactory
{
    public static FieldDeclarationSyntax FieldDeclaration(SyntaxTokenList modifiers, VariableDeclarationSyntax declaration, SyntaxToken? terminatorToken)
        => new FieldDeclarationSyntax(modifiers, declaration, terminatorToken);
}