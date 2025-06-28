namespace Raven.CodeAnalysis.Syntax;

public partial class ConstructorDeclarationSyntax : BaseMethodDeclarationSyntax
{
    public override partial SyntaxTokenList Modifiers { get; }
    public partial SyntaxToken InitKeyword { get; }
    public partial SyntaxToken? Identifier { get; }
    public override partial ParameterListSyntax ParameterList { get; }
    public override partial BlockSyntax? Body { get; }
    public override partial ArrowExpressionClauseSyntax? ExpressionBody { get; }
    public partial SyntaxToken? TerminatorToken { get; }

    internal ConstructorDeclarationSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public ConstructorDeclarationSyntax(SyntaxTokenList modifiers, SyntaxToken initKeyword, SyntaxToken? identifier, ParameterListSyntax parameters, BlockSyntax body, SyntaxToken? terminatorToken)
    : this(new InternalSyntax.ConstructorDeclarationSyntax(modifiers.Green, initKeyword.Green, identifier?.Green, (InternalSyntax.ParameterListSyntax)parameters.Green, (InternalSyntax.BlockSyntax)body.Green, terminatorToken?.Green))
    {
    }

    public ConstructorDeclarationSyntax(SyntaxTokenList modifiers, SyntaxToken initKeyword, SyntaxToken? identifier, ParameterListSyntax parameters, ArrowExpressionClauseSyntax expressionBody, SyntaxToken? terminatorToken)
    : this(new InternalSyntax.ConstructorDeclarationSyntax(modifiers.Green, initKeyword.Green, identifier?.Green, (InternalSyntax.ParameterListSyntax)parameters.Green, (InternalSyntax.ArrowExpressionClauseSyntax)expressionBody.Green, terminatorToken?.Green, null))
    {
    }

    public ConstructorDeclarationSyntax(SyntaxTokenList modifiers, SyntaxToken initKeyword, SyntaxToken? identifier, ParameterListSyntax parameters, BlockSyntax? body, ArrowExpressionClauseSyntax? expressionBody, SyntaxToken? terminatorToken)
: this(new InternalSyntax.ConstructorDeclarationSyntax(modifiers.Green, initKeyword.Green, identifier?.Green, (InternalSyntax.ParameterListSyntax)parameters.Green, (InternalSyntax.ArrowExpressionClauseSyntax)expressionBody?.Green, terminatorToken?.Green, null))
    {
    }
}

public static partial class SyntaxFactory
{
    public static ConstructorDeclarationSyntax ConstructorDeclaration(SyntaxTokenList modifiers, SyntaxToken initKeyword, SyntaxToken? identifier, ParameterListSyntax parameters, BlockSyntax body, SyntaxToken? terminatorToken)
        => new ConstructorDeclarationSyntax(modifiers, initKeyword, identifier, parameters, body, terminatorToken);

    public static ConstructorDeclarationSyntax ConstructorDeclaration(SyntaxTokenList modifiers, SyntaxToken initKeyword, SyntaxToken? identifier, ParameterListSyntax parameters, ArrowExpressionClauseSyntax expressionBody, SyntaxToken? terminatorToken)
        => new ConstructorDeclarationSyntax(modifiers, initKeyword, identifier, parameters, expressionBody, terminatorToken);
}