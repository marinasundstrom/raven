namespace Raven.CodeAnalysis.Syntax;

public partial class MethodDeclarationSyntax : BaseMethodDeclarationSyntax
{
    public override partial SyntaxTokenList Modifiers { get; }
    public partial SyntaxToken Identifier { get; }
    public override partial ParameterListSyntax ParameterList { get; }
    public partial ReturnTypeAnnotationSyntax ReturnType { get; }
    public override partial BlockSyntax? Body { get; }
    public override partial ArrowExpressionClauseSyntax? ExpressionBody { get; }
    public partial SyntaxToken? TerminatorToken { get; }

    internal MethodDeclarationSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public MethodDeclarationSyntax(SyntaxTokenList modifiers, SyntaxToken identifier, ParameterListSyntax parameters, ReturnTypeAnnotationSyntax returnType, BlockSyntax body, SyntaxToken? terminatorToken)
    : this(new InternalSyntax.MethodDeclarationSyntax(modifiers.Green, identifier.Green, (InternalSyntax.ParameterListSyntax)parameters.Green, (InternalSyntax.ReturnTypeAnnotationSyntax)returnType.Green, (InternalSyntax.BlockSyntax)body.Green, terminatorToken?.Green))
    {
    }

    public MethodDeclarationSyntax(SyntaxTokenList modifiers, SyntaxToken identifier, ParameterListSyntax parameters, ReturnTypeAnnotationSyntax returnType, ArrowExpressionClauseSyntax expressionBody, SyntaxToken? terminatorToken)
    : this(new InternalSyntax.MethodDeclarationSyntax(modifiers.Green, identifier.Green, (InternalSyntax.ParameterListSyntax)parameters.Green, (InternalSyntax.ReturnTypeAnnotationSyntax)returnType.Green, (InternalSyntax.ArrowExpressionClauseSyntax)expressionBody.Green, terminatorToken?.Green, null))
    {
    }

    public MethodDeclarationSyntax(SyntaxTokenList modifiers, SyntaxToken identifier, ParameterListSyntax parameters, ReturnTypeAnnotationSyntax returnType, BlockSyntax? body, ArrowExpressionClauseSyntax? expressionBody, SyntaxToken? terminatorToken)
: this(new InternalSyntax.MethodDeclarationSyntax(modifiers.Green, identifier.Green, (InternalSyntax.ParameterListSyntax)parameters.Green, (InternalSyntax.ReturnTypeAnnotationSyntax)returnType.Green, (InternalSyntax.BlockSyntax)body?.Green, (InternalSyntax.ArrowExpressionClauseSyntax)expressionBody?.Green, terminatorToken?.Green, null))
    {
    }
}

public static partial class SyntaxFactory
{
    public static MethodDeclarationSyntax MethodDeclaration(SyntaxTokenList modifiers, SyntaxToken identifier, ParameterListSyntax parameters, ReturnTypeAnnotationSyntax returnType, BlockSyntax body, SyntaxToken? terminatorToken)
        => new MethodDeclarationSyntax(modifiers, identifier, parameters, returnType, body, terminatorToken);

    public static MethodDeclarationSyntax MethodDeclaration(SyntaxTokenList modifiers, SyntaxToken identifier, ParameterListSyntax parameters, ReturnTypeAnnotationSyntax returnType, ArrowExpressionClauseSyntax expressionBody, SyntaxToken? terminatorToken)
        => new MethodDeclarationSyntax(modifiers, identifier, parameters, returnType, expressionBody, terminatorToken);
}