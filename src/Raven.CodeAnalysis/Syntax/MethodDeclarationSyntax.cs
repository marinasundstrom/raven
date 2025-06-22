namespace Raven.CodeAnalysis.Syntax;

public partial class MethodDeclarationSyntax : BaseMethodDeclarationSyntax
{
    public override partial SyntaxTokenList Modifiers { get; }
    public partial SyntaxToken FuncKeyword { get; }
    public partial SyntaxToken Identifier { get; }
    public override partial ParameterListSyntax ParameterList { get; }
    public partial ReturnTypeAnnotationSyntax ReturnType { get; }
    public override partial BlockSyntax? Body { get; }

    internal MethodDeclarationSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public MethodDeclarationSyntax(SyntaxTokenList modifiers, SyntaxToken funcKeyword, SyntaxToken identifier, ParameterListSyntax parameters, ReturnTypeAnnotationSyntax returnType)
        : this(new InternalSyntax.MethodDeclarationSyntax(modifiers.Green, funcKeyword.Green, identifier.Green, (InternalSyntax.ParameterListSyntax)parameters.Green, (InternalSyntax.ReturnTypeAnnotationSyntax)returnType.Green, null))
    {
    }

    public MethodDeclarationSyntax(SyntaxTokenList modifiers, SyntaxToken funcKeyword, SyntaxToken identifier, ParameterListSyntax parameters, ReturnTypeAnnotationSyntax returnType, BlockSyntax? body)
    : this(new InternalSyntax.MethodDeclarationSyntax(modifiers.Green, funcKeyword.Green, identifier.Green, (InternalSyntax.ParameterListSyntax)parameters.Green, (InternalSyntax.ReturnTypeAnnotationSyntax)returnType.Green, (InternalSyntax.BlockSyntax)body.Green))
    {
    }
}

public static partial class SyntaxFactory
{
    public static MethodDeclarationSyntax MethodDeclaration(SyntaxTokenList modifiers, SyntaxToken funcKeyword, SyntaxToken identifier, ParameterListSyntax parameters, ReturnTypeAnnotationSyntax returnType, BlockSyntax? body)
        => new MethodDeclarationSyntax(modifiers, funcKeyword, identifier, parameters, returnType, body);
}