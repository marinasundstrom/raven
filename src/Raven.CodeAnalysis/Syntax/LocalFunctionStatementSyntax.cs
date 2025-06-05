namespace Raven.CodeAnalysis.Syntax;

public partial class LocalFunctionStatementSyntax : StatementSyntax
{
    public partial SyntaxToken FuncKeyword { get; }
    public partial SyntaxToken Identifier { get; }
    public partial ParameterListSyntax ParameterList { get; }
    public partial ReturnTypeAnnotationSyntax ReturnType { get; }
    public partial BlockSyntax? Body { get; }

    internal LocalFunctionStatementSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public LocalFunctionStatementSyntax(SyntaxToken funcKeyword, SyntaxToken identifier, ParameterListSyntax parameters, ReturnTypeAnnotationSyntax returnType)
        : this(new InternalSyntax.LocalFunctionStatementSyntax(funcKeyword.Green, identifier.Green, (InternalSyntax.ParameterListSyntax)parameters.Green, (InternalSyntax.ReturnTypeAnnotationSyntax)returnType.Green, null))
    {
    }

    public LocalFunctionStatementSyntax(SyntaxToken funcKeyword, SyntaxToken identifier, ParameterListSyntax parameters, ReturnTypeAnnotationSyntax returnType, BlockSyntax? body)
    : this(new InternalSyntax.LocalFunctionStatementSyntax(funcKeyword.Green, identifier.Green, (InternalSyntax.ParameterListSyntax)parameters.Green, (InternalSyntax.ReturnTypeAnnotationSyntax)returnType.Green, (InternalSyntax.BlockSyntax)body.Green))
    {
    }
}

public static partial class SyntaxFactory
{
    public static LocalFunctionStatementSyntax LocalFunctionStatement(SyntaxToken funcKeyword, SyntaxToken identifier, ParameterListSyntax parameters, ReturnTypeAnnotationSyntax returnType, BlockSyntax? body)
        => new LocalFunctionStatementSyntax(funcKeyword, identifier, parameters, returnType, body);
}