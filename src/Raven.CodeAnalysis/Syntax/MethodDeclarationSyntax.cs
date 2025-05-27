using System.Reflection.Metadata;

using Raven.CodeAnalysis.Syntax.InternalSyntax;

namespace Raven.CodeAnalysis.Syntax;

public partial class MethodDeclarationSyntax : MemberDeclarationSyntax
{
    public partial SyntaxToken FuncKeyword { get; }
    public partial IdentifierNameSyntax Name { get; }
    public partial ParameterListSyntax ParameterList { get; }
    public partial ReturnTypeAnnotationSyntax ReturnType { get; }
    public partial BlockSyntax? Body { get; }

    internal MethodDeclarationSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public MethodDeclarationSyntax(SyntaxToken funcKeyword, IdentifierNameSyntax name, ParameterListSyntax parameters, ReturnTypeAnnotationSyntax returnType)
        : this(new InternalSyntax.MethodDeclarationSyntax(funcKeyword.Green, (InternalSyntax.IdentifierNameSyntax)name.Green, (InternalSyntax.ParameterListSyntax)parameters.Green, (InternalSyntax.ReturnTypeAnnotationSyntax)returnType.Green, null))
    {
    }

    public MethodDeclarationSyntax(SyntaxToken funcKeyword, IdentifierNameSyntax name, ParameterListSyntax parameters, ReturnTypeAnnotationSyntax returnType, BlockSyntax? body)
    : this(new InternalSyntax.MethodDeclarationSyntax(funcKeyword.Green, (InternalSyntax.IdentifierNameSyntax)name.Green, (InternalSyntax.ParameterListSyntax)parameters.Green, (InternalSyntax.ReturnTypeAnnotationSyntax)returnType.Green, (InternalSyntax.BlockSyntax)body.Green))
    {
    }
}

public static partial class SyntaxFactory
{
    public static MethodDeclarationSyntax MethodDeclaration(SyntaxToken funcKeyword, IdentifierNameSyntax name, ParameterListSyntax parameters, ReturnTypeAnnotationSyntax returnType, BlockSyntax? body)
        => new MethodDeclarationSyntax(funcKeyword, name, parameters, returnType, body);
}

public partial class LocalFunctionStatementSyntax : StatementSyntax
{
    public partial SyntaxToken FuncKeyword { get; }
    public partial IdentifierNameSyntax Name { get; }
    public partial ParameterListSyntax ParameterList { get; }
    public partial ReturnTypeAnnotationSyntax ReturnType { get; }
    public partial BlockSyntax? Body { get; }

    internal LocalFunctionStatementSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public LocalFunctionStatementSyntax(SyntaxToken funcKeyword, IdentifierNameSyntax name, ParameterListSyntax parameters, ReturnTypeAnnotationSyntax returnType)
        : this(new InternalSyntax.LocalFunctionStatementSyntax(funcKeyword.Green, (InternalSyntax.IdentifierNameSyntax)name.Green, (InternalSyntax.ParameterListSyntax)parameters.Green, (InternalSyntax.ReturnTypeAnnotationSyntax)returnType.Green, null))
    {
    }

    public LocalFunctionStatementSyntax(SyntaxToken funcKeyword, IdentifierNameSyntax name, ParameterListSyntax parameters, ReturnTypeAnnotationSyntax returnType, BlockSyntax? body)
    : this(new InternalSyntax.LocalFunctionStatementSyntax(funcKeyword.Green, (InternalSyntax.IdentifierNameSyntax)name.Green, (InternalSyntax.ParameterListSyntax)parameters.Green, (InternalSyntax.ReturnTypeAnnotationSyntax)returnType.Green, (InternalSyntax.BlockSyntax)body.Green))
    {
    }
}

public static partial class SyntaxFactory
{
    public static LocalFunctionStatementSyntax LocalFunctionStatement(SyntaxToken funcKeyword, IdentifierNameSyntax name, ParameterListSyntax parameters, ReturnTypeAnnotationSyntax returnType, BlockSyntax? body)
        => new LocalFunctionStatementSyntax(funcKeyword, name, parameters, returnType, body);
}