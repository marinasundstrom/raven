using System.Reflection.Metadata;

using Raven.CodeAnalysis.Syntax.InternalSyntax;

namespace Raven.CodeAnalysis.Syntax;

public partial class MethodDeclarationSyntax : MemberDeclarationSyntax
{
    public partial SyntaxToken FunKeyword { get; }
    public partial IdentifierNameSyntax Name { get; }
    public partial ParameterListSyntax ParameterList { get; }
    public partial TypeAnnotationSyntax ReturnType { get; }
    public partial BlockSyntax? Body { get; }

    internal MethodDeclarationSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public MethodDeclarationSyntax(SyntaxToken funKeyword, IdentifierNameSyntax name, ParameterListSyntax parameters, TypeAnnotationSyntax returnType)
        : this(new InternalSyntax.MethodDeclarationSyntax(funKeyword.Green, (InternalSyntax.IdentifierNameSyntax)name.Green, (InternalSyntax.ParameterListSyntax)parameters.Green, (InternalSyntax.TypeAnnotationSyntax)returnType.Green, null))
    {
    }

    public MethodDeclarationSyntax(SyntaxToken funKeyword, IdentifierNameSyntax name, ParameterListSyntax parameters, TypeAnnotationSyntax returnType, BlockSyntax? body)
    : this(new InternalSyntax.MethodDeclarationSyntax(funKeyword.Green, (InternalSyntax.IdentifierNameSyntax)name.Green, (InternalSyntax.ParameterListSyntax)parameters.Green, (InternalSyntax.TypeAnnotationSyntax)returnType.Green, (InternalSyntax.BlockSyntax)body.Green))
    {
    }
}

public static partial class SyntaxFactory
{
    public static MethodDeclarationSyntax MethodDeclaration(SyntaxToken funKeyword, IdentifierNameSyntax name, ParameterListSyntax parameters, TypeAnnotationSyntax returnType, BlockSyntax? body)
        => new MethodDeclarationSyntax(funKeyword, name, parameters, returnType, body);
}

public partial class LocalFunctionStatementSyntax : StatementSyntax
{
    public partial SyntaxToken FunKeyword { get; }
    public partial IdentifierNameSyntax Name { get; }
    public partial ParameterListSyntax ParameterList { get; }
    public partial TypeAnnotationSyntax ReturnType { get; }
    public partial BlockSyntax? Body { get; }

    internal LocalFunctionStatementSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public LocalFunctionStatementSyntax(SyntaxToken funKeyword, IdentifierNameSyntax name, ParameterListSyntax parameters, TypeAnnotationSyntax returnType)
        : this(new InternalSyntax.LocalFunctionStatementSyntax(funKeyword.Green, (InternalSyntax.IdentifierNameSyntax)name.Green, (InternalSyntax.ParameterListSyntax)parameters.Green, (InternalSyntax.TypeAnnotationSyntax)returnType.Green, null))
    {
    }

    public LocalFunctionStatementSyntax(SyntaxToken funKeyword, IdentifierNameSyntax name, ParameterListSyntax parameters, TypeAnnotationSyntax returnType, BlockSyntax? body)
    : this(new InternalSyntax.LocalFunctionStatementSyntax(funKeyword.Green, (InternalSyntax.IdentifierNameSyntax)name.Green, (InternalSyntax.ParameterListSyntax)parameters.Green, (InternalSyntax.TypeAnnotationSyntax)returnType.Green, (InternalSyntax.BlockSyntax)body.Green))
    {
    }
}

public static partial class SyntaxFactory
{
    public static LocalFunctionStatementSyntax LocalFunctionStatement(SyntaxToken funKeyword, IdentifierNameSyntax name, ParameterListSyntax parameters, TypeAnnotationSyntax returnType, BlockSyntax? body)
        => new LocalFunctionStatementSyntax(funKeyword, name, parameters, returnType, body);
}