using System.Reflection.Metadata;

using Raven.CodeAnalysis.Syntax.InternalSyntax;

namespace Raven.CodeAnalysis.Syntax;

public partial class MethodDeclarationSyntax : MemberDeclarationSyntax
{
    public partial TypeSyntax ReturnType { get; }
    public partial IdentifierNameSyntax Name { get; }
    public partial ParameterListSyntax ParameterList { get; }
    public partial BlockSyntax? Body { get; }

    internal MethodDeclarationSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public MethodDeclarationSyntax(TypeSyntax returnType, IdentifierNameSyntax name, ParameterListSyntax parameters)
        : this(new InternalSyntax.MethodDeclarationSyntax((InternalSyntax.TypeSyntax)returnType.Green, (InternalSyntax.IdentifierNameSyntax)name.Green, (InternalSyntax.ParameterListSyntax)parameters.Green))
    {
    }

    public MethodDeclarationSyntax(TypeSyntax returnType, IdentifierNameSyntax name, ParameterListSyntax parameters, BlockSyntax? body)
    : this(new InternalSyntax.MethodDeclarationSyntax((InternalSyntax.TypeSyntax)returnType.Green, (InternalSyntax.IdentifierNameSyntax)name.Green, (InternalSyntax.ParameterListSyntax)parameters.Green, (InternalSyntax.BlockSyntax)body.Green))
    {
    }
}

public static partial class SyntaxFactory
{
    public static MethodDeclarationSyntax MethodDeclaration(TypeSyntax returnType, IdentifierNameSyntax name, ParameterListSyntax parameters)
        => new MethodDeclarationSyntax(returnType, name, parameters);
}