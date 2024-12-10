using System.Reflection.Metadata;

using Raven.CodeAnalysis.Syntax.InternalSyntax;

namespace Raven.CodeAnalysis.Syntax;

public partial class MethodDeclarationSyntax : MemberDeclarationSyntax
{
    public partial TypeSyntax ReturnType { get; }
    public partial IdentifierNameSyntax Name { get; }
    public partial TypeParameterListSyntax ParameterList { get; }
    public partial BlockSyntax? Body { get; }

    public MethodDeclarationSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public MethodDeclarationSyntax(TypeSyntax returnType, IdentifierNameSyntax name, TypeParameterListSyntax parameters)
        : this(new InternalSyntax.MethodDeclarationSyntax((InternalSyntax.TypeSyntax)returnType.Green, (InternalSyntax.IdentifierNameSyntax)name.Green, (InternalSyntax.TypeParameterListSyntax)parameters.Green))
    {
    }

    public MethodDeclarationSyntax(TypeSyntax returnType, IdentifierNameSyntax name, TypeParameterListSyntax parameters, BlockSyntax? body)
    : this(new InternalSyntax.MethodDeclarationSyntax((InternalSyntax.TypeSyntax)returnType.Green, (InternalSyntax.IdentifierNameSyntax)name.Green, (InternalSyntax.TypeParameterListSyntax)parameters.Green, (InternalSyntax.BlockSyntax)body.Green))
    {
    }

    public override void Accept(SyntaxVisitor visitor)
    {
        visitor.VisitMethodDeclaration(this);
    }

    public override TNode Accept<TNode>(SyntaxVisitor<TNode> visitor)
    {
        return visitor.VisitMethodDeclaration(this);
    }

    public MethodDeclarationSyntax WithBody(BlockSyntax body)
    {
        return new MethodDeclarationSyntax(this.ReturnType, this.Name, this.ParameterList, body);
    }
}

public static partial class SyntaxFactory
{
    public static MethodDeclarationSyntax MethodDeclaration(TypeSyntax returnType, IdentifierNameSyntax name, TypeParameterListSyntax parameters)
        => new MethodDeclarationSyntax(returnType, name, parameters);
}