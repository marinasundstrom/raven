namespace Raven.CodeAnalysis.Syntax;

public abstract partial class TypeDeclarationSyntax : BaseTypeDeclarationSyntax
{
    internal TypeDeclarationSyntax(GreenNode greenNode, SyntaxNode? parent, int position) : base(greenNode, parent, position)
    {
    }

    public abstract int Arity { get; }

    //public abstract SyntaxList<TypeParameterConstraintClauseSyntax> ConstraintClauses { get; }

    public abstract SyntaxToken Keyword { get; }

    public abstract SyntaxList<MemberDeclarationSyntax>? Members { get; }

    public abstract ParameterListSyntax ParameterList { get; }

    //public abstract TypeParameterListSyntax TypeParameterList { get; }

}