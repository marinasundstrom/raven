namespace Raven.CodeAnalysis.Syntax;

public partial class IndexerDeclarationSyntax : BasePropertyDeclarationSyntax
{
    internal IndexerDeclarationSyntax(GreenNode greenNode, SyntaxNode? parent, int position) : base(greenNode, parent, position)
    {
    }

    public IndexerDeclarationSyntax(SyntaxTokenList modifiers, SyntaxToken identifier, BracketedParameterListSyntax parameterList, TypeAnnotationClauseSyntax type, AccessorListSyntax? accessorList, EqualsValueClauseSyntax? initializer, SyntaxToken? terminatorToken)
        : this(new InternalSyntax.IndexerDeclarationSyntax(modifiers.Green, identifier.Green, (InternalSyntax.BracketedParameterListSyntax)parameterList.Green, (InternalSyntax.TypeAnnotationClauseSyntax)type.Green, (InternalSyntax.AccessorListSyntax)accessorList.Green, (InternalSyntax.EqualsValueClauseSyntax?)initializer?.Green, terminatorToken?.Green), null, 0)
    {

    }

    public override partial SyntaxTokenList Modifiers { get; }

    public override partial SyntaxToken Identifier { get; }

    public partial BracketedParameterListSyntax ParameterList { get; }

    public override partial TypeAnnotationClauseSyntax Type { get; }

    public override partial AccessorListSyntax? AccessorList { get; }

    public partial EqualsValueClauseSyntax? Initializer { get; }

    public partial SyntaxToken? TerminatorToken { get; }
}

public static partial class SyntaxFactory
{
    public static IndexerDeclarationSyntax IndexerDeclaration(SyntaxTokenList modifiers, SyntaxToken identifier, BracketedParameterListSyntax parameterList, TypeAnnotationClauseSyntax type, AccessorListSyntax? accessorList, EqualsValueClauseSyntax? initializer, SyntaxToken? terminatorToken)
        => new(modifiers, identifier, parameterList, type, accessorList, initializer, terminatorToken);
}