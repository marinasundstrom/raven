namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class VariableDeclaratorSyntax : StatementSyntax
{
    public VariableDeclaratorSyntax(
        IdentifierNameSyntax name)
        : base(
              SyntaxKind.VariableDeclarator,
              [
                    name,
                    null,
                    null,
              ])
    {
    }

    public VariableDeclaratorSyntax(
        IdentifierNameSyntax name,
        TypeAnnotationSyntax typeAnnotation)
        : base(
              SyntaxKind.VariableDeclarator,
              [
                    name,
                    typeAnnotation,
                    null,
              ])
    {
    }

    public VariableDeclaratorSyntax(
        IdentifierNameSyntax name,
        EqualsValueClauseSyntax equalsValueClause)
        : base(
              SyntaxKind.VariableDeclarator,
              [
                    name,
                    null,
                    equalsValueClause
              ])
    {
    }

    public VariableDeclaratorSyntax(
        IdentifierNameSyntax name,
        TypeAnnotationSyntax typeAnnotation,
        EqualsValueClauseSyntax equalsValueClause)
        : base(
              SyntaxKind.VariableDeclarator,
              [
                    name,
                    typeAnnotation,
                    equalsValueClause,
              ])
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent, int position)
    {
        return new Syntax.VariableDeclaratorSyntax(this, parent, position);
    }
}