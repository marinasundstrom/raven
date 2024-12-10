namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class VariableDeclaratorSyntax : StatementSyntax
{
    public VariableDeclaratorSyntax(
        IdentifierNameSyntax name,
        IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(
              SyntaxKind.VariableDeclarator,
              [
                    name,
                    null,
                    null,
              ],
              diagnostics)
    {
    }

    public VariableDeclaratorSyntax(
        IdentifierNameSyntax name,
        TypeAnnotationSyntax typeAnnotation,
        IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(
              SyntaxKind.VariableDeclarator,
              [
                    name,
                    typeAnnotation,
                    null,
              ],
              diagnostics)
    {
    }

    public VariableDeclaratorSyntax(
        IdentifierNameSyntax name,
        EqualsValueClauseSyntax equalsValueClause,
        IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(
              SyntaxKind.VariableDeclarator,
              [
                    name,
                    null,
                    equalsValueClause
              ],
              diagnostics)
    {
    }

    public VariableDeclaratorSyntax(
        IdentifierNameSyntax name,
        TypeAnnotationSyntax typeAnnotation,
        EqualsValueClauseSyntax equalsValueClause,
        IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(
              SyntaxKind.VariableDeclarator,
              [
                    name,
                    typeAnnotation,
                    equalsValueClause,
              ],
              diagnostics)
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent, int position)
    {
        return new Syntax.VariableDeclaratorSyntax(this, parent, position);
    }
}