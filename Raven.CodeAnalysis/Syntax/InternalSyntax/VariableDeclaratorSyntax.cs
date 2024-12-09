namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class VariableDeclaratorSyntax : StatementSyntax
{
    public VariableDeclaratorSyntax(
        IdentifierNameSyntax name,
        int startPosition = 0,
        IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(
              SyntaxKind.VariableDeclarator,
              [
                    name,
                    null,
                    null,
              ],
              name.FullWidth,
              diagnostics,
              startPosition)
    {
    }

    public VariableDeclaratorSyntax(
        IdentifierNameSyntax name,
        TypeAnnotationSyntax typeAnnotation,
        int startPosition = 0,
        IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(
              SyntaxKind.VariableDeclarator,
              [
                    name,
                    typeAnnotation,
                    null,
              ],
              name.FullWidth + typeAnnotation!.FullWidth,
              diagnostics,
              startPosition)
    {
    }

    public VariableDeclaratorSyntax(
        IdentifierNameSyntax name,
        EqualsValueClauseSyntax equalsValueClause,
        int startPosition = 0,
        IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(
              SyntaxKind.VariableDeclarator,
              [
                    name,
                    null,
                    equalsValueClause
              ],
              name.FullWidth + equalsValueClause!.FullWidth,
              diagnostics,
              startPosition)
    {
    }

    public VariableDeclaratorSyntax(
        IdentifierNameSyntax name,
        TypeAnnotationSyntax typeAnnotation,
        EqualsValueClauseSyntax equalsValueClause,
        int startPosition = 0,
        IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(
              SyntaxKind.VariableDeclarator,
              [
                    name,
                    typeAnnotation,
                    equalsValueClause,
              ],
              name.FullWidth + typeAnnotation!.FullWidth + equalsValueClause.FullWidth,
              diagnostics,
              startPosition)
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent)
    {
        return new Syntax.VariableDeclaratorSyntax(this, parent);
    }
}