namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class VariableDeclaratorSyntax : StatementSyntax
{
    public VariableDeclaratorSyntax(
        IdentifierNameSyntax name,
        IEnumerable<Diagnostic>? diagnostics = null)
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
        IEnumerable<Diagnostic>? diagnostics = null)
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
        IEnumerable<Diagnostic>? diagnostics = null)
        : base(
              SyntaxKind.VariableDeclarator,
              [
                    name,
                    null,
                    equalsValueClause
              ], diagnostics)
    {
    }

    public VariableDeclaratorSyntax(
        IdentifierNameSyntax name,
        TypeAnnotationSyntax typeAnnotation,
        EqualsValueClauseSyntax equalsValueClause,
        IEnumerable<Diagnostic>? diagnostics = null)
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
}

internal static partial class SyntaxFactory
{
    public static VariableDeclaratorSyntax VariableDeclarator(
          IdentifierNameSyntax name,
          TypeAnnotationSyntax typeAnnotation,
          EqualsValueClauseSyntax equalsValueClause,
          IEnumerable<Diagnostic>? diagnostics = null)
      => new(name, typeAnnotation, equalsValueClause, diagnostics);
}