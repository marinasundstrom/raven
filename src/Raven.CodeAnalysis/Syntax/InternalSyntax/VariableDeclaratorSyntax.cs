namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class VariableDeclaratorSyntax : StatementSyntax
{
    public VariableDeclaratorSyntax(
        IdentifierNameSyntax name,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.VariableDeclarator,
              [
                  name ?? throw new ArgumentNullException(nameof(name)),
                    null,
                    null,
              ],
              diagnostics)
    {
    }

    public VariableDeclaratorSyntax(
        IdentifierNameSyntax name,
        TypeAnnotationSyntax typeAnnotation,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.VariableDeclarator,
              [
                name ?? throw new ArgumentNullException(nameof(name)),
                typeAnnotation ?? throw new ArgumentNullException(nameof(typeAnnotation)),
                null,
              ],
              diagnostics)
    {
    }

    public VariableDeclaratorSyntax(
        IdentifierNameSyntax name,
        EqualsValueClauseSyntax equalsValueClause,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
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
        IEnumerable<DiagnosticInfo>? diagnostics = null)
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
          IEnumerable<DiagnosticInfo>? diagnostics = null)
      => new(name, typeAnnotation, equalsValueClause, diagnostics);
}