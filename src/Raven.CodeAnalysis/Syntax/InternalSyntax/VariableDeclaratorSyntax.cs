namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class VariableDeclaratorSyntax : StatementSyntax
{
    public VariableDeclaratorSyntax(
        SyntaxToken identifier,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.VariableDeclarator,
              [
                  identifier ?? throw new ArgumentNullException(nameof(identifier)),
                    null,
                    null,
              ],
              diagnostics)
    {
    }

    public VariableDeclaratorSyntax(
        SyntaxToken identifier,
        TypeAnnotationSyntax typeAnnotation,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.VariableDeclarator,
              [
                identifier ?? throw new ArgumentNullException(nameof(identifier)),
                typeAnnotation ?? throw new ArgumentNullException(nameof(typeAnnotation)),
                null,
              ],
              diagnostics)
    {
    }

    public VariableDeclaratorSyntax(
        SyntaxToken identifier,
        EqualsValueClauseSyntax equalsValueClause,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.VariableDeclarator,
              [
                    identifier,
                    null,
                    equalsValueClause
              ], diagnostics)
    {
    }

    public VariableDeclaratorSyntax(
        SyntaxToken identifier,
        TypeAnnotationSyntax typeAnnotation,
        EqualsValueClauseSyntax equalsValueClause,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.VariableDeclarator,
              [
                    identifier,
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
          SyntaxToken identifier,
          TypeAnnotationSyntax typeAnnotation,
          EqualsValueClauseSyntax equalsValueClause,
          IEnumerable<DiagnosticInfo>? diagnostics = null)
      => new(identifier, typeAnnotation, equalsValueClause, diagnostics);
}