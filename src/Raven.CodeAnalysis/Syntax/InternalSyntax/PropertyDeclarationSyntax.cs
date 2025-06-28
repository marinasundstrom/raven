

namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class PropertyDeclarationSyntax : BasePropertyDeclarationSyntax
{
    public PropertyDeclarationSyntax(
        SyntaxList modifiers,
        SyntaxToken identifier,
        TypeAnnotationClauseSyntax typeAnnotation,
        AccessorListSyntax accessorList,
        EqualsValueClauseSyntax? initializer,
        SyntaxToken? terminatorToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(SyntaxKind.PropertyDeclaration,
              [
                      modifiers ?? throw new ArgumentNullException(nameof(modifiers)),
                      identifier ?? throw new ArgumentNullException(nameof(identifier)),
                      typeAnnotation ?? throw new ArgumentNullException(nameof(typeAnnotation)),
                      accessorList ?? throw new ArgumentNullException(nameof(accessorList)),
                      initializer,
                      terminatorToken
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static PropertyDeclarationSyntax PropertyDeclaration(
        SyntaxList modifiers,
        SyntaxToken identifier,
        TypeAnnotationClauseSyntax typeAnnotation,
        AccessorListSyntax accessorList,
        EqualsValueClauseSyntax? initializer,
        SyntaxToken? terminatorToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
      => new(modifiers, identifier, typeAnnotation, accessorList, initializer, terminatorToken, diagnostics);
}