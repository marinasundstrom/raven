

namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class PropertyDeclarationSyntax : BasePropertyDeclarationSyntax
{
    public PropertyDeclarationSyntax(
        SyntaxList modifiers,
        SyntaxToken identifier,
        ArrowTypeClauseSyntax returnTypeAnnotation,
        AccessorListSyntax accessorList,
        SyntaxToken? terminatorToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(SyntaxKind.PropertyDeclaration,
              [
                      modifiers ?? throw new ArgumentNullException(nameof(modifiers)),
                      identifier ?? throw new ArgumentNullException(nameof(identifier)),
                      returnTypeAnnotation,
                      accessorList ?? throw new ArgumentNullException(nameof(accessorList)),
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
        ArrowTypeClauseSyntax returnTypeAnnotation,
        AccessorListSyntax accessorList,
        SyntaxToken? terminatorToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
      => new(modifiers, identifier, returnTypeAnnotation, accessorList, terminatorToken, diagnostics);
}