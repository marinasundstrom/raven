

namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class IndexerDeclarationSyntax : BasePropertyDeclarationSyntax
{
    public IndexerDeclarationSyntax(
        SyntaxList modifiers,
        SyntaxToken identifier,
        BracketedParameterListSyntax parameterList,
        TypeAnnotationClauseSyntax typeAnnotation,
        AccessorListSyntax accessorList,
        EqualsValueClauseSyntax? initializer,
        SyntaxToken? terminatorToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(SyntaxKind.PropertyDeclaration,
              [
                      modifiers ?? throw new ArgumentNullException(nameof(modifiers)),
                      identifier ?? throw new ArgumentNullException(nameof(identifier)),
                      parameterList ?? throw new ArgumentNullException(nameof(parameterList)),
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
    public static IndexerDeclarationSyntax IndexerDeclaration(
        SyntaxList modifiers,
        SyntaxToken identifier,
        BracketedParameterListSyntax parameterList,
        TypeAnnotationClauseSyntax typeAnnotation,
        AccessorListSyntax accessorList,
        EqualsValueClauseSyntax? initializer,
        SyntaxToken? terminatorToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
      => new(modifiers, identifier, parameterList, typeAnnotation, accessorList, initializer, terminatorToken, diagnostics);
}