
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class MethodDeclarationSyntax : BaseMethodDeclarationSyntax
{
    public MethodDeclarationSyntax(
        SyntaxList modifiers,
        SyntaxToken funcKeyword,
        IdentifierNameSyntax name,
        ParameterListSyntax parameters,
        ReturnTypeAnnotationSyntax returnTypeAnnotation,
        BlockSyntax body,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(SyntaxKind.MethodDeclaration,
              [
                      modifiers ?? throw new ArgumentNullException(nameof(modifiers)),
                      funcKeyword ?? throw new ArgumentNullException(nameof(funcKeyword)),
                      name ?? throw new ArgumentNullException(nameof(name)),
                      parameters ?? throw new ArgumentNullException(nameof(parameters)),
                      returnTypeAnnotation ?? throw new ArgumentNullException(nameof(returnTypeAnnotation)),
                      body ?? throw new ArgumentNullException(nameof(body)),
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static MethodDeclarationSyntax MethodDeclaration(
        SyntaxList modifiers,
        SyntaxToken funcKeyword,
        IdentifierNameSyntax name,
        ParameterListSyntax parameters,
        ReturnTypeAnnotationSyntax returnTypeAnnotation,
        BlockSyntax body,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(modifiers, funcKeyword, name, parameters, returnTypeAnnotation, body, diagnostics);
}