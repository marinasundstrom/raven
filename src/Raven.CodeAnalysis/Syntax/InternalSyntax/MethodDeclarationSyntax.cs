
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class MethodDeclarationSyntax : MemberDeclarationSyntax
{
    public MethodDeclarationSyntax(
        TypeSyntax returnType,
        IdentifierNameSyntax name,
        ParameterListSyntax parameters,
        IEnumerable<Diagnostic>? diagnostics = null)
        : base(SyntaxKind.MethodDeclaration,
              [
                      returnType,
                      name,
                      parameters,
                      null
              ],
              diagnostics)
    {
    }

    public MethodDeclarationSyntax(
        TypeSyntax returnType,
        IdentifierNameSyntax name,
        ParameterListSyntax parameters,
        BlockSyntax body,
        IEnumerable<Diagnostic>? diagnostics = null)
    : base(SyntaxKind.MethodDeclaration,
          [
                returnType,
                name,
                parameters,
                body
          ],
          diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static MethodDeclarationSyntax MethodDeclaration(
        TypeSyntax returnType,
        IdentifierNameSyntax name,
        ParameterListSyntax parameters,
        BlockSyntax body,
        IEnumerable<Diagnostic>? diagnostics = null)
        => new(returnType, name, parameters, body, diagnostics);
}