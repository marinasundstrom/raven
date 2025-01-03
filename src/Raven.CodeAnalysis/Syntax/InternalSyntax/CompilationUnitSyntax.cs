namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class CompilationUnitSyntax : SyntaxNode
{
    public CompilationUnitSyntax(
        SyntaxList imports,
        SyntaxList members,
        SyntaxToken endOfFileToken,
        IEnumerable<Diagnostic>? diagnostics = null)
        : base(
              SyntaxKind.CompilationUnit,
              [
                      imports,
                      members,
                      endOfFileToken
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static CompilationUnitSyntax CompilationUnit(
        SyntaxList imports,
        SyntaxList members,
        SyntaxToken endOfFileToken,
        IEnumerable<Diagnostic>? diagnostics = null)
        => new(imports, members, endOfFileToken, diagnostics);
}