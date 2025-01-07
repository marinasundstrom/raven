namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class CompilationUnitSyntax : SyntaxNode
{
    public CompilationUnitSyntax(
        SyntaxList imports,
        SyntaxList members,
        SyntaxToken endOfFileToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.CompilationUnit,
              [
                      imports ?? throw new ArgumentNullException(nameof(imports)),
                      members ?? throw new ArgumentNullException(nameof(members)),
                      endOfFileToken ?? throw new ArgumentNullException(nameof(endOfFileToken))
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
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(imports, members, endOfFileToken, diagnostics);
}