namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class CompilationUnitSyntax : SyntaxNode
{
    public CompilationUnitSyntax(
        SyntaxList imports,
        SyntaxList members,
        SyntaxToken endOfFileToken,
        int startPosition = 0,
        IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(
              SyntaxKind.CompilationUnit,
              [
                      imports,
                      members,
                      endOfFileToken
              ],
              (imports?.FullWidth ?? 0) + (members?.FullWidth ?? 0),
              diagnostics,
              startPosition)
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent)
    {
        return new Syntax.CompilationUnitSyntax(this, parent);
    }
}