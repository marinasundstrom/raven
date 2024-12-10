namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class CompilationUnitSyntax : SyntaxNode
{
    public CompilationUnitSyntax(
        SyntaxList imports,
        SyntaxList members,
        SyntaxToken endOfFileToken,
        IEnumerable<DiagnosticInfo> diagnostics = null)
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

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent, int position)
    {
        return new Syntax.CompilationUnitSyntax(this, parent);
    }
}