namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class CompilationUnit : SyntaxNode
{
    public CompilationUnit(
        SyntaxList imports,
        SyntaxList members,
        int startPosition = 0,
        IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(
              SyntaxKind.CompilationUnit,
              [
                      imports,
                      members
              ],
              (imports?.FullWidth ?? 0) + (members?.FullWidth ?? 0),
              diagnostics,
              startPosition)
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent)
    {
        return new CodeAnalysis.CompilationUnit(this, parent);
    }
}
