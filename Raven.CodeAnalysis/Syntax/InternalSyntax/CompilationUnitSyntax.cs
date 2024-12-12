namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class CompilationUnitSyntax : SyntaxNode
{
    public CompilationUnitSyntax(
        SyntaxList imports,
        SyntaxList members,
        SyntaxToken endOfFileToken)
        : base(
              SyntaxKind.CompilationUnit,
              [
                      imports,
                      members,
                      endOfFileToken
              ])
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent, int position)
    {
        return new Syntax.CompilationUnitSyntax(this, parent);
    }
}