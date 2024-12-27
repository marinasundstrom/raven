namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class CompilationUnitSyntax : SyntaxNode
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
}