namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public partial class LocalDeclarationStatementSyntax : StatementSyntax
{
    public LocalDeclarationStatementSyntax(
        VariableDeclarationSyntax declaration,
        SyntaxToken semicolonToken)
        : base(
              SyntaxKind.LocalDeclaration,
              [
                      declaration,
                      semicolonToken
              ])
    {
    }
}