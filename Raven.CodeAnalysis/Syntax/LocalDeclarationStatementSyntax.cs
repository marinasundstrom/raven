using Raven.CodeAnalysis.Syntax.InternalSyntax;

namespace Raven.CodeAnalysis.Syntax;

public partial class LocalDeclarationStatementSyntax : StatementSyntax
{
    public partial VariableDeclarationSyntax Declaration { get; }

    public partial SyntaxToken SemicolonToken { get; }

    public LocalDeclarationStatementSyntax(
        InternalSyntax.SyntaxNode greenNode,
        SyntaxNode parent = null,
        int position = 0)
    : base(greenNode, parent, position)
    {
    }

    public LocalDeclarationStatementSyntax(VariableDeclarationSyntax declaration, SyntaxToken semicolonToken)
      : this(
            new InternalSyntax.LocalDeclarationStatementSyntax((InternalSyntax.VariableDeclarationSyntax)declaration.Green, semicolonToken.Green))
    {

    }
}

public static partial class SyntaxFactory
{
    public static LocalDeclarationStatementSyntax LocalDeclarationStatement(VariableDeclarationSyntax declaration, SyntaxToken semicolonToken)
        => new LocalDeclarationStatementSyntax(declaration, semicolonToken);
    
    public static LocalDeclarationStatementSyntax LocalDeclarationStatement(VariableDeclarationSyntax declaration)
        => LocalDeclarationStatement(declaration, SemicolonToken);
}