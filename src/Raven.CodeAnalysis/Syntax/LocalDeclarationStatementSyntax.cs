using Raven.CodeAnalysis.Syntax.InternalSyntax;

namespace Raven.CodeAnalysis.Syntax;

public partial class LocalDeclarationStatementSyntax : StatementSyntax
{
    public partial VariableDeclarationSyntax Declaration { get; }

    public partial SyntaxToken TerminationToken { get; }

    internal LocalDeclarationStatementSyntax(
        InternalSyntax.SyntaxNode greenNode,
        SyntaxNode parent = null,
        int position = 0)
    : base(greenNode, parent, position)
    {
    }

    public LocalDeclarationStatementSyntax(VariableDeclarationSyntax declaration, SyntaxToken terminationToken)
      : this(
            new InternalSyntax.LocalDeclarationStatementSyntax((InternalSyntax.VariableDeclarationSyntax)declaration.Green, terminationToken.Green))
    {

    }
}

public static partial class SyntaxFactory
{
    public static LocalDeclarationStatementSyntax LocalDeclarationStatement(VariableDeclarationSyntax declaration, SyntaxToken terminationToken)
        => new LocalDeclarationStatementSyntax(declaration, terminationToken);

    public static LocalDeclarationStatementSyntax LocalDeclarationStatement(VariableDeclarationSyntax declaration)
        => LocalDeclarationStatement(declaration, SemicolonToken);
}