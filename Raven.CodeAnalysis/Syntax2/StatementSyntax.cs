namespace Raven.CodeAnalysis.Syntax;

public abstract class StatementSyntax : SyntaxNode
{
    public SyntaxToken SemicolonToken { get; set; }
}