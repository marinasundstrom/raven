using System.Diagnostics.CodeAnalysis;

namespace Raven.CodeAnalysis.Syntax;

public abstract class SyntaxRewriter : SyntaxVisitor<SyntaxNode?>
{
    private int _recursionDepth;

    [return: NotNullIfNotNull(nameof(node))]
    public override SyntaxNode? Visit(SyntaxNode? node)
    {
        if (node is not null)
        {
            _recursionDepth++;

            var result = node.Accept(this);

            _recursionDepth--;

            return result;
        }

        return null;
    }

    public override SyntaxNode? DefaultVisit(SyntaxNode node)
    {
        return node;
    }

    public virtual SyntaxToken VisitToken(SyntaxToken token)
    {
        return default!;
    }

    public virtual SyntaxTrivia VisitTrivia(SyntaxTrivia trivia)
    {
        return default!;
    }

    public override SyntaxNode? VisitIfStatement(IfStatementSyntax node)
    {
        // TODO: Generate with source generator
        
        return node.Update(
            VisitToken(node.IfKeyword),
            VisitToken(node.OpenParenToken),
            (ExpressionSyntax)Visit(node.Condition),
            VisitToken(node.CloseParenToken), 
            (StatementSyntax)Visit(node.Statement), 
            (ElseClauseSyntax?)(node.ElseClause is not null ? Visit(node.ElseClause) : null));
    }
}