


namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal abstract partial class SyntaxVisitor
{
    public virtual void Visit(SyntaxNode node)
    {
        if (node != null)
        {
            node.Accept(this);
        }
    }

    public virtual void DefaultVisit(SyntaxNode node)
    {

    }

    public virtual void VisitToken(SyntaxToken syntaxToken)
    {

    }

    public virtual void VisitSyntaxList(SyntaxList syntaxList)
    {
        foreach (var trivia in syntaxList.GetChildren())
        {
            trivia.Accept(this);
        }
    }

    public virtual void VisitTriviaList(SyntaxTriviaList triviaList)
    {
        foreach (var trivia in triviaList.GetChildren())
        {
            trivia.Accept(this);
        }
    }

    public virtual void VisitTrivia(SyntaxTrivia trivia)
    {
        VisitTriviaList(trivia.LeadingTrivia);

        VisitTriviaList(trivia.TrailingTrivia);
    }
}
