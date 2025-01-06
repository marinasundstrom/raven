


namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal abstract partial class SyntaxVisitor<TResult>
{
    public virtual TResult? Visit(SyntaxNode? node)
    {
        if (node != null)
        {
            return node.Accept(this);
        }

        // should not come here too often so we will put this at the end of the method.
        return default;
    }

    public virtual TResult DefaultVisit(SyntaxNode node)
    {
        return default!;
    }

    public virtual TResult VisitToken(SyntaxToken syntaxToken)
    {
        return default!;
    }

    public virtual TResult VisitSyntaxList(SyntaxList syntaxList)
    {
        foreach (var trivia in syntaxList.GetChildren())
        {
            trivia.Accept(this);
        }

        return default!;
    }

    public virtual TResult VisitTriviaList(SyntaxTriviaList triviaList)
    {
        foreach (var trivia in triviaList.GetChildren())
        {
            trivia.Accept(this);
        }

        return default!;
    }

    internal TResult VisitTrivia(SyntaxTrivia trivia)
    {
        VisitTriviaList(trivia.LeadingTrivia);

        VisitTriviaList(trivia.TrailingTrivia);

        return default!;
    }
}