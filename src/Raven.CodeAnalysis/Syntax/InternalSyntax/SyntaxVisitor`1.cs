


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
        VisitTriviaList(syntaxToken.LeadingTrivia);

        VisitTriviaList(syntaxToken.TrailingTrivia);

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

    public virtual TResult VisitTrivia(SyntaxTrivia trivia)
    {
        if (trivia.HasStructuredTrivia)
        {
            var structure = trivia.GetStructuredTrivia()!;
            return VisitStructuredTrivia(structure);
        }

        return default!;
    }

    private TResult VisitStructuredTrivia(SyntaxNode structure)
    {
        return structure.Accept(this);
    }

    public virtual TResult VisitSkippedTokensTrivia(SkippedTokensTrivia skippedTokens)
    {
        return DefaultVisit(skippedTokens);
    }

    public virtual TResult VisitPattern(PatternSyntax pattern)
    {
        return DefaultVisit(pattern);
    }

    public virtual TResult VisitVariableDesignation(VariableDesignationSyntax variableDesignation)
    {
        return DefaultVisit(variableDesignation);
    }
}