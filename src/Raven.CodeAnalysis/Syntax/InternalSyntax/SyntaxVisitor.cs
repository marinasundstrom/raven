



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
        VisitTriviaList(syntaxToken.LeadingTrivia);

        VisitTriviaList(syntaxToken.TrailingTrivia);
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
        if (trivia.HasStructuredTrivia)
        {
            var structure = trivia.GetStructuredTrivia()!;
            VisitStructuredTrivia(structure);
        }
    }

    private void VisitStructuredTrivia(SyntaxNode structure)
    {
        structure.Accept(this);
    }

    public virtual void VisitSkippedTokensTrivia(SkippedTokensTrivia skippedTokens)
    {
        DefaultVisit(skippedTokens);
    }

    public virtual void VisitPattern(PatternSyntax pattern)
    {
        DefaultVisit(pattern);
    }

    public virtual void VisitVariableDesignation(VariableDesignationSyntax variableDesignation)
    {
        DefaultVisit(variableDesignation);
    }

    public virtual void VisitExpressionOrPattern(ExpressionOrPatternSyntax expressionOrPattern)
    {
        DefaultVisit(expressionOrPattern);
    }
}