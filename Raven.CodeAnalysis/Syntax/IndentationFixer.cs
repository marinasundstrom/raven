using System.Diagnostics.CodeAnalysis;

namespace Raven.CodeAnalysis.Syntax;

public sealed class IndentationFixer : SyntaxRewriter
{
    private readonly string _indent;
    private int _indentLevel;

    public IndentationFixer(int indentSize = 4)
    {
        _indent = new string(' ', indentSize);
        _indentLevel = 0;
    }

    public override SyntaxNode? DefaultVisit(SyntaxNode node)
    {
        return node;
    }

    [return: NotNullIfNotNull("node")]
    public override SyntaxNode? Visit(SyntaxNode? node)
    {
        return node?.Accept(this);
    }

    public override SyntaxToken VisitToken(SyntaxToken token)
    {
        return token;
    }

    public override SyntaxTrivia VisitTrivia(SyntaxTrivia trivia)
    {
        return trivia;
    }

    public override BlockSyntax VisitBlock(BlockSyntax node)
    {
        _indentLevel++;

        // Fix indentation for each statement in the block
        var statements = node.Statements.Select(RewriteStatement);
        var openBrace = node.OpenBraceToken.WithLeadingTrivia(FormatTrivia());
        var closeBrace = node.CloseBraceToken.WithLeadingTrivia(FormatTrivia());

        _indentLevel--;

        return node.WithStatements(SyntaxFactory.List(statements))
            .WithOpenBraceToken(openBrace)
            .WithCloseBraceToken(closeBrace);
    }

    private StatementSyntax RewriteStatement(StatementSyntax statement)
    {
        return statement.WithLeadingTrivia(FormatTrivia());
    }

    private SyntaxTriviaList FormatTrivia()
    {
        return SyntaxFactory.TriviaList(SyntaxFactory.Whitespace(GetIndentation()));
    }

    private string GetIndentation()
    {
        return string.Concat(Enumerable.Repeat(_indent, _indentLevel));
    }

    public override SyntaxNode VisitMethodDeclaration(MethodDeclarationSyntax node)
    {
        // Ensure newlines between methods
        var newNode = base.VisitMethodDeclaration(node);
        return newNode.WithLeadingTrivia(SyntaxFactory.TriviaList(
            SyntaxFactory.CarriageReturnLineFeed,
            SyntaxFactory.CarriageReturnLineFeed
        ));
    }
}