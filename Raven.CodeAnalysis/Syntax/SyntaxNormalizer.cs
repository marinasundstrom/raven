using System.Diagnostics.CodeAnalysis;

namespace Raven.CodeAnalysis.Syntax;

public sealed class SyntaxNormalizer : SyntaxRewriter
{
    private readonly string _indent;
    private int _indentLevel;

    public SyntaxNormalizer(int indentSize = 4)
    {
        _indent = new string(' ', indentSize);
        _indentLevel = 0;
    }

    public TSyntax Visit<TSyntax>(TSyntax syntax)
        where TSyntax : SyntaxNode
    {
        return (TSyntax)base.Visit(syntax);
    }

    public override SyntaxToken VisitToken(SyntaxToken token)
    {
        if (token.Kind == SyntaxKind.None || token is { IsMissing: true, FullWidth: 0 })
        {
            return token;
        }

        return token;
    }

    public override SyntaxNode? VisitStatement(StatementSyntax node)
    {
        var statement = base.VisitStatement(node);
        var leadingTrivia = statement!.LeadingTrivia;
        return statement.WithLeadingTrivia(FormatTrivia());
    }

    public override SyntaxNode? VisitIfStatement(IfStatementSyntax node)
    {
        // Ensure a single space after the `if` keyword.
        var ifKeyword = node.IfKeyword.WithTrailingTrivia(SyntaxFactory.Space);

        // Visit the child nodes (condition and statement).
        var condition = (ExpressionSyntax)Visit(node.Condition);
        var statement = (StatementSyntax)VisitStatement(node.Statement);

        var closeParenToken = node.CloseParenToken.WithTrailingTrivia(SyntaxFactory.Space);

        // Reconstruct the node with the updated `if` keyword.
        return node.Update(ifKeyword, node.OpenParenToken, condition, closeParenToken, statement, node.ElseClause, node.SemicolonToken);
    }

    public override SyntaxNode? VisitBlock(BlockSyntax node)
    {
        _indentLevel++;

        // Normalize open brace `{` with a trailing space.
        var openBrace = node.OpenBraceToken.WithTrailingTrivia(SyntaxFactory.CarriageReturnLineFeed);

        // Visit child statements to normalize them recursively.
        var statements = node.Statements.Select(VisitStatement).OfType<StatementSyntax>().ToList();

        // Normalize close brace `}` with leading trivia to ensure proper spacing.
        var closeBrace = node.CloseBraceToken.WithLeadingTrivia(SyntaxFactory.CarriageReturnLineFeed);

        _indentLevel--;

        // Reconstruct the block with normalized components.
        return node.Update(openBrace, SyntaxFactory.List(statements), closeBrace);
    }

    public override SyntaxNode? VisitReturnStatement(ReturnStatementSyntax node)
    {
        var returnKeyword = node.ReturnKeyword.WithTrailingTrivia(SyntaxFactory.Space);

        var expression = (ExpressionSyntax)Visit(node.Expression);

        return node.Update(returnKeyword, expression, node.SemicolonToken);
    }

    public override SyntaxNode? VisitImportDirective(ImportDirectiveSyntax node)
    {
        var importKeyword = node.ImportKeyword.WithTrailingTrivia(SyntaxFactory.Space);

        var ns = (IdentifierNameSyntax)VisitIdentifierName(node.Namespace)!;

        return node.Update(importKeyword, ns, node.SemicolonToken);
    }

    public override SyntaxNode? VisitFileScopedNamespaceDeclaration(FileScopedNamespaceDeclarationSyntax node)
    {
        var namespaceKeyword = node.NamespaceKeyword.WithTrailingTrivia(SyntaxFactory.Space);

        var name = (IdentifierNameSyntax)VisitIdentifierName(node.Name)!;

        return node.Update(namespaceKeyword, name, node.SemicolonToken, VisitList<ImportDirectiveSyntax>(node.Imports), VisitList<MemberDeclarationSyntax>(node.Members));
    }

    public override SyntaxNode? VisitBinaryExpression(BinaryExpressionSyntax node)
    {
        var operatorToken = node.OperatorToken.WithTrailingTrivia(SyntaxFactory.Space);

        var left = (ExpressionSyntax)Visit(node.LeftHandSide)
            .WithTrailingTrivia(SyntaxFactory.Space);

        var right = (ExpressionSyntax)Visit(node.LeftHandSide);

        return node.Update(left, operatorToken, right);
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

    private SyntaxTriviaList FormatTrivia()
    {
        return SyntaxFactory.TriviaList(SyntaxFactory.Whitespace(GetIndentation()));
    }

    private string GetIndentation()
    {
        return string.Concat(Enumerable.Repeat(_indent, _indentLevel));
    }
}