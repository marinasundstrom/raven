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
        var statement = base.VisitStatement(node)!;

        if (node is BlockSyntax && node?.Parent is IfStatementSyntax)
        {
            return statement;
        }

        var leadingTrivia = statement.LeadingTrivia;
        return statement.WithLeadingTrivia(FormatTrivia());
    }

    public override SyntaxNode? VisitIfStatement(IfStatementSyntax node)
    {
        // Ensure a single space after the `if` keyword.
        var ifKeyword = node.IfKeyword.WithTrailingTrivia(SyntaxFactory.Space);

        // Visit the child nodes (condition and statement).
        var condition = (ExpressionSyntax)VisitExpression(node.Condition)!;
        var statement = (StatementSyntax)VisitStatement(node.Statement)!;

        var closeParenToken = node.CloseParenToken.WithTrailingTrivia(SyntaxFactory.Space);

        // Reconstruct the node with the updated `if` keyword.
        return node.Update(ifKeyword, node.OpenParenToken, condition, closeParenToken, statement,
            node.ElseClause is null ? null : (ElseClauseSyntax?)VisitElseClause(node.ElseClause!), node.SemicolonToken);
    }

    public override SyntaxNode? VisitElseClause(ElseClauseSyntax node)
    {
        var elseKeyword = node.ElseKeyword
            .WithLeadingTrivia(SyntaxFactory.Space);

        StatementSyntax statement = null!;

        if (node.Statement is not BlockSyntax)
        {
            IncreaseIdent();

            elseKeyword = elseKeyword
                .WithTrailingTrivia(SyntaxFactory.CarriageReturnLineFeed);

            statement = (StatementSyntax)VisitStatement(node.Statement)!;

            DecreaseIndent();
        }
        else
        {
            statement = (StatementSyntax)VisitStatement(node.Statement)!;
        }

        return node.Update(elseKeyword, statement);
    }

    public override SyntaxNode? VisitBlock(BlockSyntax node)
    {
        // Normalize open brace `{` with a trailing space.
        var openBrace = node.OpenBraceToken.WithTrailingTrivia(SyntaxFactory.CarriageReturnLineFeed);

        IncreaseIdent();

        // Visit child statements to normalize them recursively.
        var statements = node.Statements.Select(VisitStatement).OfType<StatementSyntax>().ToList();

        DecreaseIndent();

        // Normalize close brace `}` with leading trivia to ensure proper spacing.
        var closeBrace = node.CloseBraceToken
            .WithLeadingTrivia(SyntaxFactory.CarriageReturnLineFeed, IndentationTrivia());

        // Reconstruct the block with normalized components.
        return node.Update(openBrace, SyntaxFactory.List(statements), closeBrace);
    }

    private void IncreaseIdent()
    {
        _indentLevel++;
    }

    private void DecreaseIndent()
    {
        _indentLevel--;
    }

    public override SyntaxNode? VisitVariableDeclaration(VariableDeclarationSyntax node)
    {

        List<SyntaxNodeOrToken> newList = [];

        foreach (var item in node.Declarators.GetWithSeparators())
        {
            if (item.AsNode(out var node2))
            {
                newList.Add(
                    new SyntaxNodeOrToken(node2.Accept(this)!));
            }
            else if (item.AsToken(out var token))
            {
                newList.Add(token.WithTrailingTrivia(SyntaxFactory.Space));
            }
        }

        var declarators = SyntaxFactory.SeparatedList<VariableDeclaratorSyntax>(newList.ToArray());

        return node.Update(node.LetKeyword.WithTrailingTrivia(SyntaxFactory.Space), declarators!);
    }

    public override SyntaxNode? VisitVariableDeclarator(VariableDeclaratorSyntax node)
    {
        return node.Update(node.Name
            .WithTrailingTrivia(SyntaxFactory.Space), (TypeAnnotationSyntax)VisitTypeAnnotation(node.TypeAnnotation)!, (EqualsValueClauseSyntax?)VisitEqualsValueClause(node.Initializer!)!);
    }

    public override SyntaxNode? VisitEqualsValueClause(EqualsValueClauseSyntax node)
    {
        return node.Update(node.EqualsToken
            .WithLeadingTrivia(SyntaxFactory.Space)
            .WithTrailingTrivia(SyntaxFactory.Space),
            (ExpressionSyntax)VisitExpression(node.Value)!);
    }

    public override SyntaxNode? VisitReturnStatement(ReturnStatementSyntax node)
    {
        var returnKeyword = node.ReturnKeyword.WithTrailingTrivia(SyntaxFactory.Space);

        var expression = Visit(node.Expression);

        return node.Update(returnKeyword, expression, node.SemicolonToken);
    }

    public override SyntaxNode? VisitImportDirective(ImportDirectiveSyntax node)
    {
        var importKeyword = node.ImportKeyword.WithTrailingTrivia(SyntaxFactory.Space);

        var ns = (IdentifierNameSyntax)VisitName(node.Namespace)!;

        var semicolonToken = node.SemicolonToken
            .WithTrailingTrivia(
                SyntaxFactory.CarriageReturnLineFeed,
                SyntaxFactory.CarriageReturnLineFeed);

        return node.Update(importKeyword, ns, semicolonToken);
    }

    public override SyntaxNode? VisitFileScopedNamespaceDeclaration(FileScopedNamespaceDeclarationSyntax node)
    {
        var namespaceKeyword = node.NamespaceKeyword.WithTrailingTrivia(SyntaxFactory.Space);

        var name = (IdentifierNameSyntax)VisitName(node.Name)!;

        var semicolonToken = node.SemicolonToken
            .WithTrailingTrivia(
                SyntaxFactory.CarriageReturnLineFeed,
                SyntaxFactory.CarriageReturnLineFeed);

        return node.Update(namespaceKeyword, name, semicolonToken, VisitList(node.Imports)!, VisitList(node.Members)!);
    }

    public override SyntaxNode? VisitBinaryExpression(BinaryExpressionSyntax node)
    {
        var operatorToken = node.OperatorToken.WithTrailingTrivia(SyntaxFactory.Space);

        var left = Visit(node.LeftHandSide)
            .WithTrailingTrivia(SyntaxFactory.Space);

        var right = Visit(node.RightHandSide);

        return node.Update(node.Kind, left, operatorToken, right);
    }

    public override SyntaxNode VisitMethodDeclaration(MethodDeclarationSyntax node)
    {
        var returnType = (TypeSyntax)VisitType(node.ReturnType)!
           .WithTrailingTrivia(SyntaxFactory.Space);

        var name = (IdentifierNameSyntax)VisitIdentifierName(node.Name)!
            .WithTrailingTrivia(SyntaxFactory.Space);

        var parameterList = (ParameterListSyntax)VisitParameterList(node.ParameterList)!
            .WithTrailingTrivia(SyntaxFactory.Space);

        return node.Update(returnType, name, parameterList, (BlockSyntax?)VisitBlock(node.Body))
            .WithLeadingTrivia(SyntaxFactory.TriviaList(
                SyntaxFactory.CarriageReturnLineFeed,
                SyntaxFactory.CarriageReturnLineFeed
        ));
    }

    public override SyntaxNode? VisitParameterList(ParameterListSyntax node)
    {
        List<SyntaxNodeOrToken> newList = [];

        foreach (var item in node.Parameters.GetWithSeparators())
        {
            if (item.AsNode(out var node2))
            {
                newList.Add(
                    new SyntaxNodeOrToken(node2.Accept(this)!));
            }
            else if (item.AsToken(out var token))
            {
                newList.Add(token.WithTrailingTrivia(SyntaxFactory.Space));
            }
        }

        var parameters = SyntaxFactory.SeparatedList<ParameterSyntax>(newList.ToArray());

        return node.Update(node.OpenParenToken, parameters!, node.CloseParenToken);
    }

    public override SyntaxNode? VisitParameter(ParameterSyntax node)
    {
        var name = node.Name.WithTrailingTrivia(SyntaxFactory.Space);

        return node.Update(name,
            node.TypeAnnotation is not null ? (TypeAnnotationSyntax?)VisitTypeAnnotation(node.TypeAnnotation) : null);
    }

    public override SyntaxNode? VisitTypeAnnotation(TypeAnnotationSyntax node)
    {
        var colonToken = node.ColonToken.WithTrailingTrivia(SyntaxFactory.Space);

        return node.Update(colonToken, (TypeSyntax)VisitType(node.Type)!);
    }

    private SyntaxTriviaList FormatTrivia()
    {
        return SyntaxFactory.TriviaList(IndentationTrivia());
    }

    private SyntaxTrivia IndentationTrivia()
    {
        return SyntaxFactory.Whitespace(GetIndentation());
    }

    private string GetIndentation()
    {
        return string.Concat(Enumerable.Repeat(_indent, _indentLevel));
    }
}