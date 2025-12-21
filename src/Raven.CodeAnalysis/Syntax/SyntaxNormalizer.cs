using System.Diagnostics.CodeAnalysis;
using System.Linq;

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

    public override SyntaxNode? VisitStatement(StatementSyntax? node)
    {
        if (node is null)
            return null;

        var statement = base.VisitStatement(node)!;

        if (node is BlockStatementSyntax && node.Parent is IfStatementSyntax)
        {
            return statement;
        }

        var leadingTrivia = statement.GetLeadingTrivia();
        return statement.WithLeadingTrivia(FormatTrivia());
    }

    public override SyntaxNode? VisitIfExpression(IfExpressionSyntax node)
    {
        // Ensure a single space after the `if` keyword.
        var ifKeyword = node.IfKeyword.WithTrailingTrivia(SyntaxFactory.Space);

        // Visit the child nodes (condition and statement).
        var condition = (ExpressionSyntax)VisitExpression(node.Condition)!;
        var statement = (ExpressionSyntax)VisitExpression(node.Expression)!;

        // Reconstruct the node with the updated `if` keyword.
        return node.Update(ifKeyword, condition, statement,
            node.ElseClause is null ? null : (ElseClauseSyntax?)VisitElseClause(node.ElseClause!));
    }

    public override SyntaxNode? VisitElseClause(ElseClauseSyntax node)
    {
        var elseKeyword = node.ElseKeyword
            .WithLeadingTrivia(SyntaxFactory.Space);

        ExpressionSyntax expression = null!;

        if (node.Expression is not BlockSyntax)
        {
            IncreaseIdent();

            elseKeyword = elseKeyword
                .WithTrailingTrivia(SyntaxFactory.CarriageReturnLineFeed);

            expression = (ExpressionSyntax)VisitExpression(node.Expression)!;

            DecreaseIndent();
        }
        else
        {
            expression = (ExpressionSyntax)VisitExpression(node.Expression)!;
        }

        return node.Update(elseKeyword, expression);
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

    public override SyntaxNode? VisitBlockStatement(BlockStatementSyntax node)
    {
        var openBrace = node.OpenBraceToken.WithTrailingTrivia(SyntaxFactory.CarriageReturnLineFeed);

        IncreaseIdent();

        var statements = node.Statements.Select(VisitStatement).OfType<StatementSyntax>().ToList();

        DecreaseIndent();

        var closeBrace = node.CloseBraceToken
            .WithLeadingTrivia(SyntaxFactory.CarriageReturnLineFeed, IndentationTrivia());

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
            if (item.TryGetNode(out var node2))
            {
                newList.Add(
                    new SyntaxNodeOrToken(node2.Accept(this)!));
            }
            else if (item.TryGetToken(out var token))
            {
                newList.Add(token.WithTrailingTrivia(SyntaxFactory.Space));
            }
        }

        var declarators = SyntaxFactory.SeparatedList<VariableDeclaratorSyntax>(newList.ToArray());

        return node.Update(node.BindingKeyword.WithTrailingTrivia(SyntaxFactory.Space), declarators!);
    }

    public override SyntaxNode? VisitVariableDeclarator(VariableDeclaratorSyntax node)
    {
        return node.Update(node.Identifier
            .WithTrailingTrivia(SyntaxFactory.Space), (TypeAnnotationClauseSyntax)VisitTypeAnnotationClause(node.TypeAnnotation)!, (EqualsValueClauseSyntax?)VisitEqualsValueClause(node.Initializer!)!);
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

        return node.Update(returnKeyword, expression, node.TerminatorToken);
    }

    public override SyntaxNode? VisitImportDirective(ImportDirectiveSyntax node)
    {
        var importKeyword = node.ImportKeyword.WithTrailingTrivia(SyntaxFactory.Space);
        var nameSyntax = (NameSyntax)VisitName(node.Name)!;
        var terminatorToken = node.TerminatorToken
            .WithTrailingTrivia(
                SyntaxFactory.CarriageReturnLineFeed,
                SyntaxFactory.CarriageReturnLineFeed);

        return node.Update(importKeyword, nameSyntax, terminatorToken);
    }

    public override SyntaxNode? VisitAliasDirective(AliasDirectiveSyntax node)
    {
        var aliasKeyword = node.AliasKeyword.WithTrailingTrivia(SyntaxFactory.Space);
        var identifier = node.Identifier.WithTrailingTrivia(SyntaxFactory.Space);
        var equalsToken = node.EqualsToken.WithTrailingTrivia(SyntaxFactory.Space);
        var target = (TypeSyntax)VisitType(node.Target)!;
        var terminatorToken = node.TerminatorToken
            .WithTrailingTrivia(
                SyntaxFactory.CarriageReturnLineFeed,
                SyntaxFactory.CarriageReturnLineFeed);

        return node.Update(aliasKeyword, identifier, equalsToken, target, terminatorToken);
    }

    public override SyntaxNode? VisitFileScopedNamespaceDeclaration(FileScopedNamespaceDeclarationSyntax node)
    {
        var namespaceKeyword = node.NamespaceKeyword.WithTrailingTrivia(SyntaxFactory.Space);

        var name = (IdentifierNameSyntax)VisitName(node.Name)!;

        var terminatorToken = node.TerminatorToken
            .WithTrailingTrivia(
                SyntaxFactory.CarriageReturnLineFeed,
                SyntaxFactory.CarriageReturnLineFeed);

        return node.Update(
            node.AttributeLists,
            node.Modifiers,
            namespaceKeyword,
            name,
            terminatorToken,
            VisitList(node.Imports)!,
            VisitList(node.Aliases)!,
            VisitList(node.Members)!);
    }

    public override SyntaxNode? VisitBinaryExpression(BinaryExpressionSyntax node)
    {
        var operatorToken = node.OperatorToken.WithTrailingTrivia(SyntaxFactory.Space);

        var left = Visit(node.Left)
            .WithTrailingTrivia(SyntaxFactory.Space);

        var right = Visit(node.Right);

        return node.Update(node.Kind, left, operatorToken, right);
    }

    public override SyntaxNode VisitMethodDeclaration(MethodDeclarationSyntax node)
    {
        ExplicitInterfaceSpecifierSyntax? explicitInterfaceSpecifier = null;
        if (node.ExplicitInterfaceSpecifier is not null)
        {
            var name = (TypeSyntax)Visit(node.ExplicitInterfaceSpecifier.Name)!;
            var dotToken = VisitToken(node.ExplicitInterfaceSpecifier.DotToken)!;
            var identifierToken = VisitToken(node.ExplicitInterfaceSpecifier.Identifier)!;
            explicitInterfaceSpecifier = node.ExplicitInterfaceSpecifier.Update(name, dotToken, identifierToken);
        }

        var identifier = VisitToken(node.Identifier)!;

        TypeParameterListSyntax? typeParameterList = null;
        if (node.TypeParameterList is not null)
        {
            identifier = identifier.WithTrailingTrivia(SyntaxFactory.TriviaList());
            typeParameterList = (TypeParameterListSyntax)Visit(node.TypeParameterList)!;
            typeParameterList = typeParameterList.WithTrailingTrivia(SyntaxFactory.Space);
        }
        else
        {
            identifier = identifier.WithTrailingTrivia(SyntaxFactory.Space);
        }

        var parameterList = (ParameterListSyntax)VisitParameterList(node.ParameterList)!
            .WithTrailingTrivia(SyntaxFactory.Space);

        ArrowTypeClauseSyntax? returnType = null;
        if (node.ReturnType is not null)
            returnType = (ArrowTypeClauseSyntax)VisitArrowTypeClause(node.ReturnType)!
                .WithTrailingTrivia(SyntaxFactory.Space);

        return node.Update(
                node.AttributeLists,
                node.Modifiers,
                explicitInterfaceSpecifier,
                identifier,
                typeParameterList,
                parameterList,
                returnType,
                (BlockStatementSyntax?)VisitBlockStatement(node.Body),
                null,
                node.TerminatorToken)
            .WithLeadingTrivia(SyntaxFactory.TriviaList(
                SyntaxFactory.CarriageReturnLineFeed,
                SyntaxFactory.CarriageReturnLineFeed
        ));
    }

    public override SyntaxNode VisitOperatorDeclaration(OperatorDeclarationSyntax node)
    {
        var operatorKeyword = VisitToken(node.OperatorKeyword)!;
        operatorKeyword = operatorKeyword.WithTrailingTrivia(SyntaxFactory.Space);

        var operatorToken = VisitToken(node.OperatorToken)!;
        operatorToken = operatorToken.WithTrailingTrivia(SyntaxFactory.Space);

        var parameterList = (ParameterListSyntax)VisitParameterList(node.ParameterList)!
            .WithTrailingTrivia(SyntaxFactory.Space);

        ArrowTypeClauseSyntax? returnType = null;
        if (node.ReturnType is not null)
            returnType = (ArrowTypeClauseSyntax)VisitArrowTypeClause(node.ReturnType)!
                .WithTrailingTrivia(SyntaxFactory.Space);

        return node.Update(
                node.AttributeLists,
                node.Modifiers,
                operatorKeyword,
                operatorToken,
                parameterList,
                returnType,
                (BlockStatementSyntax?)VisitBlockStatement(node.Body),
                null,
                node.TerminatorToken)
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
            if (item.TryGetNode(out var node2))
            {
                newList.Add(
                    new SyntaxNodeOrToken(node2.Accept(this)!));
            }
            else if (item.TryGetToken(out var token))
            {
                newList.Add(token.WithTrailingTrivia(SyntaxFactory.Space));
            }
        }

        var parameters = SyntaxFactory.SeparatedList<ParameterSyntax>(newList.ToArray());

        return node.Update(node.OpenParenToken, parameters!, node.CloseParenToken);
    }

    public override SyntaxNode? VisitParameter(ParameterSyntax node)
    {
        var identifier = node.Identifier;

        if (node.TypeAnnotation is not null || node.DefaultValue is not null)
        {
            identifier = identifier.WithTrailingTrivia(SyntaxFactory.Space);
        }

        var typeAnnotation = node.TypeAnnotation is not null
            ? (TypeAnnotationClauseSyntax?)VisitTypeAnnotationClause(node.TypeAnnotation)
            : null;

        var defaultValue = node.DefaultValue is not null
            ? (EqualsValueClauseSyntax?)VisitEqualsValueClause(node.DefaultValue)
            : null;

        return node.Update(node.AttributeLists, node.RefKindKeyword, node.BindingKeyword, identifier, typeAnnotation, defaultValue);
    }

    public override SyntaxNode? VisitTypeAnnotationClause(TypeAnnotationClauseSyntax node)
    {
        if (node is null)
            return null;

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
