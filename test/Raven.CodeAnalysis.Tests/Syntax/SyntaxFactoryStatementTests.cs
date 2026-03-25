using static Raven.CodeAnalysis.Syntax.SyntaxFactory;

namespace Raven.CodeAnalysis.Syntax.Tests;

public sealed class SyntaxFactoryStatementTests
{
    [Fact]
    public void ExpressionStatement_DefaultsTerminatorToNone()
    {
        var statement = ExpressionStatement(IdentifierName("value"));

        statement.TerminatorToken.Kind.ShouldBe(SyntaxKind.None);
    }

    [Fact]
    public void AssignmentStatement_DefaultsTerminatorToNone()
    {
        var statement = AssignmentStatement(
            SyntaxKind.SimpleAssignmentStatement,
            IdentifierName("value"),
            EqualsToken,
            LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(1)));

        statement.TerminatorToken.Kind.ShouldBe(SyntaxKind.None);
    }

    [Fact]
    public void ReturnStatement_DefaultsTerminatorToNone()
    {
        var statement = ReturnStatement(LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(1)));

        statement.TerminatorToken.Kind.ShouldBe(SyntaxKind.None);
    }

    [Fact]
    public void ReturnStatement_WithoutExpression_DefaultsTerminatorToNone()
    {
        var statement = ReturnStatement();

        statement.TerminatorToken.Kind.ShouldBe(SyntaxKind.None);
        statement.Expression.ShouldBeNull();
    }

    [Fact]
    public void IfStatement_DefaultsTerminatorToNone()
    {
        var statement = IfStatement(
            IdentifierName("condition"),
            BlockStatement(List<StatementSyntax>()));

        statement.TerminatorToken.Kind.ShouldBe(SyntaxKind.None);
    }

    [Fact]
    public void IfPatternStatement_DefaultsTerminatorToNone()
    {
        var statement = IfPatternStatement(
            ValKeyword,
            DeclarationPattern(IdentifierName("name"), SingleVariableDesignation(Token(SyntaxKind.None), Identifier("name"))),
            IdentifierName("value"),
            BlockStatement(List<StatementSyntax>()));

        statement.TerminatorToken.Kind.ShouldBe(SyntaxKind.None);
    }

    [Fact]
    public void LocalDeclarationStatement_DefaultsTerminatorToNone()
    {
        var declaration = VariableDeclaration(
            VarKeyword,
            SeparatedList<VariableDeclaratorSyntax>(
            [
                new SyntaxNodeOrToken(
                    VariableDeclarator(
                        Identifier("value"),
                        null,
                        EqualsValueClause(LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(1)))))
            ]));

        var statement = LocalDeclarationStatement(declaration);

        statement.TerminatorToken.Kind.ShouldBe(SyntaxKind.None);
    }
}
