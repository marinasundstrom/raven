using static Raven.CodeAnalysis.Syntax.SyntaxFactory;

namespace Raven.CodeAnalysis.Syntax.Tests;

public sealed class SyntaxFactoryStatementTests
{
    [Fact]
    public void ExpressionStatement_DefaultsTerminatorToNewLine()
    {
        var statement = ExpressionStatement(IdentifierName("value"));

        statement.TerminatorToken.Kind.ShouldBe(SyntaxKind.NewLineToken);
    }

    [Fact]
    public void AssignmentStatement_DefaultsTerminatorToNewLine()
    {
        var statement = AssignmentStatement(
            SyntaxKind.SimpleAssignmentStatement,
            IdentifierName("value"),
            EqualsToken,
            LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(1)));

        statement.TerminatorToken.Kind.ShouldBe(SyntaxKind.NewLineToken);
    }

    [Fact]
    public void ReturnStatement_DefaultsTerminatorToNewLine()
    {
        var statement = ReturnStatement(LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(1)));

        statement.TerminatorToken.Kind.ShouldBe(SyntaxKind.NewLineToken);
    }

    [Fact]
    public void ReturnStatement_WithoutExpression_DefaultsTerminatorToNewLine()
    {
        var statement = ReturnStatement();

        statement.TerminatorToken.Kind.ShouldBe(SyntaxKind.NewLineToken);
        statement.Expression.ShouldBeNull();
    }

    [Fact]
    public void IfStatement_DefaultsTerminatorToNewLine()
    {
        var statement = IfStatement(
            IdentifierName("condition"),
            BlockStatement(List<StatementSyntax>()));

        statement.TerminatorToken.Kind.ShouldBe(SyntaxKind.NewLineToken);
    }

    [Fact]
    public void LocalDeclarationStatement_DefaultsTerminatorToNewLine()
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

        statement.TerminatorToken.Kind.ShouldBe(SyntaxKind.NewLineToken);
    }
}
