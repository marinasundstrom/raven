using System.IO;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Syntax.InternalSyntax;
using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class AssignmentRightHandSideParserTests
{
    [Theory]
    [InlineData("selected = !selected", SyntaxKind.LogicalNotExpression)]
    [InlineData("selected = left && right", SyntaxKind.LogicalAndExpression)]
    [InlineData("selected = left || right", SyntaxKind.LogicalOrExpression)]
    [InlineData("selected = left ?? right", SyntaxKind.NullCoalesceExpression)]
    [InlineData("selected = left == right", SyntaxKind.EqualsExpression)]
    public void AssignmentIncludesTheCompleteRightHandExpression(string source, SyntaxKind expected)
    {
        var parser = new ExpressionSyntaxParser(new BaseParseContext(new Lexer(new StringReader(source))));
        var assignment = Assert.IsType<AssignmentExpressionSyntax>(parser.ParseExpression().CreateRed());
        Assert.Equal(expected, assignment.Right.Kind);
        Assert.Equal(source, assignment.ToFullString());
    }

    [Fact]
    public void ChainedAssignmentRemainsRightAssociative()
    {
        var parser = new ExpressionSyntaxParser(new BaseParseContext(new Lexer(new StringReader("first = second = !value"))));
        var first = Assert.IsType<AssignmentExpressionSyntax>(parser.ParseExpression().CreateRed());
        var second = Assert.IsType<AssignmentExpressionSyntax>(first.Right);
        Assert.Equal(SyntaxKind.LogicalNotExpression, second.Right.Kind);
    }
}
