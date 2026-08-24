using System.IO;
using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class ControlFlowExpressionParserTests
{
    [Theory]
    [InlineData("break", typeof(BreakExpressionSyntax))]
    [InlineData("continue", typeof(ContinueExpressionSyntax))]
    [InlineData("yield value", typeof(YieldExpressionSyntax))]
    [InlineData("return", typeof(ReturnExpressionSyntax))]
    public void ControlFlowForm_ParsesAsExpression(string source, Type expectedType)
    {
        var parser = new ExpressionSyntaxParser(new BaseParseContext(new Lexer(new StringReader(source))));

        var expression = parser.ParseExpression().CreateRed();

        Assert.IsType(expectedType, expression);
    }

    [Theory]
    [InlineData("yield return value", "RAV1064")]
    [InlineData("yield break", "RAV1065")]
    public void RemovedYieldForm_ReportsMigrationDiagnostic(string source, string diagnosticId)
    {
        var context = new BaseParseContext(new Lexer(new StringReader(source)));
        var expression = new ExpressionSyntaxParser(context).ParseExpression().CreateRed();

        Assert.IsType<YieldExpressionSyntax>(expression);
        Assert.Contains(context.Diagnostics, diagnostic => diagnostic.Descriptor.Id == diagnosticId);
    }

    [Theory]
    [InlineData("break outer", typeof(BreakExpressionSyntax))]
    [InlineData("continue outer", typeof(ContinueExpressionSyntax))]
    public void LabeledLoopTransfer_ParsesAsExpression(string source, Type expectedType)
    {
        var parser = new ExpressionSyntaxParser(new BaseParseContext(new Lexer(new StringReader(source))));

        var expression = Assert.IsAssignableFrom<ExpressionSyntax>(parser.ParseExpression().CreateRed());

        Assert.IsType(expectedType, expression);
        var identifier = expression switch
        {
            BreakExpressionSyntax @break => @break.Identifier,
            ContinueExpressionSyntax @continue => @continue.Identifier,
            _ => throw new Xunit.Sdk.XunitException("Unexpected expression kind."),
        };
        Assert.Equal("outer", identifier.ValueText);
    }

    [Fact]
    public void MatchArms_ParseDirectControlFlowExpressions()
    {
        var tree = SyntaxTree.ParseText("""
func Iterator(value: int) {
    match value {
        0 => break outer
        1 => continue outer
        2 => yield value
        _ => return
    }
}
""");
        var arms = tree.GetRoot().DescendantNodes().OfType<MatchArmSyntax>().ToArray();

        Assert.IsType<BreakExpressionSyntax>(arms[0].Expression);
        Assert.IsType<ContinueExpressionSyntax>(arms[1].Expression);
        Assert.IsType<YieldExpressionSyntax>(arms[2].Expression);
        Assert.IsType<ReturnExpressionSyntax>(arms[3].Expression);
    }

    [Fact]
    public void ExpressionBlock_ProjectsControlFlowFormsAsExpressions()
    {
        var parser = new ExpressionSyntaxParser(
            new BaseParseContext(new Lexer(new StringReader("{ break outer }"))));

        var block = Assert.IsType<BlockSyntax>(parser.ParseExpression().CreateRed());
        var statement = Assert.IsType<ExpressionStatementSyntax>(Assert.Single(block.Statements));
        var expression = Assert.IsType<BreakExpressionSyntax>(statement.Expression);

        Assert.Equal("outer", expression.Identifier.ValueText);
    }
}
