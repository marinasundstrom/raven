using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

[Trait("Target", "Shared")]
public sealed class StatementParserContractTests
{
    [Theory]
    [InlineData(";", SyntaxKind.EmptyStatement)]
    [InlineData("return 1;", SyntaxKind.ReturnStatement)]
    [InlineData("return;", SyntaxKind.ReturnStatement)]
    [InlineData("throw failure;", SyntaxKind.ThrowStatement)]
    [InlineData("yield 1;", SyntaxKind.YieldStatement)]
    [InlineData("yield from values;", SyntaxKind.YieldStatement)]
    [InlineData("break;", SyntaxKind.BreakStatement)]
    [InlineData("break done;", SyntaxKind.BreakStatement)]
    [InlineData("continue;", SyntaxKind.ContinueStatement)]
    [InlineData("goto done;", SyntaxKind.GotoStatement)]
    [InlineData("done: return 1;", SyntaxKind.LabeledStatement)]
    [InlineData("{}", SyntaxKind.BlockStatement)]
    [InlineData("{ val x = 1; return x; }", SyntaxKind.BlockStatement)]
    [InlineData("while (ready)\n    value = 1", SyntaxKind.WhileStatement)]
    [InlineData("if (bool)ready { value = 1 }", SyntaxKind.IfStatement)]
    [InlineData("if (ready)\n    value = 1", SyntaxKind.IfStatement)]
    [InlineData("if ready { return 1 } else { return 0 }", SyntaxKind.IfStatement)]
    [InlineData("if val value = source { return value }", SyntaxKind.IfPatternStatement)]
    [InlineData("while ready { break }", SyntaxKind.WhileStatement)]
    [InlineData("while val value = source { break }", SyntaxKind.WhilePatternStatement)]
    [InlineData("loop { break }", SyntaxKind.LoopStatement)]
    [InlineData("lock gate {}", SyntaxKind.LockStatement)]
    [InlineData("for val value in values {}", SyntaxKind.ForStatement)]
    [InlineData("for val value: int in values {}", SyntaxKind.ForStatement)]
    [InlineData("for val value in 1..10 by 2 {}", SyntaxKind.ForStatement)]
    [InlineData("await for val value in values {}", SyntaxKind.ForStatement)]
    [InlineData("try {} catch {}", SyntaxKind.TryStatement)]
    [InlineData("try {} finally {}", SyntaxKind.TryStatement)]
    [InlineData("try {} catch (Exception error) when ready {} finally {}", SyntaxKind.TryStatement)]
    [InlineData("unsafe {}", SyntaxKind.UnsafeStatement)]
    [InlineData("checked {}", SyntaxKind.CheckedStatement)]
    [InlineData("use resource = Open();", SyntaxKind.UseDeclarationStatement)]
    [InlineData("val value: int = 1;", SyntaxKind.LocalDeclarationStatement)]
    [InlineData("value += 1;", SyntaxKind.AddAssignmentStatement)]
    [InlineData("Call();", SyntaxKind.ExpressionStatement)]
    [InlineData("func Value() -> int => 1", SyntaxKind.FunctionStatement)]
    [InlineData("func Identity<T>(value: T) -> T => value", SyntaxKind.FunctionStatement)]
    [InlineData("class Local {}", SyntaxKind.TypeDeclarationStatement)]
    [InlineData("val (first, second) = pair;", SyntaxKind.PatternDeclarationAssignmentStatement)]
    [InlineData("match value { 1 => Call(); _ => Other(); }", SyntaxKind.MatchStatement)]
    public void StatementPreservesSourceAndShape(string source, SyntaxKind kind)
    {
        var parser = Parser(source);
        var result = parser.ParseStatement().CreateRed();
        Assert.Equal(kind, result.Kind);
        Assert.Equal(source, result.ToFullString());
        Assert.Empty(parser.Diagnostics);
        Assert.Equal(SyntaxKind.EndOfFileToken, parser.PeekToken().Kind);
    }

    [Theory]
    [InlineData("()", 0)]
    [InlineData("(value: int)", 1)]
    [InlineData("(value: int = 1, other: string = \"text\")", 2)]
    [InlineData("(out value: int, ref other: int)", 2)]
    [InlineData("(values: int...)", 1)]
    [InlineData("((first, second): Pair)", 1)]
    [InlineData("(_: int)", 1)]
    public void ParameterListsPreserveSourceAndShape(string source, int count)
    {
        var parser = Parser(source);
        var result = (ParameterListSyntax)parser.ParseParameterList(
            allowDestructuringPatterns: true, allowDiscardParameters: true).CreateRed();
        Assert.Equal(source, result.ToFullString());
        Assert.Equal(count, result.Parameters.Count);
        Assert.Empty(parser.Diagnostics);
        Assert.Equal(SyntaxKind.EndOfFileToken, parser.PeekToken().Kind);
    }

    [Theory]
    [InlineData("return 1;", SyntaxKind.ReturnExpression)]
    [InlineData("throw failure;", SyntaxKind.ThrowExpression)]
    [InlineData("yield 1;", SyntaxKind.YieldExpression)]
    [InlineData("break;", SyntaxKind.BreakExpression)]
    [InlineData("continue;", SyntaxKind.ContinueExpression)]
    public void AbruptTransferExpressionModePreservesSourceAndShape(string source, SyntaxKind kind)
    {
        var parser = new StatementSyntaxParser(new BaseParseContext(new Lexer(new StringReader(source))),
            parseAbruptTransfersAsExpressions: true);
        var result = Assert.IsType<ExpressionStatementSyntax>(parser.ParseStatement().CreateRed());
        Assert.Equal(kind, result.Expression.Kind);
        Assert.Equal(source, result.ToFullString());
        Assert.Empty(parser.Diagnostics);
    }

    [Fact]
    public void NestedStatementParserInheritsAbruptTransferExpressionMode()
    {
        const string source = "{ return 1; }";
        var parser = new StatementSyntaxParser(new BaseParseContext(new Lexer(new StringReader(source))),
            parseAbruptTransfersAsExpressions: true);
        var result = Assert.IsType<BlockStatementSyntax>(parser.ParseStatement().CreateRed());
        var statement = Assert.IsType<ExpressionStatementSyntax>(Assert.Single(result.Statements));
        Assert.Equal(SyntaxKind.ReturnExpression, statement.Expression.Kind);
        Assert.Equal(source, result.ToFullString());
        Assert.Empty(parser.Diagnostics);
    }

    [Fact]
    public void SkippedTokensRemainAttachedToTheStatement()
    {
        const string source = "val value = 1 unexpected;";
        var parser = Parser(source);
        Assert.Equal(source, parser.ParseStatement().CreateRed().ToFullString());
        Assert.NotEmpty(parser.Diagnostics);
        Assert.Equal(SyntaxKind.EndOfFileToken, parser.PeekToken().Kind);
    }

    private static StatementSyntaxParser Parser(string source) =>
        new(new BaseParseContext(new Lexer(new StringReader(source))));
}
