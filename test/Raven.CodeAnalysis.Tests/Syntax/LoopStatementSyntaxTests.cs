using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Syntax.Tests;

public sealed class LoopStatementSyntaxTests
{
    [Fact]
    public void LoopStatement_WithBlockBody_ParsesAsDedicatedNode()
    {
        const string testCode = """
loop {
    break
}
""";

        var tree = SyntaxTree.ParseText(testCode);
        var statement = Assert.IsType<GlobalStatementSyntax>(tree.GetRoot().Members.Single()).Statement;
        var loopStatement = Assert.IsType<LoopStatementSyntax>(statement);

        loopStatement.LoopKeyword.Kind.ShouldBe(SyntaxKind.LoopKeyword);
        var body = Assert.IsType<BlockStatementSyntax>(loopStatement.Statement);
        Assert.IsType<BreakStatementSyntax>(body.Statements.Single());
    }

    [Fact]
    public void LoopStatement_WithStatementBody_ParsesEmbeddedStatement()
    {
        const string testCode = """
loop
    continue
""";

        var tree = SyntaxTree.ParseText(testCode);
        var statement = Assert.IsType<GlobalStatementSyntax>(tree.GetRoot().Members.Single()).Statement;
        var loopStatement = Assert.IsType<LoopStatementSyntax>(statement);

        Assert.IsType<ContinueStatementSyntax>(loopStatement.Statement);
    }
}
