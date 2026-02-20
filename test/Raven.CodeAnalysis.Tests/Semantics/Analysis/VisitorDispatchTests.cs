using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class VisitorDispatchTests : CompilationTestBase
{
    [Fact]
    public void BoundVisitor_VisitExpression_DispatchesToLiteralHandler()
    {
        var compilation = CreateCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var expression = new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 42, intType);
        var visitor = new RecordingBoundVisitor();

        visitor.VisitExpression(expression);

        Assert.True(visitor.SawLiteralExpression);
    }

    [Fact]
    public void BoundVisitor_VisitStatement_DispatchesToReturnHandler()
    {
        var statement = new BoundReturnStatement(expression: null);
        var visitor = new RecordingBoundVisitor();

        visitor.VisitStatement(statement);

        Assert.True(visitor.SawReturnStatement);
    }

    [Fact]
    public void BoundRewriter_VisitExpression_DispatchesPatternFromExpressionPath()
    {
        var compilation = CreateCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var pattern = new BoundDeclarationPattern(intType, new BoundDiscardDesignator(intType));
        var rewriter = new RecordingBoundRewriter();

        var rewritten = rewriter.VisitExpression(pattern);

        Assert.True(rewriter.SawDeclarationPattern);
        Assert.NotNull(rewritten);
    }

    [Fact]
    public void SyntaxVisitor_VisitExpression_DispatchesToBinaryHandler()
    {
        var tree = SyntaxTree.ParseText("val x = 1 + 2;");
        var expression = tree.GetRoot().DescendantNodes().OfType<BinaryExpressionSyntax>().Single();
        var visitor = new RecordingSyntaxVisitor();

        visitor.VisitExpression(expression);

        Assert.True(visitor.SawBinaryExpression);
    }

    [Fact]
    public void SyntaxRewriter_VisitStatement_DispatchesToIfHandler()
    {
        var tree = SyntaxTree.ParseText("if true { }");
        var statement = tree.GetRoot().DescendantNodes().OfType<IfStatementSyntax>().Single();
        var rewriter = new RecordingSyntaxRewriter();

        var rewritten = rewriter.VisitStatement(statement);

        Assert.True(rewriter.SawIfStatement);
        Assert.NotNull(rewritten);
    }

    private sealed class RecordingBoundVisitor : BoundTreeVisitor
    {
        public bool SawLiteralExpression { get; private set; }

        public bool SawReturnStatement { get; private set; }

        public override void VisitLiteralExpression(BoundLiteralExpression node)
        {
            SawLiteralExpression = true;
        }

        public override void VisitReturnStatement(BoundReturnStatement node)
        {
            SawReturnStatement = true;
        }
    }

    private sealed class RecordingBoundRewriter : BoundTreeRewriter
    {
        public bool SawDeclarationPattern { get; private set; }

        public override BoundNode? VisitDeclarationPattern(BoundDeclarationPattern node)
        {
            SawDeclarationPattern = true;
            return node;
        }
    }

    private sealed class RecordingSyntaxVisitor : SyntaxVisitor
    {
        public bool SawBinaryExpression { get; private set; }

        public override void VisitBinaryExpression(BinaryExpressionSyntax node)
        {
            SawBinaryExpression = true;
        }
    }

    private sealed class RecordingSyntaxRewriter : SyntaxRewriter
    {
        public bool SawIfStatement { get; private set; }

        public override SyntaxNode? VisitIfStatement(IfStatementSyntax node)
        {
            SawIfStatement = true;
            return node;
        }
    }
}
