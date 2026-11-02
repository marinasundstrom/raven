using Green = Raven.CodeAnalysis.Syntax.InternalSyntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "Shared")]
public sealed class MissingExpressionVisitorTests
{
    [Fact]
    public void GreenMissingExpression_DispatchesToDefaultWithoutRecursing()
    {
        var missing = new Green.ExpressionSyntax.Missing();
        var visitor = new RecordingVisitor();
        visitor.Visit(missing);
        Assert.Equal(1, visitor.Visits);
        Assert.Same(missing, visitor.DefaultNode);
    }

    [Fact]
    public void GreenMissingExpression_GenericDispatchPreservesDefaultResult()
    {
        var missing = new Green.ExpressionSyntax.Missing();
        var visitor = new RecordingResultVisitor();
        Assert.Same(missing, visitor.Visit(missing));
        Assert.Equal(1, visitor.Visits);
        Assert.Same(missing, visitor.DefaultNode);
    }

    private sealed class RecordingVisitor : Green.SyntaxVisitor
    {
        public int Visits { get; private set; }
        public Green.SyntaxNode? DefaultNode { get; private set; }
        public override void Visit(Green.SyntaxNode node)
        {
            if (++Visits > 1)
                throw new InvalidOperationException("Missing expression redispatched into Visit.");
            base.Visit(node);
        }
        public override void DefaultVisit(Green.SyntaxNode node) => DefaultNode = node;
    }

    private sealed class RecordingResultVisitor : Green.SyntaxVisitor<Green.SyntaxNode>
    {
        public int Visits { get; private set; }
        public Green.SyntaxNode? DefaultNode { get; private set; }
        public override Green.SyntaxNode? Visit(Green.SyntaxNode? node)
        {
            if (++Visits > 1)
                throw new InvalidOperationException("Missing expression redispatched into Visit.");
            return base.Visit(node);
        }
        public override Green.SyntaxNode DefaultVisit(Green.SyntaxNode node) => DefaultNode = node;
    }
}
