using System.Linq;

using Raven.CodeAnalysis.Operations;
using Raven.CodeAnalysis.Syntax;

using Shouldly;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ControlFlowExpressionTests : CompilationTestBase
{
    [Fact]
    public void BreakExpressionInMatch_MakesInfiniteLoopEndpointReachable()
    {
        const string source = """
func Main() {
    loop {
        let value = 1
        let selected = match value {
            1 => break
            _ => value
        }
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();
        diagnostics.ShouldBeEmpty();

        var model = compilation.GetSemanticModel(tree);
        var loop = tree.GetRoot().DescendantNodes().OfType<LoopStatementSyntax>().Single();
        var analysis = model.AnalyzeControlFlow(loop);

        analysis.Succeeded.ShouldBeTrue();
        analysis.EndPointIsReachable.ShouldBeTrue();
    }

    [Fact]
    public void ContinueExpressionInMatch_DoesNotCompleteArmNormally()
    {
        const string source = """
func Main() {
    var count = 0
    loop {
        count += 1
        let selected = match count {
            1 => continue
            _ => break
        }
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();
        diagnostics.ShouldBeEmpty();

        var model = compilation.GetSemanticModel(tree);
        var declaration = tree.GetRoot().DescendantNodes()
            .OfType<LocalDeclarationStatementSyntax>()
            .Last();
        var analysis = model.AnalyzeControlFlow(declaration);

        analysis.Succeeded.ShouldBeTrue();
        analysis.EndPointIsReachable.ShouldBeFalse();
    }

    [Theory]
    [InlineData("break", OperationKind.BreakExpression)]
    [InlineData("continue", OperationKind.ContinueExpression)]
    public void GetOperation_LoopTransferExpression_ReturnsExpressionOperation(
        string expressionText,
        OperationKind expectedKind)
    {
        var source = $$"""
func Main() {
    loop {
        let value = match true {
            true => {{expressionText}}
            _ => ()
        }
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        _ = compilation.GetDiagnostics();
        var expression = tree.GetRoot().DescendantNodes().OfType<ExpressionSyntax>()
            .Single(node => node.Kind is SyntaxKind.BreakExpression or SyntaxKind.ContinueExpression);

        var operation = model.GetOperation(expression);

        operation.ShouldNotBeNull();
        operation.Kind.ShouldBe(expectedKind);
        operation.Type.ShouldNotBeNull();
    }

    [Fact]
    public void GetOperation_YieldExpressions_ReturnIteratorExpressionOperations()
    {
        const string source = """
import System.Collections.Generic.*

func Items(stop: bool) -> IEnumerable<int> {
    match stop {
        false => yield 1
        _ => yield break
    }
    yield break
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        compilation.GetDiagnostics().ShouldBeEmpty();

        var yieldExpression = tree.GetRoot().DescendantNodes().OfType<YieldExpressionSyntax>().Single();
        var yieldBreakExpression = tree.GetRoot().DescendantNodes().OfType<YieldBreakExpressionSyntax>().Single();

        model.GetOperation(yieldExpression)!.Kind.ShouldBe(OperationKind.YieldExpression);
        model.GetOperation(yieldBreakExpression)!.Kind.ShouldBe(OperationKind.YieldBreakExpression);
    }
}
