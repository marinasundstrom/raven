using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Operations;
using Raven.CodeAnalysis.Syntax;

using Shouldly;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class AssignmentAnalysisTests : CompilationTestBase
{
    [Fact]
    public void AnalyzeDataFlow_AssignmentStatement_WritesLocal()
    {
        const string source = """
var value = 0
value = 1
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var assignment = tree.GetRoot()
            .DescendantNodes()
            .OfType<AssignmentStatementSyntax>()
            .Single();

        var analysis = model.AnalyzeDataFlow(assignment);

        analysis.Succeeded.ShouldBeTrue();
        analysis.WrittenInside.ShouldContain(symbol => symbol.Name == "value");
        analysis.DataFlowsOut.ShouldContain(symbol => symbol.Name == "value");
    }

    [Fact]
    public void AnalyzeDataFlow_PatternAssignment_WritesEachLocal()
    {
        const string source = """
var first = 0
var second = 0
(first, second) = (1, 2)
first + second
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var assignment = tree.GetRoot()
            .DescendantNodes()
            .OfType<AssignmentStatementSyntax>()
            .Single();

        var boundAssignment = model.GetBoundNode(assignment).ShouldBeOfType<BoundAssignmentStatement>();
        boundAssignment.Expression.ShouldBeOfType<BoundPatternAssignmentExpression>();

        var analysis = model.AnalyzeDataFlow(assignment);

        analysis.Succeeded.ShouldBeTrue();
        analysis.WrittenInside.Select(symbol => symbol.Name)
            .ShouldBe(new[] { "first", "second" }, ignoreOrder: true);
        var dataFlowsOutNames = analysis.DataFlowsOut.Select(symbol => symbol.Name).ToArray();
        dataFlowsOutNames.ShouldContain("first");
        dataFlowsOutNames.ShouldContain("second");
    }

    [Fact]
    public void AnalyzeDataFlow_DiscardAssignment_IgnoresWrites()
    {
        const string source = "_ = 1";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var assignment = tree.GetRoot()
            .DescendantNodes()
            .OfType<AssignmentStatementSyntax>()
            .Single();

        var analysis = model.AnalyzeDataFlow(assignment);

        analysis.Succeeded.ShouldBeTrue();
        analysis.WrittenInside.ShouldBeEmpty();
        analysis.DataFlowsOut.ShouldBeEmpty();
    }

    [Fact]
    public void AnalyzeControlFlow_AssignmentStatement_Succeeds()
    {
        const string source = """
var value = 0
value = 1
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var assignment = tree.GetRoot()
            .DescendantNodes()
            .OfType<AssignmentStatementSyntax>()
            .Last();

        var analysis = model.AnalyzeControlFlow(assignment);

        analysis.Succeeded.ShouldBeTrue();
        analysis.StartPointIsReachable.ShouldBeTrue();
        analysis.EndPointIsReachable.ShouldBeTrue();
        analysis.ReturnStatements.ShouldBeEmpty();
    }

    [Fact]
    public void GetOperation_AssignmentStatement_ReturnsAssignmentOperation()
    {
        const string source = """
var value = 0
value = 1
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var assignment = tree.GetRoot()
            .DescendantNodes()
            .OfType<AssignmentStatementSyntax>()
            .Last();

        var operation = model.GetOperation(assignment);

        operation.ShouldNotBeNull();
        operation!.Kind.ShouldBe(OperationKind.Assignment);
        operation.IsImplicit.ShouldBeFalse();
        operation.Syntax.ShouldBe(assignment);
    }
}
