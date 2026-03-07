using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class FunctionExpressionCapturedVariablesTests
{
    [Fact]
    public void Lambda_CapturingParameter_ReportsParameterAsCapture()
    {
        var code = """
class Calculator {
    func Apply(base: int) -> int {
        val lambda = (offset: int) -> int => base + offset
        return lambda(2)
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var lambdaSyntax = tree.GetRoot()
            .DescendantNodes()
            .First(node => node.Kind == SyntaxKind.ParenthesizedFunctionExpression);

        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));

        Assert.Single(boundLambda.Parameters, p => p.Name == "offset");
        Assert.Empty(boundLambda.CapturedVariables);
    }

    [Fact]
    public void Lambda_CapturingLocal_ReportsLocalAsCapture()
    {
        var code = """
class Calculator {
    func Apply() -> int {
        val factor = 3
        val lambda = (value: int) -> int => factor * value
        return lambda(4)
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var lambdaSyntax = tree.GetRoot()
            .DescendantNodes()
            .First(node => node.Kind == SyntaxKind.ParenthesizedFunctionExpression);

        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));

        var capture = Assert.Single(boundLambda.CapturedVariables);
        Assert.Equal("factor", capture.Name);
    }
}
