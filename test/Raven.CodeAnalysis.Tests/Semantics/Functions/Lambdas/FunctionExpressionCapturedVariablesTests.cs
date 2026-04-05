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

    [Fact]
    public void NamedFunctionExpression_CapturesOuterLocal_FromSyntax()
    {
        var code = """
class Calculator {
    func Run() -> int {
        val offset = 2
        val transform = func Step(value: int) -> int {
            value + offset
        }

        transform(4)
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
            .OfType<ParenthesizedFunctionExpressionSyntax>()
            .Single();

        var captures = model.GetCapturedVariables(lambdaSyntax);
        Assert.Contains(captures, static symbol => symbol is ILocalSymbol { Name: "offset" });
    }

    [Fact]
    public void NamedFunctionExpression_CapturesOuterLocal_FromMethodSymbol()
    {
        var code = """
class Calculator {
    func Run() -> int {
        val offset = 2
        val transform = func Step(value: int) -> int {
            if value < 1
                offset
            else
                Step(value - 1)
        }

        transform(4)
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var stepInvocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(inv => inv.Expression is IdentifierNameSyntax identifier && identifier.Identifier.ValueText == "Step");
        var symbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(stepInvocation).Symbol);

        var captures = model.GetCapturedVariables(symbol);
        Assert.Contains(captures, static captured => captured is ILocalSymbol { Name: "offset" });
    }
}
