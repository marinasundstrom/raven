using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class LambdaCapturedVariablesTests
{
    [Fact]
    public void Lambda_CapturingParameter_ReportsParameterAsCapture()
    {
        var code = """
class Calculator {
    Apply(base: int) -> int {
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
            .First(node => node.Kind == SyntaxKind.ParenthesizedLambdaExpression);

        var boundLambda = Assert.IsType<BoundLambdaExpression>(model.GetBoundNode(lambdaSyntax));

        var captured = boundLambda.CapturedVariables.ToArray();
        var parameter = Assert.Single(boundLambda.Parameters, p => p.Name == "base");

        Assert.Contains(parameter, captured, SymbolEqualityComparer.Default);
    }

    [Fact]
    public void Lambda_CapturingLocal_ReportsLocalAsCapture()
    {
        var code = """
class Calculator {
    Apply() -> int {
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
            .First(node => node.Kind == SyntaxKind.ParenthesizedLambdaExpression);

        var boundLambda = Assert.IsType<BoundLambdaExpression>(model.GetBoundNode(lambdaSyntax));

        var captured = boundLambda.CapturedVariables.ToArray();
        var local = Assert.Single(captured.OfType<ILocalSymbol>(), symbol => symbol.Name == "factor");

        Assert.Contains(local, captured, SymbolEqualityComparer.Default);
    }
}
