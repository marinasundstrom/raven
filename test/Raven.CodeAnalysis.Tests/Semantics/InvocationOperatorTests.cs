using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class InvocationOperatorTests
{
    [Fact]
    public void InvocationOperator_BindsToInvokeMethod()
    {
        var source = """
class Test {
    public self(no: int) -> int {
        return no + 1;
    }
}
let t = Test()
let x = t(2)
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Last();
        var symbol = (IMethodSymbol)model.GetSymbolInfo(invocation).Symbol!;
        Assert.Equal("Invoke", symbol.Name);
    }
}

