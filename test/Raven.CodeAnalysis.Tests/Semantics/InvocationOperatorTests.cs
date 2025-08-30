using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class InvocationOperatorTests : CompilationTestBase
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
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Last();
        var symbol = (IMethodSymbol)model.GetSymbolInfo(invocation).Symbol!;
        Assert.Equal("Invoke", symbol.Name);
    }
}

