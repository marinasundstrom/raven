using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class IncompleteExtensionInvocationTests : CompilationTestBase
{
    [Fact]
    public void GetSymbolInfo_IncompleteExtensionInvocation_DoesNotThrow()
    {
        const string source = """
import System.Collections.Generic.*
import System.Linq.*

val numbers = List<int>()
val any = numbers.Any(
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Last();

        var symbolInfo = Record.Exception(() => model.GetSymbolInfo(invocation));

        Assert.Null(symbolInfo);
    }
}
