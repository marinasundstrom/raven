using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Tests;

public class EarlyReturnTypeInferenceTests
{
    [Fact]
    public void ReturnTypeCollector_InfersUnionFromEarlyReturns()
    {
        var code = """
class Foo {
    Test(flag: bool) -> int | () {
        if flag {
            return 42
        } else {
            return ()
        }
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var boundBody = (BoundBlockExpression)model.GetBoundNode(method.Body!)!;

        var inferred = ReturnTypeCollector.Infer(boundBody);

        Assert.NotNull(inferred);
        var union = Assert.IsAssignableFrom<IUnionTypeSymbol>(inferred);
        Assert.Contains(union.Types, t => t.SpecialType == SpecialType.System_Int32);
        Assert.Contains(union.Types, t => t.SpecialType == SpecialType.System_Unit);
    }
}
