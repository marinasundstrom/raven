using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Macros.Tests;

public sealed partial class TokenTreeMacroContextTests
{
    [Fact]
    public void GetTypeInfo_ResolvesParsedExpressionInInvocationScope()
    {
        var context = CreateContextWithLocal("count");
        var expression = context.ParseExpression();

        var info = context.GetTypeInfo(expression);

        Assert.Equal(SpecialType.System_Int32, info.Type?.SpecialType);
    }

    [Fact]
    public void GetSymbolInfo_ResolvesParsedExpressionInInvocationScope()
    {
        var context = CreateContextWithLocal("count");
        var expression = context.ParseExpression();

        var info = context.GetSymbolInfo(expression);

        var local = Assert.IsAssignableFrom<LocalSymbol>(info.Symbol);
        Assert.Equal("count", local.Name);
        Assert.Equal(SpecialType.System_Int32, local.Type.SpecialType);
    }

    private static TokenTreeMacroContext CreateContextWithLocal(string body)
    {
        var tree = SyntaxTree.ParseText($$"""
            func Main() {
                let count = 42
                probe! { {{body}} }
            }
            """);
        var compilation = Compilation.Create(
                "MacroFragmentSemantics",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default)
            .AddSyntaxTrees(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();

        return new TokenTreeMacroContext(
            compilation,
            compilation.GetSemanticModel(tree),
            invocation);
    }

}
