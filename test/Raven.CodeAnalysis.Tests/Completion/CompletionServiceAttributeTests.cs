using System;
using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Completion;

public class CompletionServiceAttributeTests
{
    [Fact]
    public void GetCompletions_InAttributeName_ReturnsAttributeTypesWithoutSuffix()
    {
        const string code = """
class PureAttribute : System.Attribute {
}

[Pu]
func Test() {
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var position = code.IndexOf("]", StringComparison.Ordinal);
        var items = new CompletionService().GetCompletions(compilation, syntaxTree, position).ToList();

        var item = Assert.Single(items.Where(static item => item.DisplayText == "Pure"));
        Assert.Equal("Pure", item.InsertionText);
    }

    [Fact]
    public void GetCompletions_InQualifiedAttributeName_ReturnsOnlyAttributeTypesWithoutSuffix()
    {
        const string code = """
[System.Runtime.CompilerServices.]
func Test() {
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var position = code.IndexOf("]", StringComparison.Ordinal);
        var items = new CompletionService().GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, static item => item.DisplayText == "CompilerGenerated");
        Assert.DoesNotContain(items, static item => item.DisplayText == "Unsafe");
    }

    [Fact]
    public void GetCompletions_InQualifiedAttributeNamespace_ReturnsNamespacesAndAttributeTypes()
    {
        const string code = """
[System.]
func Test() {
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var position = code.IndexOf("]", StringComparison.Ordinal);
        var items = new CompletionService().GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, static item => item.DisplayText == "Runtime");
        Assert.Contains(items, static item => item.DisplayText == "Obsolete");
        Assert.DoesNotContain(items, static item => item.DisplayText == "String");
    }
}
