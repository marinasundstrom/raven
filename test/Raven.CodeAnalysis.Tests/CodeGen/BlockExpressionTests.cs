using System;
using System.IO;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class BlockExpressionTests
{
    [Fact]
    public void IfExpression_WithBlockExpressionBranch_EmitsAndRuns()
    {
        var code = """
class Foo {
    Run(flag: bool, w: int) -> object {
        val y = if flag { 40 + w; } else { true }
        return y
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success);

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Foo", true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Run")!;
        var value = method.Invoke(instance, new object[] { false, 0 });
        Assert.Equal(true, value);
    }
}
