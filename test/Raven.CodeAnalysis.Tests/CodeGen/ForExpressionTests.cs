using System.IO;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class ForExpressionTests
{
    [Fact]
    public void ForEach_OverArray_UsesTypedElement()
    {
        var code = """
class Foo {
    Run() -> int {
        var items = [1, 2, 3]
        var total: int = 0
        for each item in items {
            total = total + item
        }
        return total
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
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
        var value = (int)method.Invoke(instance, Array.Empty<object>())!;
        Assert.Equal(6, value);
    }

    [Fact]
    public void ForEach_InExtensionMethod_OverSelfEnumerable()
    {
        var code = """
import System.Collections.Generic.*

extension EnumerableExt<T> for IEnumerable<T> {
    CountItems() -> int {
        var total: int = 0
        for each item in self {
            total = total + 1
        }
        return total
    }
}

class Program {
    Run() -> int {
        var items = List<int>()
        items.Add(1)
        items.Add(2)
        return items.CountItems()
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success);

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Program", throwOnError: true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Run")!;
        var value = (int)method.Invoke(instance, Array.Empty<object>())!;
        Assert.Equal(2, value);
    }
}
