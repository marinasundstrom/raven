using System;
using System.IO;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class GenericInvocationCodeGenTests
{
    [Fact]
    public void UnconstrainedTypeParameter_ToString_BoxesValueTypes()
    {
        const string code = """
import System.*

class Formatter {
    Format<T>(value: T) -> string {
        return value.ToString()
    }

    Run() -> string {
        return Format<int>(42)
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("generic_invocation", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);
        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("Formatter", true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Run", BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance);
        Assert.NotNull(method);
        var value = (string?)method!.Invoke(instance, Array.Empty<object>());
        Assert.Equal("42", value);
    }
}
