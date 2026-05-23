using System;
using System.IO;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class StringEqualityCodeGenTests
{
    [Fact]
    public void StringEquality_UsesValueEquality()
    {
        var code = """
import System.*

class Runner {
    public func Equal() -> bool {
        val value = String.Concat("Sta", "tus")
        return value == "Status"
    }

    public func NotEqual() -> bool {
        val value = String.Concat("Sta", "tus")
        return value != "Status"
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
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("Runner", throwOnError: true)!;
        var instance = Activator.CreateInstance(type)!;
        var equal = type.GetMethod("Equal", BindingFlags.Instance | BindingFlags.Public)!;
        var notEqual = type.GetMethod("NotEqual", BindingFlags.Instance | BindingFlags.Public)!;

        Assert.True((bool)equal.Invoke(instance, Array.Empty<object>())!);
        Assert.False((bool)notEqual.Invoke(instance, Array.Empty<object>())!);
    }
}
