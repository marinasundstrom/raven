using System;
using System.IO;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class MemberBindingCodeGenTests
{
    [Fact]
    public void MemberBinding_StaticField_FromReferenceAssembly_ResolvesRuntimeField()
    {
        const string code = """
class Program {
    static Get() -> string {
        .Empty
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
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Program", throwOnError: true)!;
        var get = type.GetMethod("Get")!;

        var value = (string)get.Invoke(null, Array.Empty<object>())!;
        Assert.Equal(string.Empty, value);
    }
}
