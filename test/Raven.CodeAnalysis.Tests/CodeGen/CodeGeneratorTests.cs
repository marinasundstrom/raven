using System;
using System.IO;
using System.Reflection;
using System.Runtime.Loader;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class CodeGeneratorTests
{
    [Fact]
    public void Emit_ShouldGenerateClass()
    {
        var code = """
class Foo {
    Test() -> void {
        return;
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var refAssembliesPath = ReferenceAssemblyPaths.GetReferenceAssemblyPaths();

        var runtimePath = ReferenceAssemblyPaths.GetRuntimeDll();

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(
                MetadataReference.CreateFromFile(runtimePath));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.True(result.Success);

        peStream.Seek(0, SeekOrigin.Begin);

        var assembly = AssemblyLoadContext.Default.LoadFromStream(peStream);

        Assert.NotNull(assembly.GetType("Foo", true));
    }
}
