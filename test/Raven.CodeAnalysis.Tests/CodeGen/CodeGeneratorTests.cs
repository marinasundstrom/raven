using System;
using System.IO;
using System.Reflection;
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
class Program {
    Main() -> void {
        return;
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(
                MetadataReference.CreateFromFile(typeof(object).Assembly.Location),
                MetadataReference.CreateFromFile(typeof(Console).Assembly.Location));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.True(result.Success);

        peStream.Position = 0;
        var assembly = Assembly.Load(peStream.ToArray());
        Assert.NotNull(assembly.GetType("Program"));
    }
}
