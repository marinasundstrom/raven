using System;
using System.IO;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class StringInterpolationTests
{
    [Fact]
    public void InterpolatedString_FormatsCorrectly()
    {
        var code = """
class Test {
    GetInfo(name: string, age: int, year: int) -> string {
        return "Name: ${name}, Age in ${year}: ${year - (System.DateTime.Now.Year - age)}";
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
        var type = loaded.Assembly.GetType("Test", true);
        var instance = Activator.CreateInstance(type!);
        var method = type!.GetMethod("GetInfo");

        var name = "Alice";
        var age = 30;
        var year = 2030;
        var expected = $"Name: {name}, Age in {year}: {year - (System.DateTime.Now.Year - age)}";
        var actual = (string)method!.Invoke(instance, new object[] { name, age, year })!;
        Assert.Equal(expected, actual);
    }
}
