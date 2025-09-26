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

    [Fact]
    public void InterpolatedString_EmitsUnicodeContent_ForLeftToRightScripts()
    {
        var code = """
class Test {
    Format(name: string) -> string {
        let city = "東京";
        return "こんにちは、${name}さん。${city}へようこそ";
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
        var method = type!.GetMethod("Format");

        var name = "花子";
        var expected = $"こんにちは、{name}さん。東京へようこそ";
        var actual = (string)method!.Invoke(instance, new object[] { name })!;
        Assert.Equal(expected, actual);
    }

    [Fact]
    public void InterpolatedString_EmitsUnicodeContent_ForRightToLeftScripts()
    {
        var code = """
class Test {
    Format(name: string, city: string) -> string {
        let greeting = "\u200Fمرحبا ${name}! أهلا بك في ${city}";
        return greeting;
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
        var method = type!.GetMethod("Format");

        var name = "ليلى";
        var city = "دبي";
        var expected = "\u200Fمرحبا " + name + "! أهلا بك في " + city;
        var actual = (string)method!.Invoke(instance, new object[] { name, city })!;
        Assert.Equal(expected, actual);
    }
}
