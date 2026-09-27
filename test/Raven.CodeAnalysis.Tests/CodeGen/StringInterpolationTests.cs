using System;
using System.IO;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class StringInterpolationTests
{
    [Theory]
    [InlineData("Value $value")]
    [InlineData("${value}")]
    public void MissingConcatOverloadReportsDiagnostic(string expression)
    {
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
        var core = paths.Single(path => Path.GetFileName(path) == "System.Runtime.dll");
        using var image = Mono.Cecil.AssemblyDefinition.ReadAssembly(core);
        var stringType = image.MainModule.GetType("System.String");
        foreach (var method in stringType.Methods.Where(method => method.Name == "Concat").ToArray())
            stringType.Methods.Remove(method);
        using var modified = new MemoryStream();
        image.Write(modified);
        var references = paths.Select(path => path == core
            ? MetadataReference.CreateFromImage(modified.ToArray())
            : MetadataReference.CreateFromFile(path)).ToArray();
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
            .WithMetadataImportOptions(new MetadataImportOptions("System.Runtime"))
            .WithTargetCoreAssemblyName("System.Runtime");
        var tree = SyntaxTree.ParseText("class Example { static func Run(value: int) { let text = \"" + expression + "\" } }");
        var compilation = Compilation.Create("MissingConcat", [tree], references, options);
        Assert.Contains(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error
            && diagnostic.GetMessage().Contains("Concat"));
        using var output = new MemoryStream();
        Assert.False(compilation.Emit(output).Success);
    }

    [Fact]
    public void InterpolatedString_FormatsCorrectly()
    {
        var code = """
class Test {
    func GetInfo(name: string, age: int, year: int) -> string {
        return "Name: ${name}, Age in ${year}: ${year - (System.DateTime.Now.Year - age)}";
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
        var type = loaded.Assembly.GetType("Test", true);
        var instance = Activator.CreateInstance(type!);
        const BindingFlags flags = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic;
        var method = type!.GetMethod("GetInfo", flags);
        Assert.NotNull(method);

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
    func Format(name: string) -> string {
        let city = "東京";
        return "こんにちは、${name}さん。${city}へようこそ";
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
        var type = loaded.Assembly.GetType("Test", true);
        var instance = Activator.CreateInstance(type!);
        const BindingFlags flags = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic;
        var method = type!.GetMethod("Format", flags);
        Assert.NotNull(method);

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
    func Format(name: string, city: string) -> string {
        let greeting = "\u200Fمرحبا ${name}! أهلا بك في ${city}";
        return greeting;
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
        var type = loaded.Assembly.GetType("Test", true);
        var instance = Activator.CreateInstance(type!);
        const BindingFlags flags = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic;
        var method = type!.GetMethod("Format", flags);
        Assert.NotNull(method);

        var name = "ليلى";
        var city = "دبي";
        var expected = "\u200Fمرحبا " + name + "! أهلا بك في " + city;
        var actual = (string)method!.Invoke(instance, new object[] { name, city })!;
        Assert.Equal(expected, actual);
    }
}
