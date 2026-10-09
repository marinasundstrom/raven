using System;
using System.IO;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class StringInterpolationTests
{
    [Theory]
    [InlineData("\"Value: $value\"", "Value: 42")]
    [InlineData("\"${value}\"", "42")]
    [InlineData("\"Value: \" + value", "Value: 42")]
    public void SynthesizedConcatPreservesArgumentConversions(string expression, string expected)
    {
        var tree = SyntaxTree.ParseText("public class Example { public static func Format(value: int) -> string => " + expression + " }");
        var compilation = Compilation.Create("ConcatConversions", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var syntax = tree.GetRoot().DescendantNodes().OfType<ExpressionSyntax>()
            .First(n => n is InterpolatedStringExpressionSyntax or InfixOperatorExpressionSyntax);
        var call = Assert.IsType<BoundInvocationExpression>(compilation.GetSemanticModel(tree).GetBoundNode(syntax));
        var arguments = call.Arguments.ToArray();
        Assert.Equal(call.Method.Parameters.Length, arguments.Length);
        var operands = arguments.SelectMany(argument => argument is BoundCollectionExpression collection
            ? collection.Elements : [argument]).ToArray();
        Assert.True(Assert.IsType<BoundConversionExpression>(operands.Last()).IsBoxing);
        for (var i = 0; i < arguments.Length; i++)
            Assert.Equal(call.Method.Parameters[i].Type, arguments[i].Type, SymbolEqualityComparer.Default);
        using var output = new MemoryStream();
        var emitted = compilation.Emit(output);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(output, compilation.References);
        Assert.Equal(expected, loaded.Assembly.GetType("Example")!.GetMethod("Format")!.Invoke(null, [42]));
    }

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
    public void UnionWithoutRuntimeConcatReportsDiagnosticDuringEmission()
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
        var compilation = Compilation.Create("MissingUnionConcat",
            [SyntaxTree.ParseText("public union Outcome { case Ready(int) case Empty }")], references, options);
        using var output = new MemoryStream();
        var result = compilation.Emit(output);
        Assert.False(result.Success);
        Assert.Contains(result.Diagnostics, d => d.Id == "RAV1501" && d.GetMessage().Contains("String.Concat"));
        Assert.Equal(0, output.Length);
    }

    [Fact]
    public void InterpolationPreservesEvaluationOrderAndNullText()
    {
        var tree = SyntaxTree.ParseText("""
            class Counter {
                private var count: int = 0
                func Next() -> int {
                    count += 1
                    return count
                }
            }
            public class Example {
                public static func Format() -> string {
                    let counter = Counter()
                    let absent: object? = null
                    return "${counter.Next()}:${counter.Next()}:${absent}"
                }
            }
            """);
        var compilation = Compilation.Create("ConcatEvaluation", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var output = new MemoryStream();
        var emitted = compilation.Emit(output);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(output, compilation.References);
        Assert.Equal("1:2:", loaded.Assembly.GetType("Example")!.GetMethod("Format")!.Invoke(null, null));
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
