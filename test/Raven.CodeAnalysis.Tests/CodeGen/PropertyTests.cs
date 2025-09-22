using System;
using System.IO;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public class PropertyTests
{
    [Fact]
    public void AutoProperty_GeneratesBackingField()
    {
        var code = """
class Sample {
    public Value: int { get; set; }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion("net9.0");
        MetadataReference[] references = [
            .. TargetFrameworkResolver.GetReferenceAssemblies(version)
                .Select(path => MetadataReference.CreateFromFile(path))
        ];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        var compilationType = compilation.GetTypeByMetadataName("Sample");
        Assert.NotNull(compilationType);
        Assert.Contains("Value", compilationType!.GetMembers().Select(m => m.Name));

        var propertySymbol = Assert.Single(
            compilationType.GetMembers().OfType<IPropertySymbol>(),
            p => p.Name == "Value");
        Assert.NotNull(propertySymbol.GetMethod);
        Assert.NotNull(propertySymbol.SetMethod);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var runtimeAssembly = loaded.Assembly;
        var type = runtimeAssembly.GetType("Sample", throwOnError: true)!;
        var instance = Activator.CreateInstance(type)!;

        var property = type.GetProperty("Value", BindingFlags.Public | BindingFlags.Instance);
        Assert.NotNull(property);
        Assert.Equal(0, (int)property!.GetValue(instance)!);

        property.SetValue(instance, 42);
        Assert.Equal(42, (int)property.GetValue(instance)!);
    }

    [Fact]
    public void StaticAutoProperty_GeneratesBackingField()
    {
        var code = """
class Counter {
    public static Count: int { get; set; }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion("net9.0");
        MetadataReference[] references = [
            .. TargetFrameworkResolver.GetReferenceAssemblies(version)
                .Select(path => MetadataReference.CreateFromFile(path))
        ];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        var compilationType = compilation.GetTypeByMetadataName("Counter");
        Assert.NotNull(compilationType);
        Assert.Contains("Count", compilationType!.GetMembers().Select(m => m.Name));

        var propertySymbol = Assert.Single(
            compilationType.GetMembers().OfType<IPropertySymbol>(),
            p => p.Name == "Count");
        Assert.NotNull(propertySymbol.GetMethod);
        Assert.NotNull(propertySymbol.SetMethod);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var runtimeAssembly = loaded.Assembly;
        var type = runtimeAssembly.GetType("Counter", throwOnError: true)!;

        var property = type.GetProperty("Count", BindingFlags.Public | BindingFlags.Static);
        Assert.NotNull(property);
        Assert.Equal(0, (int)property!.GetValue(null)!);

        property.SetValue(null, 7);
        Assert.Equal(7, (int)property.GetValue(null)!);
    }
}
