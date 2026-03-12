using System.IO;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class BaseConstructorTests
{
    [Fact]
    public void ParameterlessBaseConstructor_IsCalled()
    {
        var code = """
open class Base {
    var initialized: bool = false
    init() { initialized = true }
    val IsInitialized: bool {
        get { return initialized }
    }
}

class Derived : Base {
    init() {}
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion("net10.0");
        MetadataReference[] references = [
            .. TargetFrameworkResolver.GetReferenceAssemblies(version)
                .Select(path => MetadataReference.CreateFromFile(path))
        ];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success);

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var runtimeAssembly = loaded.Assembly;
        var type = runtimeAssembly.GetType("Derived", throwOnError: true)!;
        var instance = Activator.CreateInstance(type)!;
        var prop = type.GetProperty("IsInitialized", BindingFlags.Public | BindingFlags.Instance)!;
        Assert.True((bool)prop.GetValue(instance)!);
    }

    [Fact]
    public void ImplicitBaseConstructor_IsCalled()
    {
        var code = """
open class Base {}

class Derived : Base {
    init() {}
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion("net10.0");
        MetadataReference[] references = [
            .. TargetFrameworkResolver.GetReferenceAssemblies(version)
                .Select(path => MetadataReference.CreateFromFile(path))
        ];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success);

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var runtimeAssembly = loaded.Assembly;
        var type = runtimeAssembly.GetType("Derived", throwOnError: true)!;
        var instance = Activator.CreateInstance(type);
        Assert.NotNull(instance);
    }

    [Fact]
    public void ExplicitBaseConstructor_WithArguments_IsCalled()
    {
        var code = """
open class Base {
    var stored: int = 0
    init(value: int) { stored = value }
    val Value: int {
        get { return stored }
    }
}

class Derived : Base {
    init(value: int): base(value) {}
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion("net10.0");
        MetadataReference[] references = [
            .. TargetFrameworkResolver.GetReferenceAssemblies(version)
                .Select(path => MetadataReference.CreateFromFile(path))
        ];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success);

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var runtimeAssembly = loaded.Assembly;
        var type = runtimeAssembly.GetType("Derived", throwOnError: true)!;
        var instance = Activator.CreateInstance(type, new object[] { 42 })!;
        var prop = type.GetProperty("Value", BindingFlags.Public | BindingFlags.Instance)!;
        Assert.Equal(42, (int)prop.GetValue(instance)!);
    }
}
