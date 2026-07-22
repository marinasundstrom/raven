using System;
using System.IO;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class ConditionalAccessTests
{
    [Fact]
    public void ConditionalAccess_NullReference_ReturnsNull()
    {
        var code = """
class Foo {
    public func Run() -> string? {
        var x: object? = null
        return x?.ToString()
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
        var type = loaded.Assembly.GetType("Foo", true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Run")!;
        var value = method.Invoke(instance, Array.Empty<object>());
        Assert.Null(value);
    }

    [Fact]
    public void ConditionalAccess_NullableValue_ReturnsValue()
    {
        var code = """
class Foo {
    public func Run() -> string? {
        var x: int? = 1
        return x?.ToString()
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
        var type = loaded.Assembly.GetType("Foo", true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Run")!;
        var value = (string?)method.Invoke(instance, Array.Empty<object>());
        Assert.Equal("1", value);
    }

    [Fact]
    public void ConditionalAccess_NullableValueEquality_EmitsAndRuns()
    {
        var code = """
class Foo {
    public func HasLength() -> bool {
        val values: int[]? = [1, 2]
        return values?.Length == 2
    }

    public func MissingLengthIsZero() -> bool {
        val values: int[]? = null
        return values?.Length == 0
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
        var type = loaded.Assembly.GetType("Foo", true)!;
        var instance = Activator.CreateInstance(type)!;

        Assert.True((bool)type.GetMethod("HasLength")!.Invoke(instance, Array.Empty<object>())!);
        Assert.False((bool)type.GetMethod("MissingLengthIsZero")!.Invoke(instance, Array.Empty<object>())!);
    }

    [Fact]
    public void ConditionalInvocation_NullDelegate_ReturnsNull()
    {
        var code = """
import System.*

class Foo {
    public func Run() -> int? {
        var f: Func<int, int>? = null
        return f?(2)
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
        var type = loaded.Assembly.GetType("Foo", true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Run")!;
        var value = method.Invoke(instance, Array.Empty<object>());
        Assert.Null(value);
    }

    [Fact]
    public void ConditionalAccess_ExtensionMethod_OnNullableReceiver_ReturnsValue()
    {
        var code = """
import System.*

class Foo(value: int) {}

extension Test for Foo {
    func Hello() -> string? {
        return self.ToString()
    }
}

class Runner {
    public func Run() -> string? {
        var f: Foo? = Foo(21)
        return f?.Hello()
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
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics.Select(d => d.ToString())));
        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("Runner", true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Run")!;
        var value = (string?)method.Invoke(instance, Array.Empty<object>());
        Assert.Equal("Foo", value);
    }

    [Fact]
    public void ConditionalAccess_Assignment_NonNullReceiver_AssignsMember()
    {
        var code = """
class Person {
    public var Name: string = ""
}

class Runner {
    public func Run() -> string {
        var person: Person? = Person()
        person?.Name = "Ada"
        return person!.Name
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
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics.Select(d => d.ToString())));
        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("Runner", true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Run")!;
        var value = (string)method.Invoke(instance, Array.Empty<object>())!;
        Assert.Equal("Ada", value);
    }

    [Fact]
    public void ConditionalAccess_Assignment_NullReceiver_SkipsMemberWrite()
    {
        var code = """
class Person {
    public var Name: string = "before"
}

class Runner {
    public func Run() -> string {
        var person: Person? = null
        person?.Name = "after"
        return "ok"
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
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics.Select(d => d.ToString())));
        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("Runner", true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Run")!;
        var value = (string)method.Invoke(instance, Array.Empty<object>())!;
        Assert.Equal("ok", value);
    }

    [Fact]
    public void ConditionalAccess_CompoundAssignment_NonNullReceiver_UpdatesMember()
    {
        var code = """
class Counter {
    public var Value: int = 10
}

class Runner {
    public func Run() -> int {
        var counter: Counter? = Counter()
        counter?.Value += 5
        return counter!.Value
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
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics.Select(d => d.ToString())));
        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("Runner", true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Run")!;
        var value = (int)method.Invoke(instance, Array.Empty<object>())!;
        Assert.Equal(15, value);
    }

    [Fact]
    public void ConditionalElementAccess_NullArray_ReturnsNull()
    {
        var code = """
class Foo {
    public func Run() -> int? {
        var values: int[]? = null
        return values?[0]
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
        var type = loaded.Assembly.GetType("Foo", true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Run")!;
        var value = method.Invoke(instance, Array.Empty<object>());
        Assert.Null(value);
    }

    [Fact]
    public void ConditionalElementAccess_NonNullArray_ReturnsElement()
    {
        var code = """
class Foo {
    public func Run() -> int? {
        var values: int[]? = [1, 2, 3]
        return values?[1]
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
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics.Select(d => d.ToString())));
        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("Foo", true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Run")!;
        var value = method.Invoke(instance, Array.Empty<object>());
        Assert.Equal(2, value);
    }
}
