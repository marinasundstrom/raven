using System;
using System.IO;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class LambdaCodeGenTests
{
    [Fact]
    public void Lambda_ExpressionBody_ReturnsSum()
    {
        var code = """
class Calculator {
    Add() -> int {
        val add = (x: int, y: int) -> int => x + y
        return add(2, 3)
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
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Calculator", throwOnError: true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Add")!;

        var value = (int)method.Invoke(instance, Array.Empty<object>())!;
        Assert.Equal(5, value);
    }

    [Fact]
    public void Lambda_ComparisonExpression_ReturnsExpectedResults()
    {
        var code = """
class Checker {
    AreEqual(left: int, right: int) -> bool {
        val equals = (x: int, y: int) -> bool => x == y
        return equals(left, right)
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
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Checker", throwOnError: true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("AreEqual")!;

        var trueResult = (bool)method.Invoke(instance, new object[] { 4, 4 })!;
        Assert.True(trueResult);

        var falseResult = (bool)method.Invoke(instance, new object[] { 3, 4 })!;
        Assert.False(falseResult);
    }

    [Fact]
    public void Lambda_BlockBody_ReturnsComputedValue()
    {
        var code = """
class Calculator {
    Sum() -> int {
        val make = (x: int, y: int) -> int => {
            val total = x + y
            total
        }

        return make(4, 6)
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
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Calculator", throwOnError: true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Sum")!;

        var value = (int)method.Invoke(instance, Array.Empty<object>())!;
        Assert.Equal(10, value);
    }

    [Fact]
    public void Lambda_BlockBody_WithExplicitReturn_ReturnsComputedValue()
    {
        var code = """
class Calculator {
    Sum() -> int {
        val make = (x: int, y: int) -> int => {
            return x + y;
        }

        return make(4, 6)
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
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Calculator", throwOnError: true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Sum")!;

        var value = (int)method.Invoke(instance, Array.Empty<object>())!;
        Assert.Equal(10, value);
    }

    [Fact]
    public void Lambda_CapturesParameter_ReturnsExpectedResult()
    {
        var code = """
class Calculator {
    Combine(x: int) -> int {
        val add = (y: int) -> int => x + y
        return add(4)
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
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Calculator", throwOnError: true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Combine")!;

        var value = (int)method.Invoke(instance, new object[] { 6 })!;
        Assert.Equal(10, value);
    }

    [Fact]
    public void Lambda_CapturesLocal_ReturnsValue()
    {
        var code = """
class Counter {
    Multiply() -> int {
        val factor = 5
        val multiply = (value: int) -> int => factor * value
        return multiply(3)
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
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Counter", throwOnError: true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Multiply")!;

        var value = (int)method.Invoke(instance, Array.Empty<object>())!;
        Assert.Equal(15, value);
    }

    [Fact]
    public void Lambda_CapturesSelfField_UsesInstanceState()
    {
        var code = """
class Holder {
    value: int = 8

    Compute() -> int {
        val add = (offset: int) -> int => self.value + offset
        return add(7)
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
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Holder", throwOnError: true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Compute")!;

        var value = (int)method.Invoke(instance, Array.Empty<object>())!;
        Assert.Equal(15, value);
    }

}
