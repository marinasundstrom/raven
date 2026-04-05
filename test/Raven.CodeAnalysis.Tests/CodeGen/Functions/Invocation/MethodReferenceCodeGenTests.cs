using System;
using System.IO;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class MethodReferenceCodeGenTests
{
    [Fact]
    public void MethodReference_StaticMethod_InvokesTarget()
    {
        const string code = """
class Calculator {
    public static func Add(x: int, y: int) -> int { x + y }

    public static func Compute() -> int {
        val add = Calculator.Add
        add(2, 3)
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
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Calculator", throwOnError: true)!;
        var compute = type.GetMethod("Compute")!;

        var value = (int)compute.Invoke(null, Array.Empty<object>())!;
        Assert.Equal(5, value);
    }

    [Fact]
    public void MethodReference_InstanceMethod_InvokesTarget()
    {
        const string code = """
class Counter {
    var value: int = 3

    public func Increment(delta: int) -> int { self.value + delta }

    public func Run() -> int {
        val increment = self.Increment
        increment(7)
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
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Counter", throwOnError: true)!;
        var instance = Activator.CreateInstance(type)!;
        var run = type.GetMethod("Run")!;

        var value = (int)run.Invoke(instance, Array.Empty<object>())!;
        Assert.Equal(10, value);
    }

    [Fact]
    public void MethodReference_SynthesizedDelegate_IsEmitted()
    {
        const string code = """
class Accumulator {
    static func TryAccumulate(ref state: int, out doubled: int) -> bool {
        state = 21
        doubled = 42
        true
    }

    static func Execute(value: int) -> int {
        val callback = Accumulator.TryAccumulate
        var current = value
        var doubled = 0

        val result = callback(&current, &doubled)
        if !result {
            return -1
        }

        current + doubled
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
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;

        var accumulatorType = assembly.GetType("Accumulator", throwOnError: true)!;
        var execute = accumulatorType.GetMethod("Execute", BindingFlags.Public | BindingFlags.Static);
        Assert.NotNull(execute);

        Assert.Equal(typeof(int), execute!.ReturnType);
    }

    [Fact]
    public void GenericMethod_InvocationWithinType_EmitsWithoutCreatingContainingType()
    {
        const string code = """
class Calculator {
    public static func Test<T>(value: T) -> T { value }

    public static func Run() -> int {
        Test(42)
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
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Calculator", throwOnError: true)!;
        var run = type.GetMethod("Run")!;

        var value = (int)run.Invoke(null, Array.Empty<object>())!;
        Assert.Equal(42, value);
    }

    [Fact]
    public void MethodGroup_ToObjectParameter_ForValueTypeDelegateArgument_UsesBridge()
    {
        const string code = """
import System.*
import System.Collections.Generic.*
import System.Linq.*

class Program {
    static var count: int = 0

    static func Capture(value: object) -> unit {
        count = count + 1
    }

    static func ForEach<T>(source: IEnumerable<T>, callback: T -> unit) -> unit {
        for item in source {
            callback(item)
        }
    }

    static func Run() -> int {
        val o = ["1", "2", "3"].ToDictionary(x => x, y => int.Parse(y))
        ForEach(o, Capture)
        count
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
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Program", throwOnError: true)!;
        var run = type.GetMethod("Run", BindingFlags.Public | BindingFlags.Static)!;

        var value = (int)run.Invoke(null, Array.Empty<object>())!;
        Assert.Equal(3, value);
    }
}
