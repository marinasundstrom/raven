using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class MethodReferenceCodeGenTests
{
    [Fact]
    public void MethodReference_StaticMethod_InvokesTarget()
    {
        const string code = """
class Calculator {
    public static Add(x: int, y: int) -> int { x + y }

    public static Compute() -> int {
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

    public Increment(delta: int) -> int { self.value + delta }

    public Run() -> int {
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
    public static TryAccumulate(var state: &int, out var doubled: &int) -> bool {
        state = 21
        doubled = 42
        true
    }

    public static Execute(value: int) -> int {
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

        var delegateType = assembly
            .GetTypes()
            .Single(type => type.Name.StartsWith("<>f__Delegate", StringComparison.Ordinal));

        Assert.True(typeof(MulticastDelegate).IsAssignableFrom(delegateType.BaseType));

        var invoke = delegateType.GetMethod("Invoke")!;
        Assert.Equal(typeof(bool), invoke.ReturnType);

        var parameters = invoke.GetParameters();
        Assert.Equal(2, parameters.Length);
        Assert.True(parameters[0].ParameterType.IsByRef);
        Assert.False(parameters[0].IsOut);
        Assert.Equal(typeof(int), parameters[0].ParameterType.GetElementType());
        Assert.True(parameters[1].ParameterType.IsByRef);
        Assert.False(parameters[1].IsOut);
        Assert.Equal(typeof(int), parameters[1].ParameterType.GetElementType());

        var ctor = delegateType.GetConstructors().Single();
        var ctorParameters = ctor.GetParameters();
        Assert.Equal(2, ctorParameters.Length);
        Assert.Equal(typeof(object), ctorParameters[0].ParameterType);
        Assert.Equal(typeof(IntPtr), ctorParameters[1].ParameterType);
    }

    [Fact]
    public void GenericMethod_InvocationWithinType_EmitsWithoutCreatingContainingType()
    {
        const string code = """
class Calculator {
    public static Test<T>(value: T) -> T { value }

    public static Run() -> int {
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
}
