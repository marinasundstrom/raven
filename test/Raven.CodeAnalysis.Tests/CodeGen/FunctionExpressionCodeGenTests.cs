using System;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.CompilerServices;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class FunctionExpressionCodeGenTests
{
    [Fact]
    public void Lambda_ExpressionBody_ReturnsSum()
    {
        var code = """
class Calculator {
    func Add() -> int {
        val add = (x: int, y: int) -> int => x + y
        return add(2, 3)
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
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Add", BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic)!;

        var value = (int)method.Invoke(instance, Array.Empty<object>())!;
        Assert.Equal(5, value);
    }

    [Fact]
    public void Lambda_ComparisonExpression_ReturnsExpectedResults()
    {
        var code = """
class Checker {
    func AreEqual(left: int, right: int) -> bool {
        val equals = (x: int, y: int) -> bool => x == y
        return equals(left, right)
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
        var type = assembly.GetType("Checker", throwOnError: true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("AreEqual", BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic)!;

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
    func Sum() -> int {
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

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Calculator", throwOnError: true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Sum", BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic)!;

        var value = (int)method.Invoke(instance, Array.Empty<object>())!;
        Assert.Equal(10, value);
    }

    [Fact]
    public void Lambda_BlockBody_WithExplicitReturn_ReturnsComputedValue()
    {
        var code = """
class Calculator {
    func Sum() -> int {
        val make = (x: int, y: int) -> int => {
            return x + y;
        }

        return make(4, 6)
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
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Sum", BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic)!;

        var value = (int)method.Invoke(instance, Array.Empty<object>())!;
        Assert.Equal(10, value);
    }

    [Fact]
    public void Lambda_CapturesParameter_ReturnsExpectedResult()
    {
        var code = """
class Calculator {
    func Combine(x: int) -> int {
        val add = (y: int) -> int => x + y
        return add(4)
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
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Combine", BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic)!;

        var value = (int)method.Invoke(instance, new object[] { 6 })!;
        Assert.Equal(10, value);
    }

    [Fact]
    public void Lambda_CapturesLocal_ReturnsValue()
    {
        var code = """
class Counter {
    func Multiply() -> int {
        val factor = 5
        val multiply = (value: int) -> int => factor * value
        return multiply(3)
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
        var method = type.GetMethod("Multiply", BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic)!;

        var value = (int)method.Invoke(instance, Array.Empty<object>())!;
        Assert.Equal(15, value);
    }

    // Known bug: self is not detected as a captured variable, so the lambda is
    // incorrectly emitted as a static method, which produces RAV2801.
    // The correct result is 15 (self.value=8, offset=7). Tracked for fixing.
    [Fact]
    public void Lambda_CapturesSelfField_UsesInstanceState()
    {
        var code = """
class Holder {
    var value: int

    func Compute() -> int {
        self.value = 8
        val add = (offset: int) -> int => self.value + offset
        return add(7)
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
        var type = assembly.GetType("Holder", throwOnError: true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Compute", BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic)!;

        var value = (int)method.Invoke(instance, Array.Empty<object>())!;
        Assert.Equal(15, value); // self.value(8) + offset(7)
    }

    [Fact]
    public void Lambda_BlockBody_NullCoalesceReturnExpression_ReturnsFromLambda()
    {
        var code = """
class Handler {
    func Compute(input: string?) -> int {
        val lengthOrNegativeOne = (text: string?) -> int => {
            val required = text ?? return -1
            return required.Length
        }

        return lengthOrNegativeOne(input)
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
        var type = assembly.GetType("Handler", throwOnError: true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Compute", BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic)!;

        var nullResult = (int)method.Invoke(instance, new object?[] { null })!;
        var valueResult = (int)method.Invoke(instance, new object?[] { "abcd" })!;

        Assert.Equal(-1, nullResult);
        Assert.Equal(4, valueResult);
    }

    [Fact]
    public void Lambda_BlockBody_NullCoalesceThrowExpression_ThrowsFromLambda()
    {
        var code = """
class Handler {
    func Compute(input: string?) -> int {
        val lengthOrThrow = (text: string?) -> int => {
            val required = text ?? throw System.InvalidOperationException("missing")
            return required.Length
        }

        return lengthOrThrow(input)
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
        var type = assembly.GetType("Handler", throwOnError: true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Compute", BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic)!;

        var thrown = Assert.Throws<TargetInvocationException>(() => method.Invoke(instance, new object?[] { null }));
        var invalidOperation = Assert.IsType<InvalidOperationException>(thrown.InnerException);
        Assert.Equal("missing", invalidOperation.Message);
    }

    // ─── DisplayClass structure ───────────────────────────────────────────────────

    [Fact]
    public void Lambda_WithoutCaptures_GeneratesDisplayClass()
    {
        // C#-aligned closure lowering emits non-capturing lambdas as instance methods
        // on a compiler-generated closure carrier type.
        var code = """
class Pure {
    func Run() -> int {
        val add = (x: int, y: int) -> int => x + y
        return add(3, 4)
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
        var type = loaded.Assembly.GetType("Pure", throwOnError: true)!;

        var displayClasses = type.GetNestedTypes(BindingFlags.NonPublic)
            .Where(t => t.Name.Contains("DisplayClass"))
            .ToArray();

        Assert.Single(displayClasses);

        var lambdaMethods = displayClasses[0]
            .GetMethods(BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic)
            .Where(m => m.Name.Contains(">b__", StringComparison.Ordinal))
            .ToArray();
        Assert.NotEmpty(lambdaMethods);
        Assert.All(lambdaMethods, method => Assert.False(method.IsStatic));

        // Correctness check: the lambda still executes.
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Run", BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic)!;
        Assert.Equal(7, (int)method.Invoke(instance, Array.Empty<object>())!);
    }

    [Fact]
    public void MultipleLambdas_CapturingSameScope_ShareSingleDisplayClass()
    {
        // When multiple lambdas in the same method capture locals, only one
        // DisplayClass should be generated — all lambdas must share it.
        var code = """
class Pair {
    func Run() -> int {
        var shared = 0
        val inc = () -> unit => { shared = shared + 10 }
        val dec = () -> unit => { shared = shared - 3 }
        inc()
        dec()
        inc()
        return shared
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
        var type = loaded.Assembly.GetType("Pair", throwOnError: true)!;

        var displayClasses = type.GetNestedTypes(BindingFlags.NonPublic)
            .Where(t => t.Name.Contains("DisplayClass"))
            .ToArray();

        // Exactly one DisplayClass for the whole method scope.
        Assert.Single(displayClasses);

        var lambdaMethodNames = displayClasses[0]
            .GetMethods(BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic)
            .Select(m => m.Name)
            .Where(static n => n.Contains(">b__", StringComparison.Ordinal))
            .ToArray();

        Assert.Contains(lambdaMethodNames, static name => name.Contains(">b__0", StringComparison.Ordinal));
        Assert.Contains(lambdaMethodNames, static name => name.Contains(">b__1", StringComparison.Ordinal));

        // Correctness check: shared reference semantics (0 + 10 - 3 + 10 = 17).
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Run", BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic)!;
        Assert.Equal(17, (int)method.Invoke(instance, Array.Empty<object>())!);
    }

    [Fact]
    public void Lambda_CapturesMultipleLocals_AllLiftedintoOneDisplayClass()
    {
        // All captured locals in the same scope must share a single DisplayClass,
        // regardless of how many distinct variables are captured.
        var code = """
class Multi {
    func Run() -> int {
        var a = 1
        var b = 10
        var c = 100
        val sum = () -> int => a + b + c
        a = 2
        b = 20
        c = 200
        return sum()
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
        var type = loaded.Assembly.GetType("Multi", throwOnError: true)!;

        // Still exactly one DisplayClass even for three captured locals.
        var displayClasses = type.GetNestedTypes(BindingFlags.NonPublic)
            .Where(t => t.Name.Contains("DisplayClass"))
            .ToArray();
        Assert.Single(displayClasses);

        // Reference semantics: lambda sees the post-assignment values (2+20+200 = 222).
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Run", BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic)!;
        Assert.Equal(222, (int)method.Invoke(instance, Array.Empty<object>())!);
    }

    // ─── Reference-based capture (C#-style variable hoisting) ────────────────────

    [Fact]
    public void Lambda_MutableCapture_LambdaWriteReflectsInOuterMethod()
    {
        // The lambda writes to the captured local; the outer method reads the updated value.
        var code = """
class Counter {
    func CountItems() -> int {
        var count = 0
        val increment = () -> unit => { count = count + 1 }
        increment()
        increment()
        increment()
        return count
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
        var type = loaded.Assembly.GetType("Counter", throwOnError: true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("CountItems", BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic)!;

        Assert.Equal(3, (int)method.Invoke(instance, Array.Empty<object>())!);
    }

    [Fact]
    public void Lambda_MutableCapture_OuterWriteReflectsInLambda()
    {
        // The outer method writes to the captured local; the lambda reads the updated value.
        var code = """
class Spy {
    func Run() -> int {
        var value = 0
        val read = () -> int => value
        value = 42
        return read()
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
        var type = loaded.Assembly.GetType("Spy", throwOnError: true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Run", BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic)!;

        Assert.Equal(42, (int)method.Invoke(instance, Array.Empty<object>())!);
    }

    [Fact]
    public void Lambda_MultipleLambdas_SharedCapturedLocal()
    {
        // Two lambdas sharing the same captured local: writes from one are visible to the other.
        var code = """
class Pair {
    func Run() -> int {
        var shared = 0
        val inc = () -> unit => { shared = shared + 10 }
        val dec = () -> unit => { shared = shared - 3 }
        inc()
        dec()
        inc()
        return shared
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
        var type = loaded.Assembly.GetType("Pair", throwOnError: true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Run", BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic)!;

        // 0 + 10 - 3 + 10 = 17
        Assert.Equal(17, (int)method.Invoke(instance, Array.Empty<object>())!);
    }

    // ─── Local functions sharing DisplayClass with lambdas ───────────────────────

    [Fact]
    public void LocalFunction_AndLambda_ShareSingleDisplayClass()
    {
        // A local function and a lambda in the same method both capture the same local.
        // They must share a single DisplayClass (one DisplayClass per method scope).
        var code = """
class Counter {
    func Run() -> int {
        var count = 0

        func Increment() {
            count = count + 1
        }

        val read = () -> int => count

        Increment()
        Increment()
        Increment()
        return read()
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
        var type = loaded.Assembly.GetType("Counter", throwOnError: true)!;

        // Exactly one shared DisplayClass for the whole method scope.
        var displayClasses = type.GetNestedTypes(BindingFlags.NonPublic)
            .Where(t => t.Name.Contains("DisplayClass"))
            .ToArray();
        Assert.Single(displayClasses);

        // Correctness: local function and lambda share the same closure, so
        // writes from the local function are visible through the lambda.
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Run", BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic)!;
        Assert.Equal(3, (int)method.Invoke(instance, Array.Empty<object>())!);
    }

    [Fact]
    public void LocalFunction_OnlyCaptures_SingleDisplayClass()
    {
        // A method with only a local function (no lambdas) that captures a local
        // should still generate exactly one DisplayClass.
        var code = """
class Counter {
    func Run() -> int {
        var x = 10

        func Double() {
            x = x * 2
        }

        Double()
        Double()
        return x
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
        var type = loaded.Assembly.GetType("Counter", throwOnError: true)!;

        var displayClasses = type.GetNestedTypes(BindingFlags.NonPublic)
            .Where(t => t.Name.Contains("DisplayClass"))
            .ToArray();
        Assert.Single(displayClasses);

        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Run", BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic)!;
        Assert.Equal(40, (int)method.Invoke(instance, Array.Empty<object>())!);
    }

}
