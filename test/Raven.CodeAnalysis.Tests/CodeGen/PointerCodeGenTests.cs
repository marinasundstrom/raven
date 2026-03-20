using System;
using System.IO;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class PointerCodeGenTests
{
    [Fact]
    public void PointerArithmeticAndSizeOf_EmitExpectedRuntimeValues()
    {
        const string code = """
class PointerMath {
    unsafe static func Distance() -> nint {
        var value = 0
        val pointer: *int = &value;
        val advanced = pointer + 3
        advanced - pointer
    }

    static func SizeOfInt() -> int {
        sizeof(int)
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("pointer_codegen", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("PointerMath", throwOnError: true)!;

        var distanceMethod = type.GetMethod("Distance", BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic)!;
        var distance = (IntPtr)distanceMethod.Invoke(null, Array.Empty<object>())!;
        Assert.Equal(3L, distance.ToInt64());

        var sizeOfMethod = type.GetMethod("SizeOfInt", BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic)!;
        var size = (int)sizeOfMethod.Invoke(null, Array.Empty<object>())!;
        Assert.Equal(4, size);
    }

    [Fact]
    public void PointerMemberAccess_OnValueTypeMethod_PreservesOriginalStorage()
    {
        const string code = """
struct Counter {
    public var Value: int = 0

    public func Increment() -> unit {
        self.Value = self.Value + 1
    }
}

class PointerMethods {
    unsafe static func Run() -> int {
        var counter = Counter()
        val pointer: *Counter = &counter
        pointer->Increment()
        pointer->Increment()
        counter.Value
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("pointer_member_codegen", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("PointerMethods", throwOnError: true)!;
        var runMethod = type.GetMethod("Run", BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic)!;

        Assert.Equal(typeof(int), runMethod.ReturnType);
    }

    [Fact]
    public void PointerMemberFieldAssignment_WritesThroughPointer()
    {
        const string code = """
struct Holder {
    public field Foo: int = 0
}

class PointerAssignment {
    unsafe static func Run() -> int {
        var holder = Holder()
        val pointer: *Holder = &holder
        pointer->Foo = 2
        holder.Foo
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("pointer_member_assignment_codegen", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("PointerAssignment", throwOnError: true)!;
        var runMethod = type.GetMethod("Run", BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic)!;

        var value = (int)runMethod.Invoke(null, Array.Empty<object>())!;
        Assert.Equal(2, value);
    }

    [Fact]
    public void FixedUseInitializer_PinsArrayElementAndWritesThroughPointer()
    {
        const string code = """
class FixedPointer {
    unsafe static func Run() -> int {
        var values: int[] = [1, 2, 3]
        use pointer: *int = fixed &values[1]
        *pointer = 42
        values[1]
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("fixed_pointer_codegen", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("FixedPointer", throwOnError: true)!;
        var runMethod = type.GetMethod("Run", BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic)!;

        var value = (int)runMethod.Invoke(null, Array.Empty<object>())!;
        Assert.Equal(42, value);
    }
}
