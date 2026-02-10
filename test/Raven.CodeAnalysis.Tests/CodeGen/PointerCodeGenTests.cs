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
    unsafe static Distance() -> nint {
        var value = 0
        val pointer: *int = &value;
        val advanced = pointer + 3
        advanced - pointer
    }

    static SizeOfInt() -> int {
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
}
