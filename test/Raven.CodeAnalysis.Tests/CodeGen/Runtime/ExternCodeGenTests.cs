using System;
using System.IO;
using System.Reflection;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class ExternCodeGenTests
{
    [Fact]
    public void ExternDllImportMethod_EmitsPInvokeImpl()
    {
        const string source = """
import System.Runtime.InteropServices.*

class Native {
    [DllImport("kernel32", EntryPoint: "GetTickCount")]
    public extern static GetTickCount() -> uint;
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("native", [tree], new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var nativeType = loaded.Assembly.GetType("Native", throwOnError: true)!;
        var method = nativeType.GetMethod("GetTickCount", BindingFlags.Public | BindingFlags.Static);
        Assert.NotNull(method);
        Assert.True(method!.Attributes.HasFlag(MethodAttributes.PinvokeImpl));
    }
}
