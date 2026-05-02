using System;
using System.IO;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.CodeGen.Functions.Parameters;

public sealed class ByRefParameterCodeGenTests
{
    [Fact]
    public void SourceOutParameter_WithDeclaredVarLocal_EmitsCallAndAssignedLocal()
    {
        const string code = """
class Program {
    static func TryParse(text: string, out result: int) -> bool {
        result = 41
        return true
    }

    public static func Run() -> int {
        if !TryParse("value", out var total) {
            return -1
        }

        return total + 1
    }
}
""";

        var value = CompileAndInvokeRun<int>(code, "source-out-var-runtime");

        Assert.Equal(42, value);
    }

    [Fact]
    public void MetadataOutParameter_WithDeclaredVarLocal_EmitsCallAndAssignedLocal()
    {
        const string code = """
import System.Text.Json.*

class Program {
    public static func Run() -> int {
        use doc = JsonDocument.Parse("{ \"value\": 42 }")
        val root = doc.RootElement

        if !root.TryGetProperty("value", out var property) {
            return -1
        }

        return property.GetInt32()
    }
}
""";

        var value = CompileAndInvokeRun<int>(code, "metadata-out-var-runtime");

        Assert.Equal(42, value);
    }

    private static T CompileAndInvokeRun<T>(string code, string assemblyName)
    {
        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(assemblyName, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var type = loaded.Assembly.GetType("Program", throwOnError: true)!;
        var run = type.GetMethod("Run", BindingFlags.Public | BindingFlags.Static)!;
        return (T)run.Invoke(null, Array.Empty<object>())!;
    }
}
