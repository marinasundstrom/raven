using System;
using System.IO;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class EnumCodeGenTests
{
    [Fact]
    public void ByteBackedMetadataEnumMember_EmitsAsConstantOperand()
    {
        const string code = """
import System.Text.Json.*

class Program {
    public static func Run() -> bool {
        return JsonValueKind.Null == JsonValueKind.Null
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("metadata_byte_enum_constant", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var type = loaded.Assembly.GetType("Program", throwOnError: true)!;
        var run = type.GetMethod("Run", BindingFlags.Public | BindingFlags.Static)!;
        var value = (bool)run.Invoke(null, Array.Empty<object>())!;

        Assert.True(value);
    }
}
