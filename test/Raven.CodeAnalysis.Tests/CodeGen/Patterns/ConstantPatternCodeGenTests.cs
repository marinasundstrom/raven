using System;
using System.IO;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class ConstantPatternCodeGenTests
{
    [Theory]
    [InlineData(42L, true)]
    [InlineData(-42L, false)]
    [InlineData(4294967338L, false)]
    public void Int64SubjectMatchesInt32LiteralAtFullWidth(long input, bool expected)
    {
        var tree = SyntaxTree.ParseText("public static class Consumer { public static func Read(value: long) -> bool => value is 42 }");
        var compilation = Compilation.Create("WideConstant" + Guid.NewGuid().ToString("N"),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)).AddSyntaxTrees(tree).AddReferences(TestMetadataReferences.Default);
        using var stream = new MemoryStream();
        var emitted = compilation.Emit(stream);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, TestMetadataReferences.Default);
        Assert.Equal(expected, loaded.Assembly.GetType("Consumer")!.GetMethod("Read")!.Invoke(null, [input]));
    }

    [Fact]
    public void MetadataConstantMemberPattern_MatchesExactValue()
    {
        const string code = """
import System.*

class Program {
    public static func Run() -> bool {
        return Math.PI is Math.PI &&
            (2.0 is Math.PI) == false
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("metadata_constant_member_pattern", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
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
