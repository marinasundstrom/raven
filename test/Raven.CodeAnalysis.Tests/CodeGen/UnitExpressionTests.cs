using System.IO;
using System.Reflection;
using System.Reflection.Emit;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class UnitExpressionTests
{
    [Fact]
    public void UnitLiteral_AsExpressionStatement_DoesNotEmitValue()
    {
        var code = """
class Foo {
    Test() -> void {
        unit;
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success);

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var method = loaded.Assembly.GetType("Foo")!.GetMethod("Test")!;
        var il = method.GetMethodBody()!.GetILAsByteArray();

        Assert.DoesNotContain(il, b => b == OpCodes.Ldsfld.Value);
        Assert.DoesNotContain(il, b => b == OpCodes.Pop.Value);
    }
}
