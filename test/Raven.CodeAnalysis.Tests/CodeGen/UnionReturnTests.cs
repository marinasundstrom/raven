using System;
using System.IO;
using System.Reflection;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public class UnionReturnTests
{
    [Fact]
    public void ValueTypesInUnionAreBoxedWhenReturning()
    {
        var code = """
class Foo {
    Test(flag: bool) -> int | () {
        if flag {
            return 42
        } else {
            return ()
        }
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success);

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Foo", true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Test")!;
        var intResult = method.Invoke(instance, new object[] { true });
        Assert.Equal(42, (int)intResult!);

        var unitResult = method.Invoke(instance, new object[] { false });
        Assert.NotNull(unitResult);
        Assert.Equal("Unit", unitResult!.GetType().Name);
        Assert.Equal("()", unitResult.ToString());
    }
    [Fact]
    public void FinalExpressionUnitIsBoxed()
    {
        var code = """
class Foo {
    Test(flag: bool) -> int | () {
        if flag {
            return 42
        }
        ()
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success);

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Foo", true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Test")!;
        var intResult = method.Invoke(instance, new object[] { true });
        Assert.Equal(42, (int)intResult!);

        var unitResult = method.Invoke(instance, new object[] { false });
        Assert.NotNull(unitResult);
        Assert.Equal("Unit", unitResult!.GetType().Name);
        Assert.Equal("()", unitResult.ToString());
    }

}
