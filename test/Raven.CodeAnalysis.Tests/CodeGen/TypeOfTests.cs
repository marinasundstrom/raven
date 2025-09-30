using System;
using System.IO;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class TypeOfTests
{
    [Fact]
    public void TypeOfExpression_EmitsSystemType()
    {
        const string code = """
class TypeInspector {
    static GetIntType() -> System.Type {
        typeof(int)
    }

    static GetListType() -> System.Type {
        typeof(System.Collections.Generic.List<int>)
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
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("TypeInspector", throwOnError: true)!;

        var getIntType = type.GetMethod("GetIntType")!;
        var getListType = type.GetMethod("GetListType")!;

        var intType = (Type)getIntType.Invoke(null, Array.Empty<object>())!;
        var listType = (Type)getListType.Invoke(null, Array.Empty<object>())!;

        Assert.Equal(typeof(int), intType);
        Assert.Equal(typeof(System.Collections.Generic.List<int>), listType);
    }

    [Fact]
    public void TypeOfExpression_AllowsOpenGenericTypes()
    {
        const string code = """
class OpenGenericInspector {
    static GetFuncArity1() -> System.Type {
        typeof(System.Func<>)
    }

    static GetFuncArity2() -> System.Type {
        typeof(System.Func<,>)
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
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("OpenGenericInspector", throwOnError: true)!;

        var getFunc1 = type.GetMethod("GetFuncArity1")!;
        var getFunc2 = type.GetMethod("GetFuncArity2")!;

        var func1 = (Type)getFunc1.Invoke(null, Array.Empty<object>())!;
        var func2 = (Type)getFunc2.Invoke(null, Array.Empty<object>())!;

        Assert.Equal(typeof(System.Func<>), func1);
        Assert.Equal(typeof(System.Func<,>), func2);
    }
}
