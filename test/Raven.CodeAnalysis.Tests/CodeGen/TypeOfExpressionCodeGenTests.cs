using System;
using System.IO;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class TypeOfExpressionCodeGenTests
{
    [Fact]
    public void TypeOf_OpenGenericSourceType_ReturnsGenericTypeDefinition()
    {
        var code = """
class Box<T> { }

class Foo {
    public func Run() -> Type {
        return typeof(Box<>)
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
        var type = loaded.Assembly.GetType("Foo", true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Run")!;
        var value = (Type)method.Invoke(instance, Array.Empty<object>())!;

        Assert.True(value.IsGenericTypeDefinition);
        Assert.Equal("Box`1", value.Name);
    }
}
