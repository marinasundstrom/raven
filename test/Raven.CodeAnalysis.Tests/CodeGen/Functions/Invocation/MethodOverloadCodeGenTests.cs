using System;
using System.IO;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class MethodOverloadCodeGenTests
{
    [Fact]
    public void InterfaceImplementation_WithDiscardParameters_EmitsAndCanBeInvoked()
    {
        const string code = """
interface IHandler {
    func Handle(value: int, context: string) -> int
}

class Handler: IHandler {
    public func Handle(_: int, _: string) -> int => 42
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("discard_parameter_codegen", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("Handler", throwOnError: true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Handle", BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance);
        Assert.NotNull(method);

        var value = (int)method!.Invoke(instance, [1, "ignored"])!;
        Assert.Equal(42, value);
    }

    [Fact]
    public void ParamsArrayArgument_WithNullableArrayTarget_EmitsCollectionLiteral()
    {
        const string code = """
import System.*

class Widget {
    init(value: int) {
        Value = value
    }

    val Value: int
}

class Runner {
    static func Run() -> int {
        let type = typeof(Widget)
        let value: object = 42
        let created = Activator.CreateInstance(type, [value])
        let property = type.GetProperty("Value") ?? throw InvalidOperationException("missing property")
        let result = property.GetValue(created)
        Convert.ToInt32(result)
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("method_overload_codegen", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("Runner", throwOnError: true)!;
        var method = type.GetMethod("Run", BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static);
        Assert.NotNull(method);

        var value = (int)method!.Invoke(null, Array.Empty<object>())!;
        Assert.Equal(42, value);
    }
}
