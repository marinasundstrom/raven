using System;
using System.IO;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class MethodOverloadCodeGenTests
{
    [Fact]
    public void DifferentGenericArities_EmitDistinctCallableMethods()
    {
        const string code = """
class Reader {
    static func Read(text: string) -> int => 1
    static func Read<T>(text: string) -> int => 2
    static func Read<T, U>(text: string) -> int => 3
    static func Run() -> int => Read("x") + Read<int>("x") * 10 + Read<int, string>("x") * 100
}
""";
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("generic_arity_codegen", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(SyntaxTree.ParseText(code))
            .AddReferences(references);
        var tree = compilation.SyntaxTrees.Single();
        var model = compilation.GetSemanticModel(tree);
        var calls = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>()
            .Select(node => Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(node).Symbol));
        Assert.Equal(new[] { 0, 1, 2 }, calls.Select(method => method.Arity));
        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("Reader", throwOnError: true)!;
        var typed = System.Linq.Enumerable.Single(type.GetMethods(), method => method.Name == "Read" && method.GetGenericArguments().Length == 1);
        Assert.Equal(2, typed.MakeGenericMethod(typeof(int)).Invoke(null, new object[] { "x" }));
        var run = type.GetMethod("Run", BindingFlags.Public | BindingFlags.Static)!;
        Assert.Equal(321, run.Invoke(null, null));
    }

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
