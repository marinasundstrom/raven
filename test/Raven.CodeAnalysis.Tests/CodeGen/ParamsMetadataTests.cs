using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class ParamsMetadataTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ArrayCollectorsExposeParamArrayMetadata(bool nestedDelegate)
    {
        const string declaration = "public delegate Collect(params values: object?[]) -> int";
        var tree = SyntaxTree.ParseText($$"""
            {{(nestedDelegate ? "" : declaration)}}
            public class Collector {
                {{(nestedDelegate ? declaration : "")}}
                init(params values: object?[]) {}
                static func Count(params values: object?[]) -> int => values.Length
                static func List(items: int ...) -> int => items.Count
                static func Ordinary(values: object?[]) -> int => values.Length
            }
            """);
        var compilation = Compilation.Create("ParamsMetadata", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var type = compilation.GetTypeByMetadataName("Collector")!;
        var delegateName = nestedDelegate ? "Collector+Collect" : "Collect";
        var delegateType = compilation.GetTypeByMetadataName(delegateName)!;
        var invokes = delegateType.GetMembers("Invoke").OfType<IMethodSymbol>().ToArray();
        Assert.True(invokes.Length == 1, string.Join("; ", invokes.Select(method =>
            $"{method.ToDisplayString()} collector={method.Parameters[0].IsVarParams} syntax={method.DeclaringSyntaxReferences.Length}")));
        Assert.True(invokes[0].Parameters[0].IsVarParams);
        var count = Assert.Single(type.GetMembers("Count").OfType<IMethodSymbol>());
        Assert.True(Assert.Single(count.Parameters).IsVarParams);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        var emitted = loaded.Assembly.GetType("Collector")!;
        Assert.NotNull(emitted.GetMethod("Count")!.GetParameters()[0].GetCustomAttribute<ParamArrayAttribute>());
        Assert.NotNull(Assert.Single(emitted.GetConstructors()).GetParameters()[0].GetCustomAttribute<ParamArrayAttribute>());
        Assert.NotNull(loaded.Assembly.GetType(delegateName)!.GetMethod("Invoke")!.GetParameters()[0].GetCustomAttribute<ParamArrayAttribute>());
        Assert.Null(emitted.GetMethod("List")!.GetParameters()[0].GetCustomAttribute<ParamArrayAttribute>());
        Assert.Null(emitted.GetMethod("Ordinary")!.GetParameters()[0].GetCustomAttribute<ParamArrayAttribute>());
        Assert.Equal(2, emitted.GetMethod("Count")!.Invoke(null, [new object?[] { 1, null }]));
    }
}
