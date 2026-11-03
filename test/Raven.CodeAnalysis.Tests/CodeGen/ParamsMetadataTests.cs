using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class ParamsMetadataTests
{
    [Theory]
    [InlineData("System.Collections.Generic.IEnumerable<int>", false)]
    [InlineData("System.Collections.Generic.IEnumerable<int>", true)]
    [InlineData("System.Collections.Generic.IList<int>", false)]
    [InlineData("System.Collections.Generic.IList<int>", true)]
    public void ImportedCollectionCollectorsSupportEmptyAndExpandedCalls(string collectionType, bool inspectSymbolsFirst)
    {
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0")).ToArray();
        var library = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("ImportedCollectors" + Guid.NewGuid().ToString("N"),
            [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText($$"""
                public class Imported {
                    public int Count { get; }
                    public Imported(params {{collectionType}} values) { Count = System.Linq.Enumerable.Count(values); }
                    public static int Read(params {{collectionType}} values) => System.Linq.Enumerable.Count(values);
                    public static int Ordinary({{collectionType}} values) => System.Linq.Enumerable.Count(values);
                }
                """)],
            paths.Select(path => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(path)),
            new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
        using var libraryImage = new MemoryStream();
        var libraryResult = library.Emit(libraryImage);
        Assert.True(libraryResult.Success, string.Join(Environment.NewLine, libraryResult.Diagnostics));
        var tree = SyntaxTree.ParseText("""
            public class Consumer {
                static func Run() -> int => Imported.Read() + Imported.Read(1, 2) + Imported().Count + Imported(3, 4, 5).Count
            }
            """);
        var compilation = Compilation.Create("ImportedCollectionConsumer", [tree],
            paths.Select(path => MetadataReference.CreateFromFile(path))
                .Append(MetadataReference.CreateFromImage(libraryImage.ToArray())).ToArray(),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        if (inspectSymbolsFirst)
        {
            var imported = compilation.GetTypeByMetadataName("Imported")!;
            Assert.True(Assert.Single(imported.GetMembers("Read").OfType<IMethodSymbol>()).Parameters[0].IsVarParams);
            Assert.False(Assert.Single(imported.GetMembers("Ordinary").OfType<IMethodSymbol>()).Parameters[0].IsVarParams);
            Assert.True(Assert.Single(imported.Constructors).Parameters[0].IsVarParams);
        }
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(5, loaded.Assembly.GetType("Consumer")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Theory]
    [InlineData("System.Collections.Generic.IEnumerable<int>")]
    [InlineData("System.Collections.Generic.IList<int>")]
    public void CollectionCollectorsAreConsumableFromCSharp(string collectionType)
    {
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0")).ToArray();
        var tree = SyntaxTree.ParseText($$"""
            public delegate Collect(params values: {{collectionType}}) -> int
            public class Collector {
                init(params values: {{collectionType}}) {}
                static func Count(params values: {{collectionType}}) -> int => 42
            }
            """);
        var compilation = Compilation.Create("CollectionParams", [tree],
            paths.Select(path => MetadataReference.CreateFromFile(path)).ToArray(),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        var consumer = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("CollectionParamsConsumer",
            [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText("""
                public static class Consumer {
                    public static int Run() {
                        _ = new Collector();
                        _ = new Collector(1, 2);
                        Collect collect = Collector.Count;
                        return Collector.Count() + Collector.Count(1, 2) + collect(3, 4);
                    }
                }
                """)],
            paths.Select(path => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(path))
                .Append(Microsoft.CodeAnalysis.MetadataReference.CreateFromImage(image.ToArray())),
            new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
        using var consumerImage = new MemoryStream();
        var consumed = consumer.Emit(consumerImage);
        Assert.True(consumed.Success, string.Join(Environment.NewLine, consumed.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        var emitted = loaded.Assembly.GetType("Collector")!;
        Assert.Contains(emitted.GetMethod("Count")!.GetParameters()[0].GetCustomAttributesData(),
            attribute => attribute.AttributeType.FullName == "System.Runtime.CompilerServices.ParamCollectionAttribute");
        consumerImage.Position = 0;
        var consumerAssembly = System.Runtime.Loader.AssemblyLoadContext.GetLoadContext(loaded.Assembly)!.LoadFromStream(consumerImage);
        Assert.Equal(126, consumerAssembly.GetType("Consumer")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Theory]
    [InlineData("int[]?", true)]
    [InlineData("System.Collections.Generic.IEnumerable<int>?", false)]
    [InlineData("System.Collections.Generic.IList<int>?", false)]
    public void NullableCollectors_PreserveNullAndPackArguments(string collectionType, bool array)
    {
        var tree = SyntaxTree.ParseText($$"""
            import System.Linq.*
            public class Collector {
                static func Count(params values: {{collectionType}}) -> int {
                    if values is null { return -1 }
                    return values!.Count()
                }
                static func Empty() -> int => Count()
                static func Packed() -> int => Count(1, 2)
                static func Absent() -> int => Count(null)
            }
            """);
        var compilation = Compilation.Create("NullableParams", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        var type = loaded.Assembly.GetType("Collector")!;
        Assert.Equal(array, type.GetMethod("Count")!.GetParameters()[0].GetCustomAttribute<ParamArrayAttribute>() is not null);
        Assert.Equal(0, type.GetMethod("Empty")!.Invoke(null, null));
        Assert.Equal(2, type.GetMethod("Packed")!.Invoke(null, null));
        Assert.Equal(-1, type.GetMethod("Absent")!.Invoke(null, null));
    }

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
