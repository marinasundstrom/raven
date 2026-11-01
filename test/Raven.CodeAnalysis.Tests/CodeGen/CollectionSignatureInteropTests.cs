using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class CollectionSignatureInteropTests
{
    [Fact]
    public void DotNet_CollectionSignaturesCanBeConsumedByCSharpUsingReferenceAssemblies()
    {
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0")).ToArray();
        var tree = SyntaxTree.ParseText("""
            import System.Collections.Generic.*
            public static class Collections {
                public static func Create() -> List<int> {
                    let values = List<int>()
                    values.Add(42)
                    return values
                }
                public static func Read(values: IEnumerable<int>) -> IEnumerator<int> => values.GetEnumerator()
            }
            """);
        var compilation = Compilation.Create("CollectionLibrary", [tree],
            paths.Select(path => MetadataReference.CreateFromFile(path)).ToArray(),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        var consumer = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("CollectionConsumer",
            [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText("""
                public static class Consumer {
                    public static int Run() {
                        System.Collections.Generic.List<int> values = Collections.Create();
                        using var iterator = Collections.Read(values);
                        return iterator.MoveNext() ? iterator.Current : -1;
                    }
                }
                """)],
            paths.Select(path => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(path))
                .Append(Microsoft.CodeAnalysis.MetadataReference.CreateFromImage(image.ToArray())),
            new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
        using var consumerImage = new MemoryStream();
        var consumerResult = consumer.Emit(consumerImage);
        Assert.True(consumerResult.Success, string.Join(Environment.NewLine, consumerResult.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(new[] { 42 }, (List<int>)loaded.Assembly.GetType("Collections")!.GetMethod("Create")!.Invoke(null, null)!);
    }
}
