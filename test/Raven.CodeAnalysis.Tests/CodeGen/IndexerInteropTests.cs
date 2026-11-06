using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class IndexerInteropTests
{
    [Fact]
    public void DotNet_IntegerIndexerCanBeInvokedThroughItsDefaultMember()
    {
        var tree = SyntaxTree.ParseText("""
            public class Box {
                public val self[index: int]: int { get => index + 1 }
            }
            """);
        var compilation = Compilation.Create("IntegerIndexer", [tree], TestMetadataReferences.Default,
            CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));

        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        var type = loaded.Assembly.GetType("Box", throwOnError: true)!;
        var indexer = Assert.IsAssignableFrom<PropertyInfo>(Assert.Single(type.GetDefaultMembers()));
        Assert.Equal(8, indexer.GetValue(Activator.CreateInstance(type), [7]));
    }

    [Theory]
    [InlineData("class")]
    [InlineData("struct")]
    public void DotNet_CSharpCanConsumeRavenIndexers(string kind)
    {
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0")).ToArray();
        var compilation = Compilation.Create("IndexerLibrary", [SyntaxTree.ParseText($$"""
            public {{kind}} Box {
                public val self[index: int]: int => index + 40
                public val self[index: string]: int => index.Length
            }
            """)], paths.Select(path => MetadataReference.CreateFromFile(path)).ToArray(),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        var consumer = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("IndexerConsumer",
            [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText("""
                public static class Consumer {
                    public static int Run(Box value) => value[2] + value["test"];
                }
                """)], paths.Select(path => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(path))
                .Append(Microsoft.CodeAnalysis.MetadataReference.CreateFromImage(image.ToArray())),
            new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
        using var consumerImage = new MemoryStream();
        var result = consumer.Emit(consumerImage);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        var type = loaded.Assembly.GetType("Box")!;
        Assert.Equal("Item", Assert.Single(type.GetCustomAttributes<DefaultMemberAttribute>()).MemberName);
        Assert.Equal(2, type.GetDefaultMembers().OfType<PropertyInfo>().Count());
    }
}
