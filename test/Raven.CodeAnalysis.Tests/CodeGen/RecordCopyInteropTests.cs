using System.Runtime.Loader;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class RecordCopyInteropTests
{
    [Fact]
    public void CSharpAndRavenCopiesPreserveRuntimeRecordType()
    {
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0")).ToArray();
        var tree = SyntaxTree.ParseText("""
            public open record Entity(Id: int)
            public record Item(Id: int, Name: string) : Entity(Id)
            public record Box<T>(Value: T)
            public static class RavenCopy {
                static func Run() -> bool {
                    let original: Entity = Item(1, "kept")
                    let changed = original with { Id = 2 }
                    return changed is Item item && item.Id == 2 && item.Name == "kept" && original.Id == 1
                }
            }
            """);
        var compilation = Compilation.Create("RecordCopyLibrary", [tree],
            paths.Select(path => MetadataReference.CreateFromFile(path)).ToArray(),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        var consumer = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("RecordCopyConsumer",
            [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText("""
                public static class Consumer {
                    public static bool Run() {
                        Entity original = new Item(1, "kept");
                        Entity changed = original with { Id = 2 };
                        var specific = new Item(5, "before");
                        var specificCopy = specific with { Name = "after" };
                        var box = new Box<int>(3);
                        var copiedBox = box with { Value = 4 };
                        return changed is Item item && item.Id == 2 && item.Name == "kept"
                            && original.Id == 1 && box.Value == 3 && copiedBox.Value == 4
                            && specificCopy.Id == 5 && specificCopy.Name == "after" && specific.Name == "before"
                            && !object.ReferenceEquals(original, changed);
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
        Assert.Equal(true, loaded.Assembly.GetType("RavenCopy")!.GetMethod("Run")!.Invoke(null, null));
        consumerImage.Position = 0;
        var consumerAssembly = AssemblyLoadContext.GetLoadContext(loaded.Assembly)!.LoadFromStream(consumerImage);
        Assert.Equal(true, consumerAssembly.GetType("Consumer")!.GetMethod("Run")!.Invoke(null, null));
    }
}
