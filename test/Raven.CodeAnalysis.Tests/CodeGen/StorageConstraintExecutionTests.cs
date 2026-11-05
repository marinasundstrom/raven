using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class StorageConstraintExecutionTests
{
    [Theory]
    [InlineData(0, 1, 2)]
    [InlineData(0, 2, 1)]
    [InlineData(1, 0, 2)]
    [InlineData(1, 2, 0)]
    [InlineData(2, 0, 1)]
    [InlineData(2, 1, 0)]
    [Trait("Target", "DotNet")]
    public void CrossFileConstrainedPropertyParticipatesInUpdate(int first, int second, int third)
    {
        string[] sources =
        [
            """
            public open class Node {}
            public struct Items<T> where T: Node {
                readonly field Value: int

                init(value: int) { Value = value }
            }
            """,
            """
            public class Container {
                private field _items: Items<Element>

                init(items: Items<Element>) { _items = items }

                val Values: Items<Element> { get => _items }

                func Update(items: Items<Element>) -> Container {
                    if Values.Value != items.Value { return Container(items) }
                    return self
                }

                static func Run() -> int {
                    let original = Container(Items<Element>(1))
                    return original.Update(Items<Element>(2)).Values.Value
                }
            }
            """,
            "public class Element: Node {}"
        ];
        var trees = new[] { first, second, third }
            .Select(index => SyntaxTree.ParseText(sources[index], path: $"{index}.rvn"))
            .ToArray();
        var compilation = Compilation.Create(
            "StorageExecution",
            trees,
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(2, loaded.Assembly.GetType("Container")!.GetMethod("Run")!.Invoke(null, null));
    }
}
