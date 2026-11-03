using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class SourceGenericPropertyPatternTests
{
    [Theory]
    [InlineData("null", false)]
    [InlineData("List<Item>()", false)]
    [InlineData("List<Item>([Item()])", true)]
    public void NullableCollectionOfSourceTypeMatchesCount(string value, bool expected)
    {
        var tree = SyntaxTree.ParseText($$"""
            import System.Collections.Generic.*
            public class Item {}
            public class Probe {
                static func HasItems(value: List<Item>?) -> bool => value is { Count: > 0 }
                static func Run() -> bool => HasItems({{value}})
            }
            """);
        var compilation = Compilation.Create("SourceGenericPropertyPattern", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(expected, loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Theory]
    [InlineData("value is { Count: 3 }", true)]
    [InlineData("value is { Count: 4 }", false)]
    [InlineData("value is { Label.Length: 3 }", true)]
    public void PropertyPatternFindsInheritedSourceMembers(string pattern, bool expected)
    {
        AssertPatternResult($$"""
            public open class Base {
                val Count: int => 3
                val Label: string => "abc"
            }
            public class Derived : Base {}
            public class Probe {
                static func Run() -> bool {
                    val value = Derived()
                    return {{pattern}}
                }
            }
            """, expected);
    }

    [Theory]
    [InlineData(0, true)]
    [InlineData(1, false)]
    public void PropertyPatternFindsInheritedMetadataMembers(int expectedCount, bool expected)
    {
        AssertPatternResult($$"""
            import System.Collections.ObjectModel.*
            public class Probe {
                static func Run() -> bool {
                    val value = ObservableCollection<int>()
                    return value is { Count: {{expectedCount}} }
                }
            }
            """, expected);
    }

    private static void AssertPatternResult(string source, bool expected)
    {
        var compilation = Compilation.Create("InheritedPropertyPattern", [SyntaxTree.ParseText(source)],
            TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(expected, loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, null));
    }

}
