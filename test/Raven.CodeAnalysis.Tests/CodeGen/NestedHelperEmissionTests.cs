using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

[Trait("Target", "DotNet")]
public sealed class NestedHelperEmissionTests
{
    [Theory]
    [InlineData(false, false)]
    [InlineData(true, false)]
    [InlineData(false, true)]
    [InlineData(true, true)]
    public void DeepNestedHelperCanBeCalledFromContainingStaticType(bool inherits, bool generic)
    {
        var source = """
            open class Visitor {
                virtual func Visit() { }
            }
            class OuterGENERIC {
                static func Run() -> int { Analysis.Analyze() }
                private static class Analysis {
                    static func Analyze() -> int { Collector.Collect() }
                    private class Collector BASE {
                        static func Collect() -> int { 42 }
                    }
                }
            }
            """.Replace("BASE", inherits ? ": Visitor" : "")
            .Replace("GENERIC", generic ? "<T>" : "");
        var compilation = Compilation.Create("NestedHelpers" + Guid.NewGuid().ToString("N"),
            [SyntaxTree.ParseText(source)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        using var output = new MemoryStream();
        var result = compilation.Emit(output);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        var assembly = Assembly.Load(output.ToArray());
        var outer = assembly.GetType(generic ? "Outer`1" : "Outer")!;
        if (generic)
            outer = outer.MakeGenericType(typeof(int));
        Assert.Equal(42, outer.GetMethod("Run")!.Invoke(null, null));
    }
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void NestedHelperReadsItsPrivateStoredProperty(bool generic)
    {
        var source = """
            class OuterGENERIC {
                static func Run() -> int { Analysis.Analyze() }
                private static class Analysis {
                    static func Analyze() -> int { Collector.Collect() }
                    private class Collector {
                        private val _value: int = 42

                        static func Collect() -> int {
                            let collector = Collector()
                            return collector._value
                        }
                    }
                }
            }
            """.Replace("GENERIC", generic ? "<T>" : "");
        var compilation = Compilation.Create("NestedStored" + Guid.NewGuid().ToString("N"),
            [SyntaxTree.ParseText(source)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        using var output = new MemoryStream();
        var result = compilation.Emit(output);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        var assembly = Assembly.Load(output.ToArray());
        var outer = assembly.GetType(generic ? "Outer`1" : "Outer")!;
        if (generic)
            outer = outer.MakeGenericType(typeof(int));
        Assert.Equal(42, outer.GetMethod("Run")!.Invoke(null, null));
    }

    [Fact]
    public void ConstructedPrivateStoragePreservesFieldTypeForReadsAndWrites()
    {
        const string source = """
            class Box<T> {
                private var _value: T

                init(value: T) {
                    _value = value
                }

                static func Exchange(box: Box<T>, value: T) -> T {
                    let prior = box._value
                    box._value = value
                    return prior
                }

                func Read() -> T { _value }
            }
            """;
        var compilation = Compilation.Create("GenericStorage" + Guid.NewGuid().ToString("N"),
            [SyntaxTree.ParseText(source)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        using var output = new MemoryStream();
        var result = compilation.Emit(output);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        var assembly = Assembly.Load(output.ToArray());
        var box = assembly.GetType("Box`1")!.MakeGenericType(typeof(int));
        var instance = Activator.CreateInstance(box, [42]);
        Assert.Equal(42, box.GetMethod("Exchange")!.Invoke(null, [instance, 43]));
        Assert.Equal(43, box.GetMethod("Read")!.Invoke(instance, null));
        Assert.Equal(typeof(int), box.GetField("_value", BindingFlags.Instance | BindingFlags.NonPublic)!.FieldType);
    }

}
