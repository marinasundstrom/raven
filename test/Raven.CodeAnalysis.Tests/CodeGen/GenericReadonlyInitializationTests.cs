using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public class GenericReadonlyInitializationTests
{
    [Theory]
    [InlineData("init(other: Store<T>, replacement: T) { other.Value = replacement }")]
    [InlineData("func Write(replacement: T) { self.Value = replacement }")]
    public void GenericReadonlyPropertyStillRejectsWritesOutsideOwnInitialization(string member)
    {
        var compilation = Compilation.Create("ReadonlyWriteRejected", [SyntaxTree.ParseText($$"""
            class Store<T> {
                val Value: T { get; }
                {{member}}
            }
            """)], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        Assert.Single(errors);
        Assert.Equal("RAV0200", errors[0].Id);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ExplicitSelfInitializesGenericReadonlyStorage(bool publicProperty)
    {
        var source = $$"""
            public class Store<T> {
                {{(publicProperty ? "val Value: T { get; }" : "private val value: T\nval Value: T => value")}}
                init(initial: T) {
                    self.{{(publicProperty ? "Value" : "value")}} = initial
                }
            }
            """;
        var compilation = Compilation.Create("GenericReadonlyInitialization", [SyntaxTree.ParseText(source)],
            TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        using var output = new MemoryStream();
        var result = compilation.Emit(output);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(output, compilation.References);
        foreach (var value in new object[] { 42, "retained" })
        {
            var type = loaded.Assembly.GetType("Store`1")!.MakeGenericType(value.GetType());
            var instance = Activator.CreateInstance(type, [value]);
            Assert.Equal(value, type.GetProperty("Value")!.GetValue(instance));
        }
    }
}
