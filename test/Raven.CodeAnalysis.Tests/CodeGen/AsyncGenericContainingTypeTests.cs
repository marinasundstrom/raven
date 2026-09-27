using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public class AsyncGenericContainingTypeTests
{
    [Theory]
    [InlineData(false, false)]
    [InlineData(true, false)]
    [InlineData(false, true)]
    [InlineData(true, true)]
    public async Task AsyncMethodInsideGenericClassRetainsTypeArguments(bool genericMethod, bool capture)
    {
        var parameter = genericMethod ? "T" : "U";
        var compilation = Compilation.Create("GenericOwnerAsync", [SyntaxTree.ParseText($$"""
            import System.*
            import System.Threading.Tasks.*
            public class Example<U> {
                static async func Read{{(genericMethod ? "<T>" : "")}}(initial: {{parameter}}, replacement: {{parameter}}) -> Task<{{parameter}}> {
                    var value = initial
                    {{(capture ? "let read: Func<" + parameter + "> = () => value" : "")}}
                    await Task.Yield()
                    value = replacement
                    return {{(capture ? "read()" : "value")}}
                }
            }
            """)], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        using var output = new MemoryStream();
        var emitted = compilation.Emit(output);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(output, compilation.References);
        var method = loaded.Assembly.GetType("Example`1")!.MakeGenericType(typeof(string)).GetMethod("Read")!;
        if (genericMethod)
            Assert.Equal(42, await (Task<int>)method.MakeGenericMethod(typeof(int)).Invoke(null, [1, 42])!);
        else
            Assert.Equal("after", await (Task<string>)method.Invoke(null, ["before", "after"])!);
    }
    [Theory]
    [InlineData(false, false)]
    [InlineData(true, false)]
    [InlineData(false, true)]
    [InlineData(true, true)]
    public async Task InstanceAsyncMethodRetainsOriginalReceiver(bool genericOwner, bool explicitReceiver)
    {
        var parameter = genericOwner ? "U" : "string";
        var compilation = Compilation.Create("GenericReceiverAsync", [SyntaxTree.ParseText($$"""
            import System.Threading.Tasks.*
            public class Example{{(genericOwner ? "<U>" : "")}} {
                private field value: {{parameter}}
                init(initial: {{parameter}}) { value = initial }
                async func Read(replacement: {{parameter}}) -> Task<{{parameter}}> {
                    await Task.Yield()
                    {{(explicitReceiver ? "self." : "")}}value = replacement
                    return value
                }
            }
            """)], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        using var output = new MemoryStream();
        var emitted = compilation.Emit(output);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(output, compilation.References);
        var type = genericOwner ? loaded.Assembly.GetType("Example`1")!.MakeGenericType(typeof(string))
            : loaded.Assembly.GetType("Example")!;
        var instance = Activator.CreateInstance(type, ["before"]);
        Assert.Equal("after", await (Task<string>)type.GetMethod("Read")!.Invoke(instance, ["after"])!);
    }

    [Fact]
    public async Task NestedGenericOwnerRetainsAllEnclosingArguments()
    {
        var compilation = Compilation.Create("NestedGenericAsync", [SyntaxTree.ParseText("""
            import System.Threading.Tasks.*
            public class Outer<U> {
                public class Example<V> {
                    static async func Read<T>(replacement: T) -> Task<T> {
                        await Task.Yield()
                        return replacement
                    }
                }
            }
            """)], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        using var output = new MemoryStream();
        var emitted = compilation.Emit(output);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(output, compilation.References);
        var method = loaded.Assembly.GetType("Outer`1+Example`1")!
            .MakeGenericType(typeof(string), typeof(bool)).GetMethod("Read")!.MakeGenericMethod(typeof(int));
        Assert.Equal(42, await (Task<int>)method.Invoke(null, [42])!);
    }

}
