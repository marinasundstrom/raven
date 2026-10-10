using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class AsyncBaseCallTests
{
    [Theory]
    [InlineData(false, false)]
    [InlineData(true, false)]
    [InlineData(false, true)]
    [InlineData(true, true)]
    public async Task AwaitedBaseCallUsesOriginalReceiverAndDirectDispatch(bool suspend, bool external)
    {
        const string parent = """
            import System.*
            import System.Threading.Tasks.*
            public open class Parent : IDisposable {
                private var value: int = 40
                public var Label: string? = null
                public virtual func Read() -> Task<int> => Task.FromResult(value)
                public func Dispose() { value = 10 }
            }
            """;
        const string child = """
            import System.*
            import System.Threading.Tasks.*
            public class Child : Parent {
                public override async func Read() -> Task<int> {
                    Label = null
                    await Task.Delay(DELAY)
                    return (await base.Read()) + 2
                }
                public func AsDisposable() -> IDisposable => self
            }
            """;
        var trees = external
            ? new[] { SyntaxTree.ParseText(child.Replace("DELAY", suspend ? "1" : "0")) }
            : new[] { SyntaxTree.ParseText(parent), SyntaxTree.ParseText(child.Replace("DELAY", suspend ? "1" : "0")) };
        var references = external
            ? TestMetadataReferences.Default.Append(TestMetadataFactory.CreateFileReferenceFromSource(parent, "AsyncBaseProvider")).ToArray()
            : TestMetadataReferences.Default;
        var compilation = Compilation.Create("AsyncBase", trees, references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        var type = loaded.Assembly.GetType("Child")!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Read")!;
        Assert.Equal(42, await ((Task<int>)method.Invoke(instance, null)!).WaitAsync(TimeSpan.FromSeconds(10)));
        ((IDisposable)type.GetMethod("AsDisposable")!.Invoke(instance, null)!).Dispose();
        Assert.Equal(12, await ((Task<int>)method.Invoke(instance, null)!).WaitAsync(TimeSpan.FromSeconds(10)));
    }
}
