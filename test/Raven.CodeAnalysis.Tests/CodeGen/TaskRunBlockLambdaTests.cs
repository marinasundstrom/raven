using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public class TaskRunBlockLambdaTests
{
    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void TaskRunTargetsBlockLambdaResult(bool explicitTypeArgument)
    {
        var references = TestMetadataReferences.Default;
        var tree = SyntaxTree.ParseText($$"""
            import System.Threading.Tasks.*
            public class Example {
                static func Run() -> int {
                    let pending = Task.Run{{(explicitTypeArgument ? "<int>" : "")}}(() => {
                        let value = 40
                        return value + 2
                    })
                    return pending.GetAwaiter().GetResult()
                }
            }
            """);
        var compilation = Compilation.Create("BlockLambdaConsumer", [tree], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var output = new MemoryStream();
        var emitted = compilation.Emit(output);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(output, compilation.References);
        Assert.Equal(42, loaded.Assembly.GetType("Example")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Fact]
    public void UniqueActionTargetStillRejectsExplicitValueReturn()
    {
        var compilation = Compilation.Create("InvalidAction", [SyntaxTree.ParseText("""
            import System.*
            public class Example {
                static func Accept(callback: Action) { }
                static func Run() {
                    Accept(() => { return 42 })
                }
            }
            """)], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.Contains(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAV1503");
    }

    [Fact]
    public void CompletionOnlyTaskRunBlockExecutes()
    {
        var compilation = Compilation.Create("CompletionBlock", [SyntaxTree.ParseText("""
            import System.Threading.Tasks.*
            public class State {
                var Value: int = 0
            }
            public class Example {
                static func Run() -> int {
                    let state = State()
                    let pending = Task.Run(() => { state.Value = 42 })
                    pending.GetAwaiter().GetResult()
                    return state.Value
                }
            }
            """)], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var output = new MemoryStream();
        var emitted = compilation.Emit(output);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(output, compilation.References);
        Assert.Equal(42, loaded.Assembly.GetType("Example")!.GetMethod("Run")!.Invoke(null, null));
    }

}
