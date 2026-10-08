using System.Reflection;
using System.Threading.Tasks;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class AsyncDiscardCodeGenTests
{
    [Theory]
    [InlineData(OptimizationLevel.Release, false)]
    [InlineData(OptimizationLevel.Debug, false)]
    [InlineData(OptimizationLevel.Release, true)]
    [InlineData(OptimizationLevel.Debug, true)]
    public async Task DiscardedAwaitResumesAndReturns(OptimizationLevel optimization, bool completed)
    {
        var compilation = Compilation.Create("AsyncDiscard" + Guid.NewGuid().ToString("N"),
            [SyntaxTree.ParseText("""
                import System.Threading.Tasks.*

                public static class Discards {
                    public static async func Run(gate: Task<int>) -> Task<int> {
                        _ = await gate
                        return 42
                    }
                }
                """)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithOptimizationLevel(optimization));
        using var output = new MemoryStream();
        var emitted = compilation.Emit(output);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        var method = Assembly.Load(output.ToArray()).GetType("Discards")!.GetMethod("Run")!;
        var gate = new TaskCompletionSource<int>(TaskCreationOptions.RunContinuationsAsynchronously);
        if (completed)
            gate.SetResult(7);
        var task = Assert.IsAssignableFrom<Task<int>>(method.Invoke(null, [gate.Task]));
        if (!completed)
        {
            Assert.False(task.IsCompleted);
            gate.SetResult(7);
        }
        Assert.Equal(42, await task.WaitAsync(TimeSpan.FromSeconds(10)));
    }
}
