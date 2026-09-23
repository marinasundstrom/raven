using System;
using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public sealed class AsyncCancellationPolicyTests
{
    [Fact]
    public void PolicyIsExplicitAndSurvivesOptionCopies()
    {
        Assert.False(new CompilationOptions().PropagateAsyncCancellation);
        var options = new CompilationOptions().WithAsyncCancellationPropagation(true)
            .WithAsyncExceptionCapture(false).WithHeapAsyncStateMachines(true)
            .WithOutputKind(OutputKind.DynamicallyLinkedLibrary)
            .WithGraphemeChar(false).WithOptimizationLevel(OptimizationLevel.Release);
        Assert.True(options.PropagateAsyncCancellation);
        Assert.False(options.WithAsyncCancellationPropagation(false).PropagateAsyncCancellation);
    }

    [Fact]
    public void AwaitInForLoopIsRejectedUntilIterationCleanupIsSuspensionAware()
    {
        const string source = """
import System.Threading.Tasks.*
class Program {
    static async func Run(input: Task<int>) -> Task<int> {
        for item in [1, 2] { let result = await input }
        return 42
    }
}
""";
        var compilation = Compilation.Create("cancellation-loop",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithAsyncCancellationPropagation(true))
            .AddSyntaxTrees(SyntaxTree.ParseText(source)).AddReferences(TestMetadataReferences.Default);
        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == "RAV2712");
    }

    [Theory]
    [InlineData(false, "IsCancelled")]
    [InlineData(true, "SetCancelled")]
    public void MissingTargetProtocolProducesDiagnostic(bool hasCancellation, string member)
    {
        var source = """
import System.*
import System.Threading.Tasks.*
class Awaiter {
    val IsCompleted: bool => true
    CANCELLATION
    func GetAwaiter() -> Awaiter => self
    func GetResult() -> int => 42
    func OnCompleted(callback: Action) { callback() }
}
class Program {
    static async func Run(input: Awaiter) -> Task<int> { return await input }
}
""".Replace("CANCELLATION", hasCancellation ? "val IsCancelled: bool => false" : "");
        var compilation = Compilation.Create("cancellation-protocol",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithAsyncCancellationPropagation(true))
            .AddSyntaxTrees(SyntaxTree.ParseText(source)).AddReferences(TestMetadataReferences.Default);
        var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        Assert.Contains(errors, d => d.GetMessage().Contains(member, StringComparison.Ordinal));
    }
}
