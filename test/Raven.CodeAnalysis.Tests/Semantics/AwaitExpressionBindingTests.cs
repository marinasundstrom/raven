using System.Linq;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class AwaitExpressionBindingTests : CompilationTestBase
{
    [Fact]
    public void AwaitExpression_TaskOfInt_HasResultType()
    {
        const string source = """
import System.Threading.Tasks.*

async func outer() {
    val value = await Task.FromResult(42);
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var awaitExpression = tree.GetRoot().DescendantNodes()
            .OfType<UnaryExpressionSyntax>()
            .Single(e => e.Kind == SyntaxKind.AwaitExpression);
        var typeInfo = model.GetTypeInfo(awaitExpression);
        Assert.Equal(SpecialType.System_Int32, typeInfo.Type!.SpecialType);
    }

    [Fact]
    public void AwaitExpression_Task_HasUnitResult()
    {
        const string source = """
import System.Threading.Tasks.*

async func outer() {
    val value = await Task.CompletedTask;
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var awaitExpression = tree.GetRoot().DescendantNodes()
            .OfType<UnaryExpressionSyntax>()
            .Single(e => e.Kind == SyntaxKind.AwaitExpression);
        var typeInfo = model.GetTypeInfo(awaitExpression);
        Assert.Equal(SpecialType.System_Unit, typeInfo.Type!.SpecialType);
    }

    [Fact]
    public void AwaitExpression_OutsideAsyncContext_ReportsDiagnostic()
    {
        const string source = """
import System.Threading.Tasks.*

func outer() {
    await Task.CompletedTask;
}
""";
        var (compilation, _) = CreateCompilation(source);
        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.AwaitExpressionRequiresAsyncContext, diagnostic.Descriptor);
    }

    [Fact]
    public void AwaitExpression_NonAwaitable_ReportsDiagnostic()
    {
        const string source = """
async func outer() {
    await 42;
}
""";
        var (compilation, _) = CreateCompilation(source);
        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.ExpressionIsNotAwaitable, diagnostic.Descriptor);
    }

    [Fact]
    public void AwaitExpression_MissingIsCompleted_ReportsDiagnostic()
    {
        const string source = """
class Awaitable
{
    public MissingIsCompletedAwaiter GetAwaiter() => new MissingIsCompletedAwaiter();
}

class MissingIsCompletedAwaiter
{
    public void GetResult() {}
}

async func outer() {
    await new Awaitable();
}
""";
        var (compilation, _) = CreateCompilation(source);
        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.AwaiterMissingIsCompleted, diagnostic.Descriptor);
    }

    [Fact]
    public void AwaitExpression_MissingGetResult_ReportsDiagnostic()
    {
        const string source = """
class Awaitable
{
    public MissingGetResultAwaiter GetAwaiter() => new MissingGetResultAwaiter();
}

class MissingGetResultAwaiter
{
    public bool IsCompleted => true;
}

async func outer() {
    await new Awaitable();
}
""";
        var (compilation, _) = CreateCompilation(source);
        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.AwaiterMissingGetResult, diagnostic.Descriptor);
    }
}
