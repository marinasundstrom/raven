using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
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
            .OfType<PrefixOperatorExpressionSyntax>()
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
            .OfType<PrefixOperatorExpressionSyntax>()
            .Single(e => e.Kind == SyntaxKind.AwaitExpression);
        var typeInfo = model.GetTypeInfo(awaitExpression);
        Assert.Equal(SpecialType.System_Unit, typeInfo.Type!.SpecialType);
    }

    [Fact]
    public void AwaitExpression_ValueTaskAndValueTaskOfT_HaveAwaitedResultTypes()
    {
        const string source = """
import System.Threading.Tasks.*

async func outer() {
    val number = await ValueTask.FromResult(42)
    val done = await ValueTask.CompletedTask
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var awaits = tree.GetRoot().DescendantNodes()
            .OfType<PrefixOperatorExpressionSyntax>()
            .Where(e => e.Kind == SyntaxKind.AwaitExpression)
            .ToArray();

        Assert.Equal(2, awaits.Length);
        Assert.Equal(SpecialType.System_Int32, model.GetTypeInfo(awaits[0]).Type!.SpecialType);
        Assert.Equal(SpecialType.System_Unit, model.GetTypeInfo(awaits[1]).Type!.SpecialType);

        var locals = tree.GetRoot()
            .DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .ToDictionary(static declarator => declarator.Identifier.ValueText);
        var number = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(locals["number"]));
        var done = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(locals["done"]));

        Assert.Equal(SpecialType.System_Int32, number.Type.SpecialType);
        Assert.Equal(SpecialType.System_Unit, done.Type.SpecialType);
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
        var diagnostic = Assert.Single(compilation.GetDiagnostics().Where(d => d.Descriptor == CompilerDiagnostics.AwaitExpressionRequiresAsyncContext));
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
        var diagnostic = Assert.Single(compilation.GetDiagnostics().Where(d => d.Descriptor == CompilerDiagnostics.ExpressionIsNotAwaitable));
        Assert.Equal(CompilerDiagnostics.ExpressionIsNotAwaitable, diagnostic.Descriptor);
    }

    [Fact]
    public void AwaitExpression_ErrorOperand_DoesNotReportNotAwaitable()
    {
        const string source = """
async func outer() {
    await Task.CompletedTask;
}
""";
        var (compilation, _) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(
            diagnostics,
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.ExpressionIsNotAwaitable);
    }

    [Fact]
    public void AwaitExpression_MissingIsCompleted_ReportsDiagnostic()
    {
        const string source = """
async func outer() {
    await Awaitable();
}

class Awaitable
{
    public func GetAwaiter() -> MissingIsCompletedAwaiter {
        return MissingIsCompletedAwaiter();
    }
}

class MissingIsCompletedAwaiter
{
    public func GetResult() -> unit {}
}
""";
        var (compilation, _) = CreateCompilation(source);
        var diagnostic = Assert.Single(compilation.GetDiagnostics().Where(d => d.Descriptor == CompilerDiagnostics.AwaiterMissingIsCompleted));
        Assert.Equal(CompilerDiagnostics.AwaiterMissingIsCompleted, diagnostic.Descriptor);
    }

    [Fact]
    public void AwaitExpression_MissingGetResult_DoesNotReportLegacyDiagnostic()
    {
        const string source = """
async func outer() {
    await Awaitable();
}

class Awaitable
{
    public func GetAwaiter() -> MissingGetResultAwaiter {
        return MissingGetResultAwaiter();
    }
}

class MissingGetResultAwaiter
{
    public IsCompleted: bool {
        get => true
    }
}
""";
        var (compilation, _) = CreateCompilation(source);
        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.AwaiterMissingGetResult);
    }
}
