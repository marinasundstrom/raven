using System;
using System.Linq;
using System.Threading.Tasks;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class AsyncLambdaTests : CompilationTestBase
{
    [Fact]
    public void AsyncLambda_WithInferredResult_DefaultsToTaskOfResult()
    {
        const string source = """
using System;
using System.Threading.Tasks;

class C
{
    void M()
    {
        Func<Task<int>> projector = async () => await G();
    }

    Task<int> G() => Task.FromResult(1);
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);

        var lambdaSyntax = tree
            .GetRoot()
            .DescendantNodes()
            .OfType<SimpleLambdaExpressionSyntax>()
            .Single();

        var boundLambda = Assert.IsType<BoundLambdaExpression>(model.GetBoundNode(lambdaSyntax));
        var lambdaSymbol = Assert.IsAssignableFrom<ILambdaSymbol>(boundLambda.Symbol);

        Assert.True(lambdaSymbol.IsAsync);

        var expectedReturnType = ((INamedTypeSymbol)compilation.GetSpecialType(SpecialType.System_Threading_Tasks_Task_T))
            .Construct(compilation.GetSpecialType(SpecialType.System_Int32));

        Assert.True(SymbolEqualityComparer.Default.Equals(expectedReturnType, boundLambda.ReturnType));
        Assert.Equal(SpecialType.System_Int32, boundLambda.Body.Type?.SpecialType);
    }

    [Fact]
    public void AsyncLambda_WithBlockBody_DefaultsToTask()
    {
        const string source = """
using System;
using System.Threading.Tasks;

class C
{
    void M()
    {
        var handler = async () => { await G(); };
    }

    Task G() => Task.CompletedTask;
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);

        var lambdaSyntax = tree
            .GetRoot()
            .DescendantNodes()
            .OfType<ParenthesizedLambdaExpressionSyntax>()
            .Single();

        var boundLambda = Assert.IsType<BoundLambdaExpression>(model.GetBoundNode(lambdaSyntax));
        var lambdaSymbol = Assert.IsAssignableFrom<ILambdaSymbol>(boundLambda.Symbol);

        Assert.True(lambdaSymbol.IsAsync);

        var taskType = compilation.GetSpecialType(SpecialType.System_Threading_Tasks_Task);
        Assert.True(SymbolEqualityComparer.Default.Equals(taskType, boundLambda.ReturnType));
    }

    [Fact]
    public void AsyncLambda_WithExplicitNonTaskReturnType_ReportsDiagnostic()
    {
        const string source = """
using System;
using System.Threading.Tasks;

class C
{
    void M()
    {
        Func<int> projector = async () -> int => 42;
    }
}
""";

        var (compilation, _) = CreateCompilation(source);
        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.AsyncReturnTypeMustBeTaskLike, diagnostic.Descriptor);
        Assert.Contains("int", diagnostic.GetMessage(), StringComparison.OrdinalIgnoreCase);
    }

    [Fact]
    public void AsyncLambda_WithExplicitNonTaskReturnTypeAndBlockBody_ReportsSingleDiagnostic()
    {
        const string source = """
import System.*
import System.Threading.Tasks.*

let projector = async () -> int => {
    return 1
}
""";

        var (compilation, _) = CreateCompilation(source);
        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.AsyncReturnTypeMustBeTaskLike, diagnostic.Descriptor);
    }

    [Fact]
    public void AsyncLambda_InTopLevelAwaitContext_BindsAndInfersTaskResult()
    {
        const string source = """
import System.*
import System.Threading.Tasks.*

let handler = async () => await Task.FromResult(2)
let result = await handler()
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var lambdaSyntax = tree
            .GetRoot()
            .DescendantNodes()
            .OfType<SimpleLambdaExpressionSyntax>()
            .Single();

        var model = compilation.GetSemanticModel(tree);
        var boundLambda = Assert.IsType<BoundLambdaExpression>(model.GetBoundNode(lambdaSyntax));
        var lambdaSymbol = Assert.IsAssignableFrom<ILambdaSymbol>(boundLambda.Symbol);

        Assert.True(lambdaSymbol.IsAsync);

        var expectedReturn = ((INamedTypeSymbol)compilation.GetSpecialType(SpecialType.System_Threading_Tasks_Task_T))
            .Construct(compilation.GetSpecialType(SpecialType.System_Int32));

        Assert.True(SymbolEqualityComparer.Default.Equals(expectedReturn, boundLambda.ReturnType));
    }

    [Fact]
    public void AsyncLambda_TaskRunWithoutAwait_InfersTaskOfResult()
    {
        const string source = """
import System.Threading.Tasks.*

let result = await Task.Run(async () => 42)
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, d => d.Severity == DiagnosticSeverity.Error);

        var lambdaSyntax = tree
            .GetRoot()
            .DescendantNodes()
            .OfType<SimpleLambdaExpressionSyntax>()
            .Single();

        var model = compilation.GetSemanticModel(tree);
        var boundLambda = Assert.IsType<BoundLambdaExpression>(model.GetBoundNode(lambdaSyntax));
        var returnType = Assert.IsAssignableFrom<INamedTypeSymbol>(boundLambda.ReturnType);

        Assert.Equal("Task`1", returnType.MetadataName);
        Assert.Equal(SpecialType.System_Int32, returnType.TypeArguments.Single().SpecialType);
    }

    [Fact]
    public void AsyncLambda_TaskRunInvocation_BindsGenericOverload()
    {
        const string source = """
import System.Console.*
import System.Threading.Tasks.*

let t = await Task.Run(async () => 42)
WriteLine(t)
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, d => d.Severity == DiagnosticSeverity.Error);

        var root = tree.GetRoot();
        var taskRunInvocation = root
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .First();

        var model = compilation.GetSemanticModel(tree);
        var symbolInfo = model.GetSymbolInfo(taskRunInvocation);
        var method = Assert.IsAssignableFrom<IMethodSymbol>(symbolInfo.Symbol);

        Assert.True(method.IsGenericMethod);
        Assert.Equal("Run", method.Name);
        var typeArgument = Assert.Single(method.TypeArguments);
        Assert.Equal(SpecialType.System_Int32, typeArgument.SpecialType);
    }
}
