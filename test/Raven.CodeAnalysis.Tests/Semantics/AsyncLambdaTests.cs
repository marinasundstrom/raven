using System;
using System.Linq;
using System.Threading.Tasks;
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
    public void AsyncLambda_WithoutAwait_RewritesToCompletedTaskFromResult()
    {
        const string source = """
import System.Threading.Tasks.*

let projector = async () => 42
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var model = compilation.GetSemanticModel(tree);
        var lambdaSyntax = tree
            .GetRoot()
            .DescendantNodes()
            .OfType<ParenthesizedLambdaExpressionSyntax>()
            .Single();

        var boundLambda = Assert.IsType<BoundLambdaExpression>(model.GetBoundNode(lambdaSyntax));
        var owner = boundLambda.Symbol?.ContainingSymbol ?? compilation.Assembly.GlobalNamespace;

        var lowered = LambdaLowerer.Rewrite(boundLambda, owner);

        var invocation = Assert.IsType<BoundInvocationExpression>(lowered.Body);
        Assert.Equal("FromResult", invocation.Method.Name);
        var containingType = Assert.IsAssignableFrom<INamedTypeSymbol>(invocation.Method.ContainingType);
        Assert.Equal(SpecialType.System_Threading_Tasks_Task_T, containingType.OriginalDefinition.SpecialType);
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
}
