using System;
using System.Linq;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class AsyncMethodTests : CompilationTestBase
{
    [Fact]
    public void AsyncMethod_WithoutReturnType_DefaultsToTask()
    {
        const string source = """
class C {
    async f() {}
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var symbol = (IMethodSymbol)model.GetDeclaredSymbol(method)!;
        Assert.True(symbol.IsAsync);
        Assert.Equal(SpecialType.System_Threading_Tasks_Task, symbol.ReturnType.SpecialType);
    }

    [Fact]
    public void AsyncMethod_WithExplicitNonTaskReturnType_ReportsDiagnostic()
    {
        const string source = """
class C {
    async f() -> int {
        return 1;
    }
}
""";
        var (compilation, _) = CreateCompilation(source);
        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.AsyncReturnTypeMustBeTaskLike, diagnostic.Descriptor);
        Assert.Contains("int", diagnostic.GetMessage(), StringComparison.OrdinalIgnoreCase);
    }

    [Fact]
    public void TopLevelAwait_PromotesSynthesizedMainToAsyncTask()
    {
        const string source = """
import System.Threading.Tasks.*

await Task.CompletedTask
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        Assert.Empty(compilation.GetDiagnostics());

        var program = Assert.IsType<INamedTypeSymbol>(compilation.SourceGlobalNamespace.GetMembers("Program").Single());
        var main = Assert.IsType<IMethodSymbol>(program.GetMembers("Main").Single());

        Assert.True(main.IsAsync);
        Assert.Equal(SpecialType.System_Threading_Tasks_Task, main.ReturnType.SpecialType);
    }

    [Fact]
    public void AsyncFunction_WithExplicitNonTaskReturnType_ReportsDiagnostic()
    {
        const string source = """
async func outer() -> string {
    return "done";
}
""";
        var (compilation, _) = CreateCompilation(source);
        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.AsyncReturnTypeMustBeTaskLike, diagnostic.Descriptor);
        Assert.Contains("string", diagnostic.GetMessage(), StringComparison.OrdinalIgnoreCase);
    }

    [Fact]
    public void AsyncPropertyGetter_WithNonTaskType_ReportsDiagnostic()
    {
        const string source = """
import System.Threading.Tasks.*

class C {
    public Value: Int32 {
        async get { return 1 }
    }
}
""";

        var (compilation, _) = CreateCompilation(source);
        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.AsyncReturnTypeMustBeTaskLike, diagnostic.Descriptor);
        Assert.Contains("Int32", diagnostic.GetMessage(), StringComparison.OrdinalIgnoreCase);
    }

    [Fact]
    public void AsyncPropertyGetter_WithTaskType_IsAccepted()
    {
        const string source = """
import System.Threading.Tasks.*

class C {
    public Value: Task<int> {
        async get => await Task.FromResult(1)
    }
}
""";

        var (compilation, _) = CreateCompilation(source);
        Assert.Empty(compilation.GetDiagnostics());
    }
}
