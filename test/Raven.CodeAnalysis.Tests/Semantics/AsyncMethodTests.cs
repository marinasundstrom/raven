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
    public void AsyncMethod_WithExplicitNonTaskReturnTypeAndBareReturn_ReportsSingleDiagnostic()
    {
        const string source = """
class C {
    async f() -> int {
        return;
    }
}
""";
        var (compilation, _) = CreateCompilation(source);
        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.AsyncReturnTypeMustBeTaskLike, diagnostic.Descriptor);
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

        var program = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.SourceGlobalNamespace.GetMembers("Program").Single());
        var main = Assert.IsAssignableFrom<IMethodSymbol>(program.GetMembers("Main").Single());
        var asyncMain = Assert.IsAssignableFrom<IMethodSymbol>(program.GetMembers("MainAsync").Single());

        Assert.False(main.IsAsync);
        Assert.Equal(SpecialType.System_Unit, main.ReturnType.SpecialType);

        Assert.True(asyncMain.IsAsync);
        Assert.Equal(SpecialType.System_Threading_Tasks_Task, asyncMain.ReturnType.SpecialType);

        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void TopLevelWithoutAwait_DoesNotProduceAsyncImplementation()
    {
        const string source = "let x = 0";

        var (compilation, _) = CreateCompilation(source);
        compilation.EnsureSetup();

        var program = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.SourceGlobalNamespace.GetMembers("Program").Single());
        var main = Assert.IsAssignableFrom<IMethodSymbol>(program.GetMembers("Main").Single());

        Assert.False(main.IsAsync);
        Assert.Equal(SpecialType.System_Unit, main.ReturnType.SpecialType);
        Assert.Empty(program.GetMembers("MainAsync"));
    }

    [Fact]
    public void TopLevelAwait_WithReturnExpression_SynthesizesTaskOfInt()
    {
        const string source = """
import System.Threading.Tasks.*

return await Task.FromResult(1)
""";

        var (compilation, _) = CreateCompilation(source);
        compilation.EnsureSetup();

        var program = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.SourceGlobalNamespace.GetMembers("Program").Single());
        var main = Assert.IsAssignableFrom<IMethodSymbol>(program.GetMembers("Main").Single());
        var asyncMain = Assert.IsAssignableFrom<IMethodSymbol>(program.GetMembers("MainAsync").Single());

        Assert.Equal(SpecialType.System_Int32, main.ReturnType.SpecialType);
        var asyncReturn = Assert.IsAssignableFrom<INamedTypeSymbol>(asyncMain.ReturnType);
        Assert.Equal("Task`1", asyncReturn.MetadataName);
        var containingNamespace = Assert.IsAssignableFrom<INamespaceSymbol>(asyncReturn.ContainingNamespace);
        Assert.Equal("System.Threading.Tasks", containingNamespace.ToDisplayString());
        Assert.Single(asyncReturn.TypeArguments);
        Assert.Equal(SpecialType.System_Int32, asyncReturn.TypeArguments[0].SpecialType);
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
