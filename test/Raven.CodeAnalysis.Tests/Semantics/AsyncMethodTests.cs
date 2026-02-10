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
    public void AsyncMethod_WithoutReturnType_WithReturnExpression_InfersTaskOfResult()
    {
        const string source = """
class C {
    async f() {
        return 42;
    }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var symbol = (IMethodSymbol)model.GetDeclaredSymbol(method)!;

        Assert.True(symbol.IsAsync);
        Assert.Equal(
            "System.Threading.Tasks.Task<System.Int32>",
            symbol.ReturnType.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));
        Assert.Empty(compilation.GetDiagnostics());
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
    public void AsyncTaskMethod_WithReturnExpression_ReportsDiagnostic()
    {
        const string source = """
import System.Threading.Tasks.*

class C {
    async f() {
        return Task.CompletedTask;
    }
}
""";
        var (compilation, _) = CreateCompilation(source);
        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.AsyncTaskReturnCannotHaveExpression, diagnostic.Descriptor);
    }

    [Fact]
    public void AsyncMethod_ExpressionBody_WithReturnValue_InferredTaskOfResult()
    {
        const string source = """
class C {
    async f() => 5;
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var symbol = (IMethodSymbol)model.GetDeclaredSymbol(method)!;

        Assert.Equal(
            "System.Threading.Tasks.Task<System.Int32>",
            symbol.ReturnType.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void AsyncTaskOfIntMethod_WithIncompatibleReturnExpression_ReportsDiagnostic()
    {
        const string source = """
import System.Threading.Tasks.*

class C {
    async f() -> Task<Int32> {
        return "oops";
    }
}
""";
        var (compilation, _) = CreateCompilation(source);
        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.CannotConvertFromTypeToType, diagnostic.Descriptor);
        Assert.Contains("String", diagnostic.GetMessage(), StringComparison.OrdinalIgnoreCase);
        Assert.Contains("Int32", diagnostic.GetMessage(), StringComparison.OrdinalIgnoreCase);
    }

    [Fact]
    public void AsyncTaskOfResult_BlockBody_AllowsTailExpressionWithoutReturn()
    {
        const string source = """
import System.Threading.Tasks.*

union Result<T, E> {
    Ok(value: T)
    Error(data: E)
}

class C {
    async f(v: int) -> Task<Result<int, string>> {
        await Task.Yield()
        if v < 0 {
            return .Error("neg")
        }

        .Ok(v)
    }
}
""";

        var (compilation, _) = CreateCompilation(source);
        Assert.Empty(compilation.GetDiagnostics());
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
    public void TopLevelAwait_InVariableInitializer_PromotesSynthesizedMainToAsyncTask()
    {
        const string source = """
import System.Threading.Tasks.*

val value = await Task.FromResult(1)
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
    public void TopLevelAwait_InInvocationArgument_PromotesSynthesizedMainToAsyncTask()
    {
        const string source = """
import System.*
import System.Threading.Tasks.*

func Use(value: Int32) -> Int32 {
    return value
}

val result = Use(await Task.FromResult(1))
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
        const string source = "val x = 0";

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
    public void TopLevelReturnUnit_SynthesizesUnitMain()
    {
        const string source = "return ()";

        var (compilation, _) = CreateCompilation(source, options: new CompilationOptions(OutputKind.ConsoleApplication));
        compilation.EnsureSetup();

        var program = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.SourceGlobalNamespace.GetMembers("Program").Single());
        var main = Assert.IsAssignableFrom<IMethodSymbol>(program.GetMembers("Main").Single());

        Assert.Equal(SpecialType.System_Unit, main.ReturnType.SpecialType);
        Assert.Empty(program.GetMembers("MainAsync"));
    }

    [Fact]
    public void TopLevelAwait_WithReturnUnit_SynthesizesTaskMain()
    {
        const string source = """
import System.Threading.Tasks.*

await Task.CompletedTask
return ()
""";

        var (compilation, _) = CreateCompilation(source, options: new CompilationOptions(OutputKind.ConsoleApplication));
        compilation.EnsureSetup();

        var program = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.SourceGlobalNamespace.GetMembers("Program").Single());
        var main = Assert.IsAssignableFrom<IMethodSymbol>(program.GetMembers("Main").Single());
        var asyncMain = Assert.IsAssignableFrom<IMethodSymbol>(program.GetMembers("MainAsync").Single());

        Assert.Equal(SpecialType.System_Unit, main.ReturnType.SpecialType);
        Assert.Equal(SpecialType.System_Threading_Tasks_Task, asyncMain.ReturnType.SpecialType);
    }

    [Fact]
    public void TopLevelReturnWithoutExpression_ReportsExpressionExpected()
    {
        const string source = "return";

        var (compilation, _) = CreateCompilation(source, options: new CompilationOptions(OutputKind.ConsoleApplication));
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.ExpressionExpected);
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
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext);
        var asyncDiagnostic = Assert.Single(diagnostics.Where(d => d.Descriptor == CompilerDiagnostics.AsyncReturnTypeMustBeTaskLike));
        Assert.Contains("Int32", asyncDiagnostic.GetMessage(), StringComparison.OrdinalIgnoreCase);
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

    [Fact]
    public void AsyncPropertyGetter_WithUnresolvedType_ReportsAsyncDiagnostic()
    {
        const string source = """
class C {
    public Value: MissingTask {
        async get { return }
    }
}
""";

        var (compilation, _) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext);
        var asyncDiagnostic = Assert.Single(diagnostics.Where(d => d.Descriptor == CompilerDiagnostics.AsyncReturnTypeMustBeTaskLike));
        Assert.Contains("MissingTask", asyncDiagnostic.GetMessage(), StringComparison.OrdinalIgnoreCase);
    }

    [Fact]
    public void AsyncIndexerGetter_WithUnresolvedType_ReportsAsyncDiagnostic()
    {
        const string source = """
class C {
    public this[i: int]: MissingTask {
        async get { return }
    }
}
""";

        var (compilation, _) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext);
        var asyncDiagnostic = Assert.Single(diagnostics.Where(d => d.Descriptor == CompilerDiagnostics.AsyncReturnTypeMustBeTaskLike));
        Assert.Contains("MissingTask", asyncDiagnostic.GetMessage(), StringComparison.OrdinalIgnoreCase);
    }

    [Fact]
    public void AsyncExpressionBodiedGetter_WithCompletedTaskExpression_CachesImplicitReturn()
    {
        const string source = """
import System.Threading.Tasks.*

class C {
    public Value: Task {
        async get => Task.CompletedTask
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);

        var model = compilation.GetSemanticModel(tree);
        var accessorSyntax = tree
            .GetRoot()
            .DescendantNodes()
            .OfType<AccessorDeclarationSyntax>()
            .Single();

        var accessorSymbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(accessorSyntax));
        Assert.True(accessorSymbol.IsAsync);

        var boundBody = Assert.IsType<BoundBlockStatement>(model.GetBoundNode(accessorSyntax.ExpressionBody!));
        Assert.Empty(boundBody.LocalsToDispose);

        var returnStatement = Assert.IsType<BoundReturnStatement>(Assert.Single(boundBody.Statements));
        var memberAccess = Assert.IsType<BoundMemberAccessExpression>(returnStatement.Expression);
        var property = Assert.IsAssignableFrom<IPropertySymbol>(memberAccess.Member);

        Assert.Equal("CompletedTask", property.Name);
        var containingType = Assert.IsAssignableFrom<INamedTypeSymbol>(property.ContainingType);
        Assert.Equal("Task", containingType.Name);
        var containingNamespace = Assert.IsAssignableFrom<INamespaceSymbol>(containingType.ContainingNamespace);
        Assert.Equal("System.Threading.Tasks", containingNamespace.ToDisplayString());

        Assert.Empty(compilation.GetDiagnostics());
    }
}
