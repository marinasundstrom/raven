using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Binding;

public sealed class NodeInterestBindingTests
{
    [Fact]
    public void GetSymbolInfo_ResolvesLocalFromSyntaxNodeWithoutPrecomputingDiagnostics()
    {
        const string source = """
class C {
    func Run() -> int {
        val count = 42
        return count
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "node-interest-binding-tests",
                [syntaxTree],
                TestMetadataReferences.Default,
                new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(syntaxTree);
        var identifier = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Last(node => node.Identifier.ValueText == "count");

        var symbol = model.GetSymbolInfo(identifier).Symbol;

        var local = Assert.IsAssignableFrom<ILocalSymbol>(symbol);
        Assert.Equal("count", local.Name);
    }

    [Fact]
    public void GetTypeInfo_ResolvesLocalReferenceTypeWithoutPrecomputingDiagnostics()
    {
        const string source = """
class C {
    func Run() -> int {
        val count = 42
        return count
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "node-interest-binding-tests",
                [syntaxTree],
                TestMetadataReferences.Default,
                new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(syntaxTree);
        var identifier = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Last(node => node.Identifier.ValueText == "count");

        var typeInfo = model.GetTypeInfo(identifier);

        Assert.Equal(SpecialType.System_Int32, typeInfo.Type?.SpecialType);
        Assert.Equal(SpecialType.System_Int32, typeInfo.ConvertedType?.SpecialType);
    }

    [Fact]
    public void GetSymbolInfo_ResolvesParameterReceiverFromNodeInterestPath()
    {
        const string source = """
class Counter {
    func Increment() -> () { }
}

class C {
    func Run(counter: Counter) -> () {
        counter.Increment()
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "node-interest-binding-tests",
                [syntaxTree],
                TestMetadataReferences.Default,
                new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(syntaxTree);
        var identifier = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Last(node => node.Identifier.ValueText == "counter");

        var symbol = model.GetSymbolInfo(identifier).Symbol;

        var parameter = Assert.IsAssignableFrom<IParameterSymbol>(symbol);
        Assert.Equal("counter", parameter.Name);
    }

    [Fact]
    public void GetTypeInfo_OnFunctionExpression_DoesNotUseNodeInterestBinderFallback()
    {
        const string source = """
class C {
    func Run() -> () {
        val project = (x: int) => x
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "node-interest-binding-tests",
                [syntaxTree],
                TestMetadataReferences.Default,
                new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(syntaxTree);
        var functionExpression = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionExpressionSyntax>()
            .Single();

        var typeInfo = model.GetTypeInfo(functionExpression);

        Assert.NotNull(typeInfo.Type ?? typeInfo.ConvertedType);
    }

    [Fact]
    public void TryGetFunctionExpressionDelegateType_ResolvesContextualDelegateWithoutFullDiagnostics()
    {
        const string source = """
class C {
    func Run() -> () {
        val project: (int) -> int = (x) => x
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "node-interest-binding-tests",
                [syntaxTree],
                TestMetadataReferences.Default,
                new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(syntaxTree);
        var functionExpression = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionExpressionSyntax>()
            .Single();

        var resolved = model.TryGetFunctionExpressionDelegateType(functionExpression, out var delegateType);

        Assert.True(resolved);
        Assert.NotNull(delegateType);
        Assert.Equal(TypeKind.Delegate, delegateType!.TypeKind);
        Assert.Equal(delegateType, model.GetTypeInfo(functionExpression).Type);
    }

    [Fact]
    public void GetSymbolInfo_OnNamedFunctionExpression_ResolvesLambdaMethodFromNodeInterestPath()
    {
        const string source = """
class C {
    func Run() -> () {
        val project = func Step(n: int) -> int => n
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "node-interest-binding-tests",
                [syntaxTree],
                TestMetadataReferences.Default,
                new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(syntaxTree);
        var functionExpression = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionExpressionSyntax>()
            .Single();

        var symbol = model.GetSymbolInfo(functionExpression).Symbol;

        var method = Assert.IsAssignableFrom<IMethodSymbol>(symbol);
        Assert.Equal(MethodKind.LambdaMethod, method.MethodKind);
        Assert.Same(functionExpression.SyntaxTree, method.DeclaringSyntaxReferences.Single().SyntaxTree);
    }

    [Fact]
    public void GetDeclaredSymbol_OnNamedRecursiveFunctionExpression_DoesNotRecurseThroughContainingOwner()
    {
        const string source = """
class C {
    func Run() -> int {
        val compute = func Step(n: int) -> int {
            if n < 1
                0
            else
                Step(n - 1)
        }

        compute(3)
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "node-interest-binding-tests",
                [syntaxTree],
                TestMetadataReferences.Default,
                new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(syntaxTree);
        var functionExpression = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<ParenthesizedFunctionExpressionSyntax>()
            .Single();

        var symbol = model.GetDeclaredSymbol(functionExpression);

        var method = Assert.IsAssignableFrom<IMethodSymbol>(symbol);
        Assert.Equal(MethodKind.LambdaMethod, method.MethodKind);
        Assert.Equal("Step", functionExpression.Identifier.ValueText);
    }

    [Fact]
    public void TryGetFunctionExpressionSymbol_UpgradesShallowErrorSymbol_AfterContextualBinding()
    {
        const string source = """
class User(var Name: string)

class C {
    func Run() -> unit {
        val project: (User) -> string = user => user.Name
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "node-interest-binding-tests",
                [syntaxTree],
                TestMetadataReferences.Default,
                new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(syntaxTree);
        var lambda = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<SimpleFunctionExpressionSyntax>()
            .Single();
        var parameterSyntax = lambda.Parameter;
        parameterSyntax.ShouldNotBeNull();

        model.TryGetFunctionExpressionSymbol(lambda, out var initialSymbol).ShouldBeTrue();
        initialSymbol.ShouldNotBeNull();
        initialSymbol!.Parameters.Length.ShouldBe(1);
        initialSymbol.Parameters[0].Type.TypeKind.ShouldBe(TypeKind.Error);

        var reboundParameter = model.GetFunctionExpressionParameterSymbol(parameterSyntax!);
        reboundParameter.ShouldNotBeNull();
        reboundParameter!.Type.Name.ShouldBe("User");

        model.TryGetFunctionExpressionSymbol(lambda, out var upgradedSymbol).ShouldBeTrue();
        upgradedSymbol.ShouldNotBeNull();
        upgradedSymbol!.Parameters.Length.ShouldBe(1);
        upgradedSymbol.Parameters[0].Type.Name.ShouldBe("User");
        upgradedSymbol.Parameters[0].Type.TypeKind.ShouldNotBe(TypeKind.Error);
    }

    [Fact]
    public void GetDeclaredSymbol_OnLambdaDependentLocal_ReturnsContextualResultType()
    {
        const string source = """
import System.*
import System.Linq.*
import System.Collections.Generic.*
import System.Linq.Expressions.*

class User(var Name: string, var IsActive: bool)

class C {
    func Run(users: IQueryable<User>) -> unit {
        val onlyActiveAdults: Expression<System.Func<User, bool>> = user => user.IsActive
        val query = users
            |> Where(onlyActiveAdults)
            |> OrderBy(user => user.Name)
            |> Select(user => user.Name)
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "node-interest-binding-tests",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var queryDeclarator = root.DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(node => node.Identifier.ValueText == "query");
        var nameLambdaParameter = root.DescendantNodes()
            .OfType<SimpleFunctionExpressionSyntax>()
            .Last()
            .Parameter;

        var initialLocal = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(queryDeclarator));
        Assert.NotEqual(TypeKind.Error, initialLocal.Type.TypeKind);

        var contextualParameter = Assert.IsAssignableFrom<IParameterSymbol>(model.GetDeclaredSymbol(nameLambdaParameter));
        Assert.Equal("User", contextualParameter.Type.Name);

        var upgradedLocal = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(queryDeclarator));
        Assert.NotEqual(TypeKind.Error, upgradedLocal.Type.TypeKind);

        var queryType = Assert.IsAssignableFrom<INamedTypeSymbol>(upgradedLocal.Type);
        Assert.Equal("IQueryable", queryType.Name);
        Assert.Equal("String", Assert.Single(queryType.TypeArguments).Name);
    }

    [Fact]
    public void TryGetInvocationTargetSymbolInfo_ForPipeExtensionLambda_UsesCompilerOwnedInvocationResult()
    {
        const string source = """
import System.*
import System.Linq.*
import System.Linq.Expressions.*

class User(var Name: string, var IsActive: bool)

class C {
    func Run(users: IQueryable<User>) -> unit {
        val onlyActiveAdults: Expression<System.Func<User, bool>> = user => user.IsActive
        val query = users
            |> Where(onlyActiveAdults)
            |> OrderBy(user => user.Name)
            |> Select(user => user.Name)
    }
}
""";

        var instrumentation = new PerformanceInstrumentation();
        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "node-interest-binding-tests",
                [syntaxTree],
                TestMetadataReferences.Default,
                new CompilationOptions(
                    OutputKind.ConsoleApplication,
                    performanceInstrumentation: instrumentation));
        var model = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var whereInvocation = root.DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression.ToString() == "Where");

        var setupBefore = instrumentation.Setup.CaptureSnapshot();
        var queryBefore = instrumentation.SemanticQuery.CaptureSnapshot();

        var resolved = model.TryGetInvocationTargetSymbolInfo(whereInvocation, out var info);

        var setupDelta = CompilerSetupInstrumentation.Subtract(
            instrumentation.Setup.CaptureSnapshot(),
            setupBefore);
        var queryDelta = SemanticQueryInstrumentation.Subtract(
            instrumentation.SemanticQuery.CaptureSnapshot(),
            queryBefore);
        resolved.ShouldBeTrue();
        var method = info.Symbol.ShouldBeAssignableTo<IMethodSymbol>();
        method.Name.ShouldBe("Where");
        method.ContainingType?.Name.ShouldBe("Queryable");
        model.RootBinderCreated.ShouldBeFalse();
        setupDelta.EnsureRootBinderCreatedCalls.ShouldBe(0);
        setupDelta.RootBinderCreations.ShouldBe(0);
        queryDelta.SymbolInfoBinderFallbacks.ShouldBe(0);
    }

    [Fact]
    public void TryGetInvocationTargetSymbolInfo_WithMixedGenericOverloads_UsesClosedAvailableCandidate()
    {
        const string source = """
import System.*
import System.Runtime.CompilerServices.*

class App()

static class EndpointExtensions {
    [ExtensionAttribute]
    static func MapGet(app: App, pattern: string, handler: Delegate) -> int {
        return 0
    }

    [ExtensionAttribute]
    static func MapGet<T>(app: App, pattern: string, handler: Func<T>) -> int {
        return 1
    }
}

class C {
    func Run(app: App) -> unit {
        app.MapGet("/", func () => 1)
    }
}
""";

        var instrumentation = new PerformanceInstrumentation();
        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "node-interest-binding-tests",
                [syntaxTree],
                TestMetadataReferences.Default,
                new CompilationOptions(
                    OutputKind.ConsoleApplication,
                    performanceInstrumentation: instrumentation));
        var model = compilation.GetSemanticModel(syntaxTree);
        var invocation = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression.ToString() == "app.MapGet");

        var setupBefore = instrumentation.Setup.CaptureSnapshot();
        var queryBefore = instrumentation.SemanticQuery.CaptureSnapshot();

        var resolved = model.TryGetInvocationTargetSymbolInfo(invocation, out var info);

        var setupDelta = CompilerSetupInstrumentation.Subtract(
            instrumentation.Setup.CaptureSnapshot(),
            setupBefore);
        var queryDelta = SemanticQueryInstrumentation.Subtract(
            instrumentation.SemanticQuery.CaptureSnapshot(),
            queryBefore);
        resolved.ShouldBeTrue();
        var method = info.Symbol.ShouldBeAssignableTo<IMethodSymbol>();
        method.Name.ShouldBe("MapGet");
        method.TypeParameters.Length.ShouldBe(0);
        model.RootBinderCreated.ShouldBeFalse();
        setupDelta.EnsureRootBinderCreatedCalls.ShouldBe(0);
        setupDelta.RootBinderCreations.ShouldBe(0);
        queryDelta.BoundNodeBindFallbacks.ShouldBe(0);
    }

}
