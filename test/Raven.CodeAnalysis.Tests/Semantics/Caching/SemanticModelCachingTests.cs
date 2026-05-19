using System.Linq;
using System.Reflection;
using System.Threading.Tasks;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class SemanticModelCachingTests : CompilationTestBase
{
    [Fact]
    public void GetSemanticModel_ReturnsStableInstanceUnderConcurrentQueries()
    {
        var tree = SyntaxTree.ParseText("""
class C {
    func Test() -> int {
        1
    }
}
""");
        var compilation = CreateCompilation(tree);
        var models = new SemanticModel[128];

        Parallel.For(0, models.Length, i =>
        {
            models[i] = compilation.GetSemanticModel(tree);
        });

        var first = models[0];
        Assert.NotNull(first);
        Assert.All(models, model => Assert.Same(first, model));
    }

    [Fact]
    public void RepeatedCachedNodeLookup_DoesNotRebindContextualRoot()
    {
        var code = """
class C {
    func Test(flag: bool) {
        if flag {
            ()
        }
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var ifStatement = tree.GetRoot().DescendantNodes().OfType<IfStatementSyntax>().First();
        var condition = ifStatement.Condition;

        var first = model.GetBoundNode(condition);

        Assert.NotNull(model.TryGetCachedBoundNode(condition));
        Assert.NotNull(model.TryGetCachedBoundNode(ifStatement));

        model.RemoveCachedBoundNode(ifStatement);
        Assert.Null(model.TryGetCachedBoundNode(ifStatement));

        var second = model.GetBoundNode(condition);

        Assert.Same(first, second);
        Assert.Null(model.TryGetCachedBoundNode(ifStatement));
    }

    [Fact]
    public void BinderReentryInstrumentation_TracksRepeatedRootBinding()
    {
        var code = """
class C {
    func Test(flag: bool) {
        if flag {
            ()
        }
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var instrumentation = new PerformanceInstrumentation();
        var options = new CompilationOptions(
            OutputKind.DynamicallyLinkedLibrary,
            performanceInstrumentation: instrumentation);
        var compilation = CreateCompilation(tree, options: options);
        var model = compilation.GetSemanticModel(tree);
        var ifStatement = tree.GetRoot().DescendantNodes().OfType<IfStatementSyntax>().First();

        _ = model.GetBoundNode(ifStatement);
        model.RemoveCachedBoundNode(ifStatement);
        _ = model.GetBoundNode(ifStatement);

        var counters = instrumentation.BinderReentry;
        Assert.True(counters.TotalInvocations >= 2);
        Assert.True(counters.TotalRepeatedNodeInvocations >= 1);
        Assert.True(counters.TotalBindExecutions >= 2);
        Assert.True(counters.GetBindExecutionCount(ifStatement) >= 2);
    }

    [Fact]
    public void ExplicitMemberNameLookup_ReusesParentMemberAccessBinding()
    {
        var code = """
class DomainError(Message: string)

class C {
    func Test(items: ItemExtensions) {
        val first = items.FirstOrError(() => DomainError("Wow"))
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var instrumentation = new PerformanceInstrumentation();
        var options = new CompilationOptions(
            OutputKind.DynamicallyLinkedLibrary,
            performanceInstrumentation: instrumentation);
        var compilation = CreateCompilation(tree, options: options);
        var model = compilation.GetSemanticModel(tree);
        var memberAccess = tree.GetRoot().DescendantNodes().OfType<MemberAccessExpressionSyntax>().Single();
        var memberName = (IdentifierNameSyntax)memberAccess.Name;

        var first = model.GetBoundNode(memberName);
        var second = model.GetBoundNode(memberName);

        Assert.Same(first, second);
        Assert.Equal(0, instrumentation.BinderReentry.GetBindExecutionCount(memberName));
        Assert.True(instrumentation.BinderReentry.GetBindExecutionCount(memberAccess) >= 1);
    }

    [Fact]
    public void TryGetCachedSymbolInfo_ReusesCachedInvocationSymbolWithoutBinding()
    {
        var code = """
class C {
    func Test() {
        Print()
    }

    func Print() {
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var instrumentation = new PerformanceInstrumentation();
        var options = new CompilationOptions(
            OutputKind.DynamicallyLinkedLibrary,
            performanceInstrumentation: instrumentation);
        var compilation = CreateCompilation(tree, options: options);
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single();
        var identifier = (IdentifierNameSyntax)invocation.Expression;

        var warmed = model.GetSymbolInfo(invocation);
        Assert.IsAssignableFrom<IMethodSymbol>(warmed.Symbol);

        instrumentation.BinderReentry.Reset();

        Assert.True(model.TryGetCachedSymbolInfo(identifier, out var cached));
        Assert.IsAssignableFrom<IMethodSymbol>(cached.Symbol);
        Assert.Equal(0, instrumentation.BinderReentry.TotalBindExecutions);
    }

    [Fact]
    public void TryGetAvailableSymbolInfo_DoesNotBindColdInvocation()
    {
        var code = """
class C {
    func Test() {
        Print()
    }

    func Print() {
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var instrumentation = new PerformanceInstrumentation();
        var options = new CompilationOptions(
            OutputKind.DynamicallyLinkedLibrary,
            performanceInstrumentation: instrumentation);
        var compilation = CreateCompilation(tree, options: options);
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single();
        var identifier = (IdentifierNameSyntax)invocation.Expression;

        instrumentation.BinderReentry.Reset();
        var before = instrumentation.SemanticQuery.CaptureSnapshot();

        Assert.False(model.TryGetAvailableSymbolInfo(identifier, out _));

        var after = instrumentation.SemanticQuery.CaptureSnapshot();
        var delta = SemanticQueryInstrumentation.Subtract(after, before);
        Assert.Equal(0, instrumentation.BinderReentry.TotalBindExecutions);
        Assert.Equal(0, delta.SymbolInfoBinderFallbacks);
        Assert.Equal(0, delta.BoundNodeBindFallbacks);
    }

    [Fact]
    public void TryGetAvailableSymbolInfo_ResolvesFunctionExpressionParameterReferenceFromCompilerState()
    {
        var code = """
func Apply(value: int, transform: int -> int) -> int {
    transform(value)
}

func Main() -> int {
    Apply(1, value => value + 1)
}
""";

        var tree = SyntaxTree.ParseText(code);
        var instrumentation = new PerformanceInstrumentation();
        var options = new CompilationOptions(
            OutputKind.DynamicallyLinkedLibrary,
            performanceInstrumentation: instrumentation);
        var compilation = CreateCompilation(tree, options: options);
        var model = compilation.GetSemanticModel(tree);
        var valueReference = tree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Last(static identifier => identifier.Identifier.ValueText == "value");

        compilation.EnsureSourceDeclarationsComplete();
        instrumentation.BinderReentry.Reset();
        var before = instrumentation.SemanticQuery.CaptureSnapshot();

        Assert.True(model.TryGetAvailableSymbolInfo(valueReference, out var info));

        var after = instrumentation.SemanticQuery.CaptureSnapshot();
        var delta = SemanticQueryInstrumentation.Subtract(after, before);
        var parameter = Assert.IsAssignableFrom<IParameterSymbol>(info.Symbol);
        Assert.Equal("value", parameter.Name);
        Assert.Equal("int", parameter.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.Equal(0, instrumentation.BinderReentry.TotalBindExecutions);
        Assert.Equal(0, delta.SymbolInfoBinderFallbacks);
        Assert.Equal(0, delta.BoundNodeBindFallbacks);
    }

    [Fact]
    public void TryGetAvailableSymbolInfo_DoesNotReturnUnsubstitutedGenericFunctionExpressionParameterReference()
    {
        var code = """
import System.Linq.*

func Main() {
    val values = [1, 2, 3]
    val projected = values.Select(value => value + 1)
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var valueReference = tree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Last(static identifier => identifier.Identifier.ValueText == "value");

        if (model.TryGetAvailableSymbolInfo(valueReference, out var info) &&
            info.Symbol is IParameterSymbol parameter)
        {
            Assert.DoesNotContain("TSource", parameter.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        }
    }

    [Fact]
    public void GetSymbolInfo_UsesAvailableFunctionExpressionParameterReferenceAfterDeclarationWarmup()
    {
        var code = """
import System.*
import System.Linq.*
import System.Collections.Generic.*

class FuelConsumptionRecord(var DistanceDrivenKm: double)

class C {
    func Run(samples: IEnumerable<FuelConsumptionRecord>) -> double {
        samples.Sum(entry => entry.DistanceDrivenKm)
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var instrumentation = new PerformanceInstrumentation();
        var options = new CompilationOptions(
            OutputKind.DynamicallyLinkedLibrary,
            performanceInstrumentation: instrumentation);
        var compilation = CreateCompilation(tree, options: options);
        var model = compilation.GetSemanticModel(tree);
        var valueReference = tree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Last(static identifier => identifier.Identifier.ValueText == "entry");

        instrumentation.BinderReentry.Reset();
        var before = instrumentation.SemanticQuery.CaptureSnapshot();

        var info = model.GetSymbolInfo(valueReference);

        var after = instrumentation.SemanticQuery.CaptureSnapshot();
        var delta = SemanticQueryInstrumentation.Subtract(after, before);
        var parameter = Assert.IsAssignableFrom<IParameterSymbol>(info.Symbol);
        Assert.Equal("entry", parameter.Name);
        Assert.Equal("FuelConsumptionRecord", parameter.Type.Name);
        Assert.Equal(0, instrumentation.BinderReentry.TotalBindExecutions);
        Assert.Equal(0, delta.SymbolInfoBinderFallbacks);
        Assert.Equal(0, delta.BoundNodeBindFallbacks);
    }

    [Fact]
    public void TryGetAvailableSymbolInfo_CachesLocalDeclarationSymbolInfo()
    {
        var code = """
class C {
    func Test() {
        val scope: string = ""
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var instrumentation = new PerformanceInstrumentation();
        var options = new CompilationOptions(
            OutputKind.DynamicallyLinkedLibrary,
            performanceInstrumentation: instrumentation);
        var compilation = CreateCompilation(tree, options: options);
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot()
            .DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(static declarator => declarator.Identifier.ValueText == "scope");

        Assert.True(model.TryGetAvailableSymbolInfo(declarator, out var first));
        Assert.IsAssignableFrom<ILocalSymbol>(first.Symbol);

        instrumentation.BinderReentry.Reset();
        var before = instrumentation.SemanticQuery.CaptureSnapshot();

        Assert.True(model.TryGetAvailableSymbolInfo(declarator, out var second));

        var after = instrumentation.SemanticQuery.CaptureSnapshot();
        var delta = SemanticQueryInstrumentation.Subtract(after, before);
        Assert.IsAssignableFrom<ILocalSymbol>(second.Symbol);
        Assert.Equal(0, instrumentation.BinderReentry.TotalBindExecutions);
        Assert.Equal(1, delta.SymbolInfoCacheHits);
        Assert.Equal(0, delta.SymbolInfoBinderFallbacks);
        Assert.Equal(0, delta.BoundNodeBindFallbacks);
    }

    [Fact]
    public void GetDeclaredSymbol_CachesLocalDeclarationSymbolInfo()
    {
        var code = """
class C {
    func CreateScope() -> string => ""

    func Test() {
        val scope = CreateScope()
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var instrumentation = new PerformanceInstrumentation();
        var options = new CompilationOptions(
            OutputKind.DynamicallyLinkedLibrary,
            performanceInstrumentation: instrumentation);
        var compilation = CreateCompilation(tree, options: options);
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot()
            .DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(static declarator => declarator.Identifier.ValueText == "scope");

        Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarator));

        instrumentation.BinderReentry.Reset();
        var before = instrumentation.SemanticQuery.CaptureSnapshot();

        Assert.True(model.TryGetAvailableSymbolInfo(declarator, out var cached));

        var after = instrumentation.SemanticQuery.CaptureSnapshot();
        var delta = SemanticQueryInstrumentation.Subtract(after, before);
        Assert.IsAssignableFrom<ILocalSymbol>(cached.Symbol);
        Assert.Equal(0, instrumentation.BinderReentry.TotalBindExecutions);
        Assert.Equal(1, delta.SymbolInfoCacheHits);
        Assert.Equal(0, delta.SymbolInfoBinderFallbacks);
        Assert.Equal(0, delta.BoundNodeBindFallbacks);
    }

    [Fact]
    public void TryGetAvailableTypeInfo_DoesNotBindColdInvocation()
    {
        var code = """
class C {
    func Test() {
        Print()
    }

    func Print() {
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var instrumentation = new PerformanceInstrumentation();
        var options = new CompilationOptions(
            OutputKind.DynamicallyLinkedLibrary,
            performanceInstrumentation: instrumentation);
        var compilation = CreateCompilation(tree, options: options);
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single();

        instrumentation.BinderReentry.Reset();
        var before = instrumentation.SemanticQuery.CaptureSnapshot();

        Assert.False(model.TryGetAvailableTypeInfo(invocation, out _));

        var after = instrumentation.SemanticQuery.CaptureSnapshot();
        var delta = SemanticQueryInstrumentation.Subtract(after, before);
        Assert.Equal(0, instrumentation.BinderReentry.TotalBindExecutions);
        Assert.Equal(0, delta.TypeInfoBoundFallbacks);
        Assert.Equal(0, delta.BoundNodeBindFallbacks);
    }

    [Fact]
    public void GetDeclaredSymbol_InfersExplicitGenericMetadataInvocationFromAvailableInitializer()
    {
        var code = """
import System.Text.Json.*

func Main() -> () {
    val str = "{}"
    val options = JsonSerializerOptions()
    val obj = JsonSerializer.Deserialize<Foo>(str, options)
}

record Foo(Name: string)
""";

        var tree = SyntaxTree.ParseText(code);
        var instrumentation = new PerformanceInstrumentation();
        var options = new CompilationOptions(
            OutputKind.ConsoleApplication,
            performanceInstrumentation: instrumentation);
        var compilation = CreateCompilation(tree, options: options);
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot()
            .DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(static declarator => declarator.Identifier.ValueText == "obj");

        var local = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarator));

        Assert.Equal("Foo", local.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void GetDeclaredSymbol_InfersChainedJsonSerializerGenericInvocationInitializers(bool topLevel)
    {
        var body = """
import System.Text.Json.*

val foo = Foo("Marina")
val options = JsonSerializerOptions()
val str = JsonSerializer.Serialize(foo, options)
val obj = JsonSerializer.Deserialize<Foo>(str, options)

record Foo(Name: string)
""";
        var code = topLevel
            ? body
            : body.Replace(
                """
val foo = Foo("Marina")
val options = JsonSerializerOptions()
val str = JsonSerializer.Serialize(foo, options)
val obj = JsonSerializer.Deserialize<Foo>(str, options)
""",
                """
func Main() -> () {
    val foo = Foo("Marina")
    val options = JsonSerializerOptions()
    val str = JsonSerializer.Serialize(foo, options)
    val obj = JsonSerializer.Deserialize<Foo>(str, options)
}
""");

        var tree = SyntaxTree.ParseText(code);
        var instrumentation = new PerformanceInstrumentation();
        var options = new CompilationOptions(
            OutputKind.ConsoleApplication,
            performanceInstrumentation: instrumentation);
        var compilation = CreateCompilation(tree, options: options);
        var model = compilation.GetSemanticModel(tree);
        var declarators = tree.GetRoot()
            .DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .ToArray();

        var str = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(
            declarators.Single(static declarator => declarator.Identifier.ValueText == "str")));
        var obj = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(
            declarators.Single(static declarator => declarator.Identifier.ValueText == "obj")));

        Assert.Equal("string", str.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.Equal("Foo", obj.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
    }

    [Fact]
    public void GetSymbolInfo_TopLevelGenericInvocation_SeesPrecedingInvocationLocalBeforeDiagnostics()
    {
        var code = """
import System.Text.Json.*

val foo = Foo("Marina")
val options = JsonSerializerOptions()
val str = JsonSerializer.Serialize(foo, options)
val obj = JsonSerializer.Deserialize<Foo>(str, options)

record Foo(Name: string)
""";

        var tree = SyntaxTree.ParseText(code);
        var instrumentation = new PerformanceInstrumentation();
        var options = new CompilationOptions(
            OutputKind.ConsoleApplication,
            performanceInstrumentation: instrumentation);
        var compilation = CreateCompilation(tree, options: options);
        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var deserializeInvocation = root
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(static invocation => invocation.Expression.ToString() == "JsonSerializer.Deserialize<Foo>");
        var strArgument = deserializeInvocation.ArgumentList.Arguments[0].Expression
            .DescendantNodesAndSelf()
            .OfType<IdentifierNameSyntax>()
            .Single(static identifier => identifier.Identifier.ValueText == "str");

        var invocationMethod = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(deserializeInvocation).Symbol);
        var strLocal = Assert.IsAssignableFrom<ILocalSymbol>(model.GetSymbolInfo(strArgument).Symbol);
        var diagnostics = compilation.GetDiagnostics();

        Assert.Equal("Deserialize", invocationMethod.Name);
        Assert.Equal("string", strLocal.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Id == CompilerDiagnostics.NoOverloadForMethod.Id);
        Assert.Empty(diagnostics.Where(static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error));
    }

    [Fact]
    public void GetSymbolInfo_TopLevelInvocationArgument_ResolvesPrecedingInvocationInitializedLocalCold()
    {
        var code = """
class Options {
}

record Foo(val Name: string)

func Test(foo: Foo, options: Options) -> string {
    "ok"
}

val foo = Foo("Foo")
val options = Options()
val str = Test(foo, options)
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var invocation = root
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(static candidate => candidate.Expression.ToString() == "Test");
        var fooArgument = invocation.ArgumentList.Arguments[0].Expression
            .DescendantNodesAndSelf()
            .OfType<IdentifierNameSyntax>()
            .Single(static identifier => identifier.Identifier.ValueText == "foo");
        var fooDeclarator = root
            .DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .First(static declarator => declarator.Identifier.ValueText == "foo");

        Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(fooDeclarator));
        var symbol = Assert.IsAssignableFrom<ILocalSymbol>(model.GetSymbolInfo(fooArgument).Symbol);

        Assert.Equal("foo", symbol.Name);
        Assert.Equal("Foo", symbol.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
    }

    [Fact]
    public void GetDeclaredSymbol_TopLevelLocal_InLibraryOutputStillBindsForSemanticQueries()
    {
        var code = """
val foo = 1
val bar = foo
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(
            tree,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var fooDeclarator = root
            .DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .First(static declarator => declarator.Identifier.ValueText == "foo");
        var fooReference = root
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(static identifier => identifier.Identifier.ValueText == "foo");

        var declared = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(fooDeclarator));
        var referenced = Assert.IsAssignableFrom<ILocalSymbol>(model.GetSymbolInfo(fooReference).Symbol);

        Assert.Same(declared, referenced);
    }

    [Fact]
    public void GetDiagnostics_InvalidInvocationAfterSemanticQuery_StillReportsNoOverload()
    {
        var code = """
func Main() -> () {
    Print()
}

func Print(value: string) -> () {
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(static invocation => invocation.Expression.ToString() == "Print");

        _ = model.GetSymbolInfo(invocation);

        var diagnostics = compilation.GetDiagnostics();
        var diagnostic = Assert.Single(diagnostics.Where(static diagnostic =>
            diagnostic.Id == CompilerDiagnostics.NoOverloadForMethod.Id));

        Assert.Contains("Print", diagnostic.GetMessage(), StringComparison.Ordinal);
    }

    [Fact]
    public void GetDiagnostics_InvalidTopLevelGenericInvocationAfterSemanticQuery_StillReportsNoOverload()
    {
        var code = """
import System.Text.Json.*

val foo = Foo("Marina")
val options = JsonSerializerOptions()
val str = JsonSerializer.Serialize(foo, options)
val obj = JsonSerializer.Deserialize<Foo>(options, str)

record Foo(Name: string)
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(static invocation => invocation.Expression.ToString() == "JsonSerializer.Deserialize<Foo>");

        _ = model.GetSymbolInfo(invocation);

        var diagnostics = compilation.GetDiagnostics();
        var diagnostic = Assert.Single(diagnostics.Where(static diagnostic =>
            diagnostic.Id == CompilerDiagnostics.NoOverloadForMethod.Id));

        Assert.Contains("Deserialize", diagnostic.GetMessage(), StringComparison.Ordinal);
    }

    [Fact]
    public void GetDeclaredSymbol_InfersInvocationChainInitializerWithoutBindingWholeBody()
    {
        var code = """
func Normalize(kind: string) -> string {
    val normalized = kind.Trim().ToLowerInvariant()
    return normalized
}
""";

        var tree = SyntaxTree.ParseText(code);
        var instrumentation = new PerformanceInstrumentation();
        var options = new CompilationOptions(
            OutputKind.DynamicallyLinkedLibrary,
            performanceInstrumentation: instrumentation);
        var compilation = CreateCompilation(tree, options: options);
        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var declarator = root
            .DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(static declarator => declarator.Identifier.ValueText == "normalized");
        var returnStatement = root.DescendantNodes().OfType<ReturnStatementSyntax>().Single();

        var local = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarator));

        Assert.Equal("string", local.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.Null(model.TryGetCachedBoundNode(returnStatement));
    }

    [Fact]
    public void TryGetAvailableInvocationCandidates_ResolvesColdFunctionWithoutBinding()
    {
        var code = """
func Foo() -> () {
}

func Foo(value: int) -> () {
}

func Main() -> () {
    Foo()
}
""";

        var tree = SyntaxTree.ParseText(code);
        var instrumentation = new PerformanceInstrumentation();
        var options = new CompilationOptions(
            OutputKind.DynamicallyLinkedLibrary,
            performanceInstrumentation: instrumentation);
        var compilation = CreateCompilation(tree, options: options);
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single();

        instrumentation.BinderReentry.Reset();
        var setupBefore = instrumentation.Setup.CaptureSnapshot();

        Assert.True(model.TryGetAvailableInvocationCandidates(invocation, out var methods));
        var setupDelta = CompilerSetupInstrumentation.Subtract(
            instrumentation.Setup.CaptureSnapshot(),
            setupBefore);
        Assert.Equal(2, methods.Length);
        Assert.All(methods, method => Assert.Equal("Foo", method.Name));
        Assert.Equal(0, setupDelta.EnsureSourceDeclarationsCompleteCalls);
        Assert.False(compilation.SourceDeclarationsComplete);
        Assert.Equal(0, instrumentation.BinderReentry.TotalBindExecutions);
    }

    [Fact]
    public void TryGetAvailableInvocationCandidates_DoesNotResolveLocalFunctionFromUnrelatedBody()
    {
        var code = """
func Outer() -> () {
    func Hidden() -> () {
    }
}

func Main() -> () {
    Hidden()
}
""";

        var tree = SyntaxTree.ParseText(code);
        var instrumentation = new PerformanceInstrumentation();
        var options = new CompilationOptions(
            OutputKind.DynamicallyLinkedLibrary,
            performanceInstrumentation: instrumentation);
        var compilation = CreateCompilation(tree, options: options);
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single();

        instrumentation.BinderReentry.Reset();

        Assert.False(model.TryGetAvailableInvocationCandidates(invocation, out var methods));
        Assert.True(methods.IsDefaultOrEmpty);
        Assert.Equal(0, instrumentation.BinderReentry.TotalBindExecutions);
    }

    [Fact]
    public void TryGetAvailableExtensionInvocationCandidates_DoesNotNameScanColdImports()
    {
        var code = """
import System.Linq.*

class C {
    func Test() {
        val projection = missing.Where(value => true)
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var instrumentation = new PerformanceInstrumentation();
        var options = new CompilationOptions(
            OutputKind.DynamicallyLinkedLibrary,
            performanceInstrumentation: instrumentation);
        var compilation = CreateCompilation(tree, options: options);
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single();

        instrumentation.BinderReentry.Reset();
        var before = instrumentation.SemanticQuery.CaptureSnapshot();

        Assert.False(model.TryGetAvailableExtensionInvocationCandidates(invocation, out var methods));
        Assert.True(methods.IsDefaultOrEmpty);

        var after = instrumentation.SemanticQuery.CaptureSnapshot();
        var delta = SemanticQueryInstrumentation.Subtract(after, before);
        Assert.Equal(0, instrumentation.BinderReentry.TotalBindExecutions);
        Assert.Equal(0, delta.SymbolInfoBinderFallbacks);
        Assert.Equal(0, delta.BoundNodeBindFallbacks);
    }

    [Fact]
    public void TryGetAvailableInvocationCandidates_ResolvesColdStandardUnionConstructorsWithoutBinding()
    {
        var code = """
func Main() -> () {
    val x = Foo()
}

union Foo(int | string)
""";

        var tree = SyntaxTree.ParseText(code);
        var instrumentation = new PerformanceInstrumentation();
        var options = new CompilationOptions(
            OutputKind.DynamicallyLinkedLibrary,
            performanceInstrumentation: instrumentation);
        var compilation = CreateCompilation(tree, options: options);
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single();

        instrumentation.BinderReentry.Reset();

        Assert.True(model.TryGetAvailableInvocationCandidates(invocation, out var methods));
        Assert.Equal(2, methods.Length);
        Assert.All(methods, method => Assert.Equal(MethodKind.Constructor, method.MethodKind));
        Assert.Contains(methods, method => method.Parameters.Single().Type.SpecialType == SpecialType.System_Int32);
        Assert.Contains(methods, method => method.Parameters.Single().Type.SpecialType == SpecialType.System_String);
        Assert.Equal(0, instrumentation.BinderReentry.TotalBindExecutions);
    }

    [Fact]
    public void TryGetAvailableTypeInfo_ReusesCachedMemberAccessTypeWithoutBinding()
    {
        var code = """
class Item {
    val DistanceDrivenKm: decimal = 0m
}

class C {
    func Test(entry: Item) {
        val distance = entry.DistanceDrivenKm
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var instrumentation = new PerformanceInstrumentation();
        var options = new CompilationOptions(
            OutputKind.DynamicallyLinkedLibrary,
            performanceInstrumentation: instrumentation);
        var compilation = CreateCompilation(tree, options: options);
        var model = compilation.GetSemanticModel(tree);
        var memberAccess = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single(static memberAccess => memberAccess.Name.Identifier.ValueText == "DistanceDrivenKm");

        var warmed = model.GetTypeInfo(memberAccess);
        Assert.NotNull(warmed.Type);

        instrumentation.BinderReentry.Reset();

        Assert.True(model.TryGetAvailableTypeInfo(memberAccess, out var cached));
        Assert.NotNull(cached.Type);
        Assert.Equal(0, instrumentation.BinderReentry.TotalBindExecutions);
    }

    [Fact]
    public void TryGetCachedSymbolInfo_ReusesCachedMemberAccessSymbolWithoutBinding()
    {
        var code = """
class Counter {
    private var count: int = 0

    func Test() {
        val current = self.count
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var instrumentation = new PerformanceInstrumentation();
        var options = new CompilationOptions(
            OutputKind.DynamicallyLinkedLibrary,
            performanceInstrumentation: instrumentation);
        var compilation = CreateCompilation(tree, options: options);
        var model = compilation.GetSemanticModel(tree);
        var memberAccess = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single(static memberAccess => memberAccess.Name.Identifier.ValueText == "count");
        var memberName = (IdentifierNameSyntax)memberAccess.Name;

        var warmed = model.GetSymbolInfo(memberName);
        Assert.IsAssignableFrom<IPropertySymbol>(warmed.Symbol);

        instrumentation.BinderReentry.Reset();

        Assert.True(model.TryGetCachedSymbolInfo(memberName, out var cached));
        Assert.IsAssignableFrom<IPropertySymbol>(cached.Symbol);
        Assert.Equal(0, instrumentation.BinderReentry.TotalBindExecutions);
    }

    [Fact]
    public void GetSymbolInfo_MetadataInvocationWithInterpolatedString_ChoosesStringOverloadWithoutBindingBody()
    {
        var code = """
import System.*

func Main() -> unit {
    val value = 42
    Console.WriteLine("value: ${value.ToString()}")
}
""";

        var tree = SyntaxTree.ParseText(code);
        var instrumentation = new PerformanceInstrumentation();
        var options = new CompilationOptions(
            OutputKind.ConsoleApplication,
            performanceInstrumentation: instrumentation);
        var compilation = CreateCompilation(tree, options: options);
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(static invocation => invocation.Expression.ToString() == "Console.WriteLine");

        instrumentation.BinderReentry.Reset();
        var before = instrumentation.SemanticQuery.CaptureSnapshot();

        var symbolInfo = model.GetSymbolInfo(invocation);

        var after = instrumentation.SemanticQuery.CaptureSnapshot();
        var delta = SemanticQueryInstrumentation.Subtract(after, before);
        var method = Assert.IsAssignableFrom<IMethodSymbol>(symbolInfo.Symbol);
        Assert.Equal("WriteLine", method.Name);
        var parameter = Assert.Single(method.Parameters);
        Assert.Equal(SpecialType.System_String, parameter.Type.GetPlainType().SpecialType);
        Assert.Equal(0, instrumentation.BinderReentry.TotalBindExecutions);
        Assert.Equal(0, delta.BoundNodeBindFallbacks);
        Assert.Equal(0, delta.SymbolInfoBinderFallbacks);
    }

    [Fact]
    public void BinderReentryInstrumentation_Reset_ClearsRecordedCounts()
    {
        var code = """
class C {
    func Test(flag: bool) {
        if flag {
            ()
        }
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var instrumentation = new PerformanceInstrumentation();
        var options = new CompilationOptions(
            OutputKind.DynamicallyLinkedLibrary,
            performanceInstrumentation: instrumentation);
        var compilation = CreateCompilation(tree, options: options);
        var model = compilation.GetSemanticModel(tree);
        var ifStatement = tree.GetRoot().DescendantNodes().OfType<IfStatementSyntax>().First();

        _ = model.GetBoundNode(ifStatement);
        Assert.True(instrumentation.BinderReentry.TotalInvocations > 0);

        instrumentation.BinderReentry.Reset();

        Assert.Equal(0, instrumentation.BinderReentry.TotalInvocations);
        Assert.Equal(0, instrumentation.BinderReentry.TotalCacheHits);
        Assert.Equal(0, instrumentation.BinderReentry.TotalBindExecutions);
        Assert.Equal(0, instrumentation.BinderReentry.GetBindExecutionCount(ifStatement));
    }

    [Fact]
    public void ConstructedMethodSymbol_CachesSubstitutedSignatureTypes()
    {
        var code = """
class Box<T> {
}

class C {
    func Echo<T>(value: Box<T>) -> Box<T> => value

    func Test(value: Box<int>) {
        Echo<int>(value)
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single();

        var method = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol);
        var parameter = Assert.Single(method.Parameters);

        Assert.Same(parameter.Type, parameter.Type);
        Assert.Same(method.ReturnType, method.ReturnType);
    }

    [Fact]
    public void SubstitutedMethodSymbol_CachesSubstitutedSignatureTypes()
    {
        var code = """
class Box<T> {
    func Echo(value: Box<T>) -> Box<T> => value
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var declaration = tree.GetRoot()
            .DescendantNodes()
            .OfType<ClassDeclarationSyntax>()
            .Single();
        var box = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(declaration));
        var constructedBox = box.Construct(compilation.GetSpecialType(SpecialType.System_Int32));
        var method = Assert.IsAssignableFrom<IMethodSymbol>(
            Assert.Single(constructedBox.GetMembers("Echo")));
        var parameter = Assert.Single(method.Parameters);

        Assert.Same(parameter.Type, parameter.Type);
        Assert.Same(method.ReturnType, method.ReturnType);
    }

    [Fact]
    public void MacroInstrumentation_Reset_ClearsRecordedCounts()
    {
        var instrumentation = new PerformanceInstrumentation();
        var macros = instrumentation.Macros;
        typeof(MacroInstrumentation).GetMethod("RecordAttachedExpansionInvocation", BindingFlags.Instance | BindingFlags.NonPublic)!.Invoke(macros, []);
        typeof(MacroInstrumentation).GetMethod("RecordFreestandingExpansionInvocation", BindingFlags.Instance | BindingFlags.NonPublic)!.Invoke(macros, []);
        typeof(MacroInstrumentation).GetMethod("RecordShadowOutputCacheHit", BindingFlags.Instance | BindingFlags.NonPublic)!.Invoke(macros, []);
        typeof(MacroInstrumentation).GetMethod("RecordShadowOutputCacheMiss", BindingFlags.Instance | BindingFlags.NonPublic)!.Invoke(macros, []);
        typeof(MacroInstrumentation).GetMethod("RecordConsumerRefreshRun", BindingFlags.Instance | BindingFlags.NonPublic)!.Invoke(macros, [2]);

        instrumentation.Macros.Reset();

        Assert.Equal(0, instrumentation.Macros.AttachedExpansionInvocations);
        Assert.Equal(0, instrumentation.Macros.FreestandingExpansionInvocations);
        Assert.Equal(0, instrumentation.Macros.ShadowOutputCacheHits);
        Assert.Equal(0, instrumentation.Macros.ShadowOutputCacheMisses);
        Assert.Equal(0, instrumentation.Macros.ConsumerRefreshRuns);
        Assert.Equal(0, instrumentation.Macros.ConsumerRefreshProjectUpdates);
    }

    [Fact]
    public void CompilerSetupInstrumentation_Reset_ClearsRecordedCounts()
    {
        var instrumentation = new PerformanceInstrumentation();
        var setup = instrumentation.Setup;
        typeof(CompilerSetupInstrumentation).GetMethod("RecordEnsureSemanticModelsCreatedCall", BindingFlags.Instance | BindingFlags.NonPublic)!.Invoke(setup, []);
        typeof(CompilerSetupInstrumentation).GetMethod("RecordSemanticModelCreated", BindingFlags.Instance | BindingFlags.NonPublic)!.Invoke(setup, []);
        typeof(CompilerSetupInstrumentation).GetMethod("RecordEnsureSourceDeclarationsDeclaredCall", BindingFlags.Instance | BindingFlags.NonPublic)!.Invoke(setup, []);
        typeof(CompilerSetupInstrumentation).GetMethod("RecordEnsureSourceDeclarationsCompleteCall", BindingFlags.Instance | BindingFlags.NonPublic)!.Invoke(setup, []);
        typeof(CompilerSetupInstrumentation).GetMethod("RecordEnsureDeclarationsCall", BindingFlags.Instance | BindingFlags.NonPublic)!.Invoke(setup, []);
        typeof(CompilerSetupInstrumentation).GetMethod("RecordDeclarationPass", BindingFlags.Instance | BindingFlags.NonPublic)!.Invoke(setup, []);
        typeof(CompilerSetupInstrumentation).GetMethod("RecordEnsureRootBinderCreatedCall", BindingFlags.Instance | BindingFlags.NonPublic)!.Invoke(setup, []);
        typeof(CompilerSetupInstrumentation).GetMethod("RecordRootBinderCreated", BindingFlags.Instance | BindingFlags.NonPublic)!.Invoke(setup, []);

        instrumentation.Setup.Reset();

        Assert.Equal(0, instrumentation.Setup.EnsureSemanticModelsCreatedCalls);
        Assert.Equal(0, instrumentation.Setup.SemanticModelsCreated);
        Assert.Equal(0, instrumentation.Setup.EnsureSourceDeclarationsDeclaredCalls);
        Assert.Equal(0, instrumentation.Setup.EnsureSourceDeclarationsCompleteCalls);
        Assert.Equal(0, instrumentation.Setup.EnsureDeclarationsCalls);
        Assert.Equal(0, instrumentation.Setup.DeclarationPasses);
        Assert.Equal(0, instrumentation.Setup.EnsureRootBinderCreatedCalls);
        Assert.Equal(0, instrumentation.Setup.RootBinderCreations);
    }
}
