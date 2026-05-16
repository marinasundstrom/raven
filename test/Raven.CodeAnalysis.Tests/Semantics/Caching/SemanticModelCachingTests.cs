using System.Linq;
using System.Reflection;
using System.Threading.Tasks;

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
