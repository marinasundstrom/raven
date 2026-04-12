using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class SemanticModelCachingTests : CompilationTestBase
{
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
        typeof(CompilerSetupInstrumentation).GetMethod("RecordEnsureSourceDeclarationsCompleteCall", BindingFlags.Instance | BindingFlags.NonPublic)!.Invoke(setup, []);
        typeof(CompilerSetupInstrumentation).GetMethod("RecordEnsureDeclarationsCall", BindingFlags.Instance | BindingFlags.NonPublic)!.Invoke(setup, []);
        typeof(CompilerSetupInstrumentation).GetMethod("RecordDeclarationPass", BindingFlags.Instance | BindingFlags.NonPublic)!.Invoke(setup, []);
        typeof(CompilerSetupInstrumentation).GetMethod("RecordEnsureRootBinderCreatedCall", BindingFlags.Instance | BindingFlags.NonPublic)!.Invoke(setup, []);
        typeof(CompilerSetupInstrumentation).GetMethod("RecordRootBinderCreated", BindingFlags.Instance | BindingFlags.NonPublic)!.Invoke(setup, []);

        instrumentation.Setup.Reset();

        Assert.Equal(0, instrumentation.Setup.EnsureSemanticModelsCreatedCalls);
        Assert.Equal(0, instrumentation.Setup.SemanticModelsCreated);
        Assert.Equal(0, instrumentation.Setup.EnsureSourceDeclarationsCompleteCalls);
        Assert.Equal(0, instrumentation.Setup.EnsureDeclarationsCalls);
        Assert.Equal(0, instrumentation.Setup.DeclarationPasses);
        Assert.Equal(0, instrumentation.Setup.EnsureRootBinderCreatedCalls);
        Assert.Equal(0, instrumentation.Setup.RootBinderCreations);
    }
}
