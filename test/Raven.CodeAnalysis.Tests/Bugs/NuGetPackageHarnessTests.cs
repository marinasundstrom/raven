using System;
using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Bugs;

public sealed class NuGetPackageHarnessTests
{
    [Fact]
    public void NuGetHarness_CanCompilePackageBackedSource()
    {
        using var harness = NuGetProjectTestHarness.Create(
            """
import System.*
import System.Console.*
import System.Reactive.*

static func Main() -> unit {
    val observer = Observer.Create<int>((value: int) => {
        WriteLine(value)
    })

    observer.OnNext(1)
    observer.OnCompleted()
}
""",
            [("System.Reactive", "6.1.0")]);

        var compilation = harness.GetCompilation();
        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void NuGetHarness_SystemReactiveObserverCreate_WithImplicitLambdaParameter_BindsLikeCSharp()
    {
        using var harness = NuGetProjectTestHarness.Create(
            """
import System.*
import System.Console.*
import System.Reactive.*

static func Main() -> unit {
    val observer = Observer.Create<int>(value => {
        WriteLine(value)
    })

    observer.OnNext(1)
    observer.OnCompleted()
}
""",
            [("System.Reactive", "6.1.0")]);

        var compilation = harness.GetCompilation();
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

        var tree = harness.GetSyntaxTree("src/main.rav");
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .First(inv => inv.Expression.ToString().Contains("Observer.Create", StringComparison.Ordinal));

        var model = compilation.GetSemanticModel(tree);
        var symbolInfo = model.GetSymbolInfo(invocation);
        Assert.NotNull(symbolInfo.Symbol);
        Assert.Contains("Create", symbolInfo.Symbol!.Name, StringComparison.Ordinal);

        var lambda = tree.GetRoot().DescendantNodes().OfType<SimpleFunctionExpressionSyntax>().Single();
        var lambdaType = model.GetTypeInfo(lambda);
        Assert.NotNull(lambdaType.ConvertedType);
        Assert.Contains("int -> ()", lambdaType.ConvertedType!.ToDisplayString(), StringComparison.Ordinal);
    }

    [Fact]
    public void NuGetHarness_SystemReactiveObserverInstanceMemberCompletion_ReturnsObserverMembers()
    {
        const string source = """
import System.*
import System.Console.*
import System.Reactive.*

static func Main() -> unit {
    val observer = Observer.Create<int>(value => {
        WriteLine(value)
    })

    observer.
}
""";

        using var harness = NuGetProjectTestHarness.Create(
            source,
            [("System.Reactive", "6.1.0")]);

        var compilation = harness.GetCompilation();
        var tree = harness.GetSyntaxTree("src/main.rav");
        var service = new CompletionService();
        var position = source.LastIndexOf("observer.", StringComparison.Ordinal) + "observer.".Length;
        var token = tree.GetRoot().FindToken(position - 1);
        var model = compilation.GetSemanticModel(tree);

        var providerException = Record.Exception(() => CompletionProvider.GetCompletions(token, model, position).ToList());
        Assert.Null(providerException);

        var items = service.GetCompletions(compilation, tree, position).ToList();

        Assert.Contains(items, item => item.DisplayText == "OnNext");
        Assert.Contains(items, item => item.DisplayText == "OnCompleted");
        Assert.DoesNotContain(items, item => item.DisplayText == "const");
    }
}
