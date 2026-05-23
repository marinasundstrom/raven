using System;
using System.IO;
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
    public void NuGetHarness_AvaloniaRefAssemblySignature_EmitsWithoutRuntimeTypeLoadCrash()
    {
        using var harness = NuGetProjectTestHarness.Create(
            """
import Avalonia.Controls.*

func CreateWindow() -> Window {
    return Window()
}
""",
            [("Avalonia", "11.3.16")],
            compilationOptions: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var compilation = harness.GetCompilation();
        using var peStream = new MemoryStream();
        EmitResult? emitResult = null;

        var exception = Record.Exception(() => emitResult = compilation.Emit(peStream));

        Assert.Null(exception);
        Assert.NotNull(emitResult);
        Assert.True(emitResult!.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
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
        Assert.DoesNotContain(items, item => item.DisplayText == "Subscribe");
    }

    [Fact]
    public void NuGetHarness_EfCoreDbSetPipeline_BindsQueryableChain()
    {
        using var harness = NuGetProjectTestHarness.Create(
            """
import System.*
import System.Linq.*
import System.Linq.Expressions.*
import Microsoft.EntityFrameworkCore.*

class User(var Id: int, var Name: string, var Age: int, var IsActive: bool)

class AppDbContext : DbContext {
    #pragma warning disable-next-line RAV9006
    var Users: DbSet<User>

    protected override func OnConfiguring(optionsBuilder: DbContextOptionsBuilder) {
        optionsBuilder.UseInMemoryDatabase("Raven.ExpressionTrees.Stage1")
    }
}

func Seed(db: AppDbContext) {
    db.Users.Add(.(1, "Ana", 29, true))
    db.Users.Add(.(2, "Bob", 17, true))
}

static func Main() -> unit {
    val db = AppDbContext()
    Seed(db)
    val minAge = 21
    val isActive: Expression<System.Func<User, bool>> = user => user.IsActive && user.Age >= minAge
    val query = db.Users
        |> Where(isActive)
        |> OrderBy(user => user.Name)
        |> Select(user => user.Name)
}
""",
            [
                ("Microsoft.EntityFrameworkCore", "10.0.0"),
                ("Microsoft.EntityFrameworkCore.InMemory", "10.0.0")
            ],
            compilationOptions: new CompilationOptions(OutputKind.ConsoleApplication));

        var ravenCorePath = Path.GetFullPath(Path.Combine(
            AppContext.BaseDirectory,
            "..",
            "..",
            "..",
            "..",
            "..",
            "src",
            "Raven.Core",
            "bin",
            "Debug",
            "net10.0",
            "Raven.Core.dll"));
        if (File.Exists(ravenCorePath))
        {
            harness.Workspace.TryApplyChanges(
                harness.Workspace.CurrentSolution.AddMetadataReference(
                    harness.ProjectId,
                    MetadataReference.CreateFromFile(ravenCorePath)));
        }

        var compilation = harness.GetCompilation();
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

        var tree = harness.GetSyntaxTree("src/main.rvn");
        var model = compilation.GetSemanticModel(tree);
        var pipelines = tree.GetRoot()
            .DescendantNodes()
            .OfType<InfixOperatorExpressionSyntax>()
            .Where(node => node.OperatorToken.Kind == SyntaxKind.PipeToken)
            .ToArray();

        Assert.Equal(3, pipelines.Length);

        var whereInvocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .First(node => node.Expression.ToString() == "Where");
        var whereTargetInfo = model.GetSymbolInfo(whereInvocation.Expression);
        var whereArgument = whereInvocation.ArgumentList.Arguments.Single().Expression;
        var whereArgumentInfo = model.GetSymbolInfo(whereArgument);
        var whereArgumentType = model.GetTypeInfo(whereArgument);

        Assert.Equal("isActive", whereArgumentInfo.Symbol?.Name);
        Assert.Equal("Expression", whereArgumentType.Type?.Name);

        foreach (var pipeline in pipelines)
        {
            var boundPipeline = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(pipeline));
            Assert.True(boundPipeline.Method.IsExtensionMethod);
            Assert.Equal("Queryable", boundPipeline.Method.ContainingType?.Name);
        }

        var whereSymbol = Assert.IsAssignableFrom<IMethodSymbol>(whereTargetInfo.Symbol);
        Assert.Equal("Where", whereSymbol.Name);
        Assert.Equal("Queryable", whereSymbol.ContainingType?.Name);

        Assert.Equal(["Select", "OrderBy", "Where"], pipelines
            .Select(pipeline => Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(pipeline)).Method.Name)
            .ToArray());
    }
}
