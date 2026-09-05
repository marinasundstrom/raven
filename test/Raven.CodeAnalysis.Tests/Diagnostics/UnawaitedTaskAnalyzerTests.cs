using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public sealed class UnawaitedTaskAnalyzerTests : AnalyzerTestBase
{
    [Theory]
    [InlineData("Task", "Task.CompletedTask", false)]
    [InlineData("Task", "Task.CompletedTask", true)]
    [InlineData("Task<int>", "Task.FromResult(42)", false)]
    [InlineData("Task<int>", "Task.FromResult(42)", true)]
    [InlineData("ValueTask", "ValueTask.CompletedTask", false)]
    [InlineData("ValueTask", "ValueTask.CompletedTask", true)]
    [InlineData("ValueTask<int>", "ValueTask<int>(42)", false)]
    [InlineData("ValueTask<int>", "ValueTask<int>(42)", true)]
    public void IgnoredTaskCall_ReportsByDefault(string taskType, string value, bool isAsync)
    {
        var code = $$"""
import System.Threading.Tasks.*
func Test() -> {{taskType}} { return {{value}} }
{{(isAsync ? "async " : "")}}func Main() -> {{(isAsync ? "Task" : "()")}} {
    Test()
}
""";

        CreateAnalyzerVerifier<UnawaitedTaskAnalyzer>(code,
            expectedDiagnostics:
            [
                new DiagnosticResult(UnawaitedTaskAnalyzer.DiagnosticId).WithSpan(4, 5, 4, 11)
            ],
            disabledDiagnostics: [CompilerDiagnostics.AsyncLacksAwait.Id]).Verify();
    }

    [Theory]
    [InlineData("await Test()")]
    [InlineData("_ = Test()")]
    [InlineData("let pending = Test()\n    await pending")]
    public void HandledTask_DoesNotReport(string statement)
    {
        var code = $$"""
import System.Threading.Tasks.*
func Test() -> Task { return Task.CompletedTask }
async func Main() -> Task {
    {{statement}}
}
""";

        CreateAnalyzerVerifier<UnawaitedTaskAnalyzer>(code,
            disabledDiagnostics: [CompilerDiagnostics.AsyncLacksAwait.Id]).Verify();
    }

    [Theory]
    [InlineData("return Test()")]
    [InlineData("Test()")]
    public void ReturnedTask_DoesNotReport(string statement)
    {
        var code = $$"""
import System.Threading.Tasks.*
func Test() -> Task { return Task.CompletedTask }
func Forward() -> Task {
    {{statement}}
}
func Main() -> () { _ = Forward() }
""";

        CreateAnalyzerVerifier<UnawaitedTaskAnalyzer>(code).Verify();
    }

    [Fact]
    public void IgnoredTaskBeforeAwait_ReportsAtTheCall()
    {
        const string code = """
import System.Threading.Tasks.*
func Test() -> Task { return Task.CompletedTask }
async func Main() -> Task {
    Test()
    await Test()
}
""";

        CreateAnalyzerVerifier<UnawaitedTaskAnalyzer>(code,
            expectedDiagnostics:
            [
                new DiagnosticResult(UnawaitedTaskAnalyzer.DiagnosticId).WithSpan(4, 5, 4, 11)
            ]).Verify();
    }

    [Fact]
    public void IgnoredOrdinaryResult_DoesNotReport()
    {
        const string code = """
func Test() -> int { return 42 }
func Main() -> () { Test() }
""";

        CreateAnalyzerVerifier<UnawaitedTaskAnalyzer>(code).Verify();
    }
    [Theory]
    [InlineData("(Test())", 5)]
    [InlineData("if true { Test() }", 15)]
    public void WrappedOrNestedCall_Reports(string statement, int column)
    {
        var code = $$"""
import System.Threading.Tasks.*
func Test() -> Task { return Task.CompletedTask }
func Main() -> () {
    {{statement}}
}
""";

        CreateAnalyzerVerifier<UnawaitedTaskAnalyzer>(code,
            expectedDiagnostics:
            [
                new DiagnosticResult(UnawaitedTaskAnalyzer.DiagnosticId).WithSpan(4, column, 4, column + 6)
            ]).Verify();
    }

    [Fact]
    public void BuiltInAnalyzers_ReportUnawaitedCallByDefault()
    {
        const string code = """
import System.Threading.Tasks.*
func Test() -> Task { return Task.CompletedTask }
async func Main() -> Task {
    Test()
    await Test()
}
""";
        var workspace = RavenWorkspace.Create(targetFramework: TestTargetFramework.Default);
        var projectId = workspace.AddProject("UnawaitedTask");
        var project = workspace.CurrentSolution.GetProject(projectId)!.AddBuiltInAnalyzers();
        var documentId = DocumentId.CreateNew(projectId);
        var solution = project.Solution.AddDocument(documentId, "test.rvn", Raven.CodeAnalysis.Text.SourceText.From(code));
        workspace.TryApplyChanges(solution);

        var diagnostic = Assert.Single(workspace.GetDiagnostics(projectId)
            .Where(d => d.Id == UnawaitedTaskAnalyzer.DiagnosticId));
        Assert.Equal(DiagnosticSeverity.Warning, diagnostic.Severity);
        Assert.Equal(3, diagnostic.Location.GetLineSpan().StartLinePosition.Line);
    }

    [Theory]
    [InlineData(ReturnedValueHandlingMode.Off)]
    [InlineData(ReturnedValueHandlingMode.Full)]
    public void WarningIsIndependentOfReturnedValueHandling(ReturnedValueHandlingMode mode)
    {
        const string code = """
import System.Threading.Tasks.*
func Main() -> () {
    Task.Delay(1)
}
""";
        CreateAnalyzerVerifier<UnawaitedTaskAnalyzer>(code,
            expectedDiagnostics:
            [
                new DiagnosticResult(UnawaitedTaskAnalyzer.DiagnosticId).WithSpan(3, 5, 3, 18)
            ], returnedValueHandlingMode: mode).Verify();
    }

    [Fact]
    public void DiagnosticCanBeSuppressed()
    {
        const string code = """
import System.Threading.Tasks.*
func Main() -> () { Task.Delay(1) }
""";
        CreateAnalyzerVerifier<UnawaitedTaskAnalyzer>(code,
            specificDiagnosticOptions: new Dictionary<string, ReportDiagnostic>
            {
                [UnawaitedTaskAnalyzer.DiagnosticId] = ReportDiagnostic.Suppress
            }).Verify();
    }

    [Fact]
    public void ConditionalTaskCall_Reports()
    {
        const string code = """
import System.Threading.Tasks.*
class Worker {
    func Run() -> Task { return Task.CompletedTask }
}
func Run(worker: Worker?) -> () {
    worker?.Run()
}
func Main() -> () { Run(null) }
""";
        CreateAnalyzerVerifier<UnawaitedTaskAnalyzer>(code,
            expectedDiagnostics:
            [
                new DiagnosticResult(UnawaitedTaskAnalyzer.DiagnosticId).WithSpan(6, 5, 6, 18)
            ]).Verify();
    }

    [Fact]
    public void AsyncLambda_IgnoredTaskCall_Reports()
    {
        const string code = """
import System.Threading.Tasks.*
func Main() -> () {
    let run = async func () -> Task {
        Task.Delay(1)
    }
    _ = run
}
""";
        CreateAnalyzerVerifier<UnawaitedTaskAnalyzer>(code,
            expectedDiagnostics:
            [
                new DiagnosticResult(UnawaitedTaskAnalyzer.DiagnosticId).WithSpan(4, 9, 4, 22)
            ], disabledDiagnostics: [CompilerDiagnostics.AsyncLacksAwait.Id]).Verify();
    }

}
