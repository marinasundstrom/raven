using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public sealed class UnhandledMemberReturnValueAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void ReturningMethodCall_ReportsDiagnosticWhenReturnValueIgnored()
    {
        const string code = """
func Compute() -> int {
    42
}

func Test() -> () {
    Compute()
}
""";

        var verifier = CreateAnalyzerVerifier<UnhandledMemberReturnValueAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(UnhandledMemberReturnValueAnalyzer.DiagnosticId)
                    .WithSpan(6, 5, 6, 14)
                    .WithArguments("Compute")
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void ReturningMethodCall_DefaultWarningMessage_SaysReturnValueIsIgnored()
    {
        var diagnostic = AnalyzeReturnedValueDiagnostic();

        Assert.Equal(DiagnosticSeverity.Warning, diagnostic.Severity);
        Assert.Equal("Returned value of 'Compute' is not handled.", diagnostic.GetMessage());
    }

    [Fact]
    public void ReturningMethodCall_ErrorSeverity_KeepsStableMessage()
    {
        var diagnostic = AnalyzeReturnedValueDiagnostic(ReportDiagnostic.Error);

        Assert.Equal(DiagnosticSeverity.Error, diagnostic.Severity);
        Assert.Equal("Returned value of 'Compute' is not handled.", diagnostic.GetMessage());
    }

    [Fact]
    public void UnitMethodCall_DoesNotReport()
    {
        const string code = """
func Log() -> () {
}

func Test() -> () {
    Log()
}
""";

        var verifier = CreateAnalyzerVerifier<UnhandledMemberReturnValueAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void ReturningMethodCall_DoesNotReportWhenReturnValueAssignedToLocal()
    {
        const string code = """
func Compute() -> int {
    42
}

func Test() -> () {
    val value = Compute()
}
""";

        var verifier = CreateAnalyzerVerifier<UnhandledMemberReturnValueAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id, UnusedVariableAnalyzer.DiagnosticId]);

        verifier.Verify();
    }

    [Fact]
    public void ReturningMethodCall_DoesNotReportWhenReturnValueAssignedToDiscard()
    {
        const string code = """
func Compute() -> int {
    42
}

func Test() -> () {
    _ = Compute()
}
""";

        var verifier = CreateAnalyzerVerifier<UnhandledMemberReturnValueAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void ReturningMethodCall_DoesNotReportWhenReturnValuePassedAsArgument()
    {
        const string code = """
func Compute() -> int {
    42
}

func Use(value: int) -> () {
}

func Test() -> () {
    Use(Compute())
}
""";

        var verifier = CreateAnalyzerVerifier<UnhandledMemberReturnValueAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void ReturningMethodCall_DoesNotReportWhenReturnValueImplicitlyReturned()
    {
        const string code = """
func Compute() -> int {
    42
}

func Test() -> int {
    Compute()
}
""";

        var verifier = CreateAnalyzerVerifier<UnhandledMemberReturnValueAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void ReturningPropertyAccess_ReportsDiagnosticWhenReturnValueIgnored()
    {
        const string code = """
class Counter {
    val Count: int = 1
}

func Test() -> () {
    val counter = Counter()
    counter.Count
}
""";

        var verifier = CreateAnalyzerVerifier<UnhandledMemberReturnValueAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(UnhandledMemberReturnValueAnalyzer.DiagnosticId)
                    .WithSpan(7, 5, 7, 18)
                    .WithArguments("Count")
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    private static Diagnostic AnalyzeReturnedValueDiagnostic(ReportDiagnostic? option = null)
    {
        const string code = """
func Compute() -> int {
    42
}

func Test() -> () {
    Compute()
}
""";

        var workspace = RavenWorkspace.Create(targetFramework: TestTargetFramework.Default);
        var options = new CompilationOptions(OutputKind.ConsoleApplication);
        if (option is { } diagnosticOption)
        {
            options = options.WithSpecificDiagnosticOption(
                UnhandledMemberReturnValueAnalyzer.DiagnosticId,
                diagnosticOption);
        }

        var projectId = workspace.AddProject("Test", compilationOptions: options);
        var documentId = DocumentId.CreateNew(projectId);
        workspace.TryApplyChanges(workspace.CurrentSolution.AddDocument(documentId, "test.rvn", SourceText.From(code)));

        var project = workspace.CurrentSolution.GetProject(projectId)!;
        project = project.AddAnalyzerReference(new AnalyzerReference(new UnhandledMemberReturnValueAnalyzer()));
        foreach (var reference in ReferenceAssemblies.Default)
            project = project.AddMetadataReference(reference);

        workspace.TryApplyChanges(project.Solution);

        return Assert.Single(
            workspace.GetDiagnostics(projectId),
            diagnostic => diagnostic.Id == UnhandledMemberReturnValueAnalyzer.DiagnosticId);
    }
}
