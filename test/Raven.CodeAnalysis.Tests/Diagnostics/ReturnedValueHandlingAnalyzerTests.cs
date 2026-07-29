using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public sealed class ReturnedValueHandlingAnalyzerTests : AnalyzerTestBase
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

        var verifier = CreateReturnedValueAnalyzerVerifier(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(UnusedExpressionResultAnalyzer.DiagnosticId)
                    .WithSpan(6, 5, 6, 14)
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void ReturningMethodCall_Default_DoesNotReportUntilEnabled()
    {
        var diagnostics = AnalyzeReturnedValueDiagnostics();

        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Id == UnusedExpressionResultAnalyzer.DiagnosticId);
    }

    [Fact]
    public void ReturningMethodCall_SeverityOnly_DoesNotEnableAnalyzer()
    {
        var diagnostics = AnalyzeReturnedValueDiagnostics(option: ReportDiagnostic.Warn);

        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Id == UnusedExpressionResultAnalyzer.DiagnosticId);
    }

    [Fact]
    public void ReturningMethodCall_WarningSeverity_UsesUnusedResultMessage()
    {
        var diagnostic = Assert.Single(AnalyzeReturnedValueDiagnostics(ReturnedValueHandlingMode.Full));

        Assert.Equal(DiagnosticSeverity.Warning, diagnostic.Severity);
        Assert.Equal("Expression result is not used; assign it to '_' to discard it explicitly.", diagnostic.GetMessage());
    }

    [Fact]
    public void ReturningMethodCall_ErrorSeverity_KeepsStableMessage()
    {
        var diagnostic = Assert.Single(AnalyzeReturnedValueDiagnostics(
            ReturnedValueHandlingMode.Full,
            ReportDiagnostic.Error));

        Assert.Equal(DiagnosticSeverity.Error, diagnostic.Severity);
        Assert.Equal("Expression result is not used; assign it to '_' to discard it explicitly.", diagnostic.GetMessage());
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

        var verifier = CreateReturnedValueAnalyzerVerifier(
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
    let value = Compute()
}
""";

        var verifier = CreateReturnedValueAnalyzerVerifier(
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

        var verifier = CreateReturnedValueAnalyzerVerifier(
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

        var verifier = CreateReturnedValueAnalyzerVerifier(
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

        var verifier = CreateReturnedValueAnalyzerVerifier(
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
    let counter = Counter()
    counter.Count
}
""";

        var verifier = CreateReturnedValueAnalyzerVerifier(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(UnusedExpressionResultAnalyzer.DiagnosticId)
                    .WithSpan(7, 5, 7, 18)
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    private AnalyzerVerifier<UnusedExpressionResultAnalyzer> CreateReturnedValueAnalyzerVerifier(
        string code,
        IEnumerable<DiagnosticResult>? expectedDiagnostics = null,
        IEnumerable<string>? disabledDiagnostics = null)
        => CreateAnalyzerVerifier<UnusedExpressionResultAnalyzer>(
            code,
            expectedDiagnostics,
            disabledDiagnostics,
            returnedValueHandlingMode: ReturnedValueHandlingMode.Full,
            specificDiagnosticOptions: new Dictionary<string, ReportDiagnostic>(StringComparer.OrdinalIgnoreCase)
            {
                [UnusedExpressionResultAnalyzer.DiagnosticId] = ReportDiagnostic.Warn
            });

    private static Diagnostic[] AnalyzeReturnedValueDiagnostics(
        ReturnedValueHandlingMode? mode = null,
        ReportDiagnostic? option = null)
    {
        const string code = """
func Compute() -> int {
    42
}

func Log() -> () {
}

func Test() -> () {
    Compute()
    Log()
}
""";

        var workspace = RavenWorkspace.Create(targetFramework: TestTargetFramework.Default);
        var options = new CompilationOptions(OutputKind.ConsoleApplication);
        if (mode is { } returnedValueHandlingMode)
            options = options.WithReturnedValueHandlingMode(returnedValueHandlingMode);

        if (option is { } diagnosticOption)
        {
            options = options.WithSpecificDiagnosticOption(
                UnusedExpressionResultAnalyzer.DiagnosticId,
                diagnosticOption);
        }

        var projectId = workspace.AddProject("Test", compilationOptions: options);
        var documentId = DocumentId.CreateNew(projectId);
        workspace.TryApplyChanges(workspace.CurrentSolution.AddDocument(documentId, "test.rvn", SourceText.From(code)));

        var project = workspace.CurrentSolution.GetProject(projectId)!;
        project = project.AddAnalyzerReference(new AnalyzerReference(new UnusedExpressionResultAnalyzer()));
        foreach (var reference in ReferenceAssemblies.Default)
            project = project.AddMetadataReference(reference);

        workspace.TryApplyChanges(project.Solution);

        return workspace.GetDiagnostics(projectId)
            .Where(diagnostic => diagnostic.Id == UnusedExpressionResultAnalyzer.DiagnosticId)
            .ToArray();
    }
}
