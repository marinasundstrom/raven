using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public sealed class UnusedExpressionResultAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void BinaryExpressionStatement_InImplicitUnitFunction_ReportsDiagnostic()
    {
        const string code = """
func A(x: int) {
    42 + x
}
""";

        var verifier = CreateAnalyzerVerifier<UnusedExpressionResultAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(UnusedExpressionResultAnalyzer.DiagnosticId)
                    .WithSpan(2, 5, 2, 11)
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void BinaryExpressionStatement_InExplicitUnitFunction_ReportsDiagnostic()
    {
        const string code = """
func A(x: int) -> () {
    42 + x
}
""";

        var verifier = CreateAnalyzerVerifier<UnusedExpressionResultAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(UnusedExpressionResultAnalyzer.DiagnosticId)
                    .WithSpan(2, 5, 2, 11)
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void UnaryExpressionStatement_ReportsDiagnostic()
    {
        const string code = """
func A(x: int) -> () {
    -x
}
""";

        var verifier = CreateAnalyzerVerifier<UnusedExpressionResultAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(UnusedExpressionResultAnalyzer.DiagnosticId)
                    .WithSpan(2, 5, 2, 7)
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void LocalReferenceExpressionStatement_ReportsDiagnostic()
    {
        const string code = """
func A(x: int) -> () {
    x
}
""";

        var verifier = CreateAnalyzerVerifier<UnusedExpressionResultAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(UnusedExpressionResultAnalyzer.DiagnosticId)
                    .WithSpan(2, 5, 2, 6)
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void LiteralExpressionStatement_ReportsDiagnostic()
    {
        const string code = """
func A() -> () {
    42
}
""";

        var verifier = CreateAnalyzerVerifier<UnusedExpressionResultAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(UnusedExpressionResultAnalyzer.DiagnosticId)
                    .WithSpan(2, 5, 2, 7)
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void UnitExpressionStatement_DoesNotReport()
    {
        const string code = """
func A() -> () {
    ()
}
""";

        var verifier = CreateAnalyzerVerifier<UnusedExpressionResultAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void InvocationExpressionStatement_DoesNotReport()
    {
        const string code = """
func Compute() -> int {
    42
}

func A() -> () {
    Compute()
}
""";

        var verifier = CreateAnalyzerVerifier<UnusedExpressionResultAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void AssignmentToDiscard_DoesNotReport()
    {
        const string code = """
func A(x: int) -> () {
    _ = 42 + x
}
""";

        var verifier = CreateAnalyzerVerifier<UnusedExpressionResultAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void ImplicitReturnValue_DoesNotReport()
    {
        const string code = """
func A(x: int) -> int {
    42 + x
}
""";

        var verifier = CreateAnalyzerVerifier<UnusedExpressionResultAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void IfExpressionBranchValues_AssignedToLocal_DoNotReport()
    {
        const string code = """
func A(totalDistance: decimal) -> decimal {
    val averageLitersPer100Km =
        if totalDistance == 0m {
            0m
        } else {
            1m
        }

    averageLitersPer100Km
}
""";

        var verifier = CreateAnalyzerVerifier<UnusedExpressionResultAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpressionBranchValues_AssignedToLocal_DoNotReport()
    {
        const string code = """
func A(value: int) -> int {
    val result = value match {
        0 => {
            0
        }
        _ => {
            1
        }
    }

    result
}
""";

        var verifier = CreateAnalyzerVerifier<UnusedExpressionResultAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }
}
