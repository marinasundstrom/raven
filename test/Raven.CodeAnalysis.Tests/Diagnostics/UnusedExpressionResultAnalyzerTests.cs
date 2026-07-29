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
    public void InvocationExpressionStatement_AsUnitCallableTail_ReportsDiagnostic()
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
            expectedDiagnostics:
            [
                new DiagnosticResult(UnusedExpressionResultAnalyzer.DiagnosticId)
                    .WithSpan(6, 5, 6, 14)
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void InvocationExpressionStatement_AsImplicitUnitCallableTail_ReportsDiagnostic()
    {
        const string code = """
func Compute() -> int {
    42
}

func A() {
    Compute()
}
""";

        var verifier = CreateAnalyzerVerifier<UnusedExpressionResultAnalyzer>(
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
    public void InvocationExpressionStatement_BeforeUnitCallableTail_DoesNotReport()
    {
        const string code = """
func Compute() -> int {
    42
}

func Log() -> () { }

func A() -> () {
    Compute()
    Log()
}
""";

        var verifier = CreateAnalyzerVerifier<UnusedExpressionResultAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void BinaryExpressionContainingInvocation_BeforeUnitCallableTail_ReportsDiagnostic()
    {
        const string code = """
func Compute() -> int {
    40
}

func Log() -> () { }

func A() -> () {
    2 + Compute()
    Log()
}
""";

        var verifier = CreateAnalyzerVerifier<UnusedExpressionResultAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(UnusedExpressionResultAnalyzer.DiagnosticId)
                    .WithSpan(8, 5, 8, 18)
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void InvocationExpressionStatement_InFullMode_ReportsSingleUnusedResultDiagnostic()
    {
        const string code = """
func Compute() -> int {
    42
}

func Log() -> () { }

func A() -> () {
    Compute()
    Log()
}
""";

        var verifier = CreateAnalyzerVerifier<UnusedExpressionResultAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(UnusedExpressionResultAnalyzer.DiagnosticId)
                    .WithSpan(8, 5, 8, 14)
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id],
            returnedValueHandlingMode: ReturnedValueHandlingMode.Full);

        verifier.Verify();
    }

    [Fact]
    public void InvocationAssignedToDiscard_AsUnitCallableTail_DoesNotReport()
    {
        const string code = """
func Compute() -> int {
    42
}

func A() -> () {
    _ = Compute()
}
""";

        var verifier = CreateAnalyzerVerifier<UnusedExpressionResultAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void InvocationExpressionStatement_AsActionLambdaTail_ReportsDiagnostic()
    {
        const string code = """
import System.*

func Compute() -> int {
    42
}

let action: Action = func () {
    Compute()
}
""";

        var verifier = CreateAnalyzerVerifier<UnusedExpressionResultAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(UnusedExpressionResultAnalyzer.DiagnosticId)
                    .WithSpan(8, 5, 8, 14)
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void InvocationExpressionStatement_AsInferredValueLambdaTail_DoesNotReport()
    {
        const string code = """
func Compute() -> int {
    42
}

let calculate = func () {
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
    public void BlockExpressionValue_AssignedToLocal_DoesNotReport()
    {
        const string code = """
func A() -> int {
    let value = {
        let result = 42
        result
    }

    value
}
""";

        var verifier = CreateAnalyzerVerifier<UnusedExpressionResultAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void ArrowLambdaBlockValue_DoesNotReport()
    {
        const string code = """
let adjust = (value: int) -> int => {
    let result = value + 1
    result
}
""";

        var verifier = CreateAnalyzerVerifier<UnusedExpressionResultAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void ArrowActionBlockTail_ReportsDiagnostic()
    {
        const string code = """
import System.*

func Compute() -> int {
    42
}

let action: Action = () => {
    Compute()
}
""";

        var verifier = CreateAnalyzerVerifier<UnusedExpressionResultAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(UnusedExpressionResultAnalyzer.DiagnosticId)
                    .WithSpan(8, 5, 8, 14)
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void IfExpressionBranchValues_AssignedToLocal_DoNotReport()
    {
        const string code = """
func A(totalDistance: decimal) -> decimal {
    let averageLitersPer100Km =
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
    let result = match value {
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
