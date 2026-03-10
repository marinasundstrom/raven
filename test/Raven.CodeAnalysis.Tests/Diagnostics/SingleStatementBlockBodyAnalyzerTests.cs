using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class SingleStatementBlockBodyAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void Method_WithSingleReturnStatement_ReportsDiagnostic()
    {
        const string code = """
class C {
    func Get() -> int {
        return 42
    }
}
""";

        var verifier = CreateAnalyzerVerifier<SingleStatementBlockBodyAnalyzer>(
            code,
            [
                new DiagnosticResult(SingleStatementBlockBodyAnalyzer.DiagnosticId)
                    .WithSeverity(DiagnosticSeverity.Info)
                    .WithLocation(2, 23)
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void Function_WithSingleReturnStatement_ReportsDiagnostic()
    {
        const string code = """
func Get() -> int {
    return 42
}
""";

        var verifier = CreateAnalyzerVerifier<SingleStatementBlockBodyAnalyzer>(
            code,
            [
                new DiagnosticResult(SingleStatementBlockBodyAnalyzer.DiagnosticId)
                    .WithSeverity(DiagnosticSeverity.Info)
                    .WithLocation(1, 19)
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void Method_WithMultipleStatements_DoesNotReportDiagnostic()
    {
        const string code = """
class C {
    func Get() -> int {
        val x = 41
        return x + 1
    }
}
""";

        var verifier = CreateAnalyzerVerifier<SingleStatementBlockBodyAnalyzer>(
            code,
            [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void Method_AlreadyExpressionBody_DoesNotReportDiagnostic()
    {
        const string code = """
class C {
    func Get() -> int => 42
}
""";

        var verifier = CreateAnalyzerVerifier<SingleStatementBlockBodyAnalyzer>(
            code,
            [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void Operator_WithSingleReturnStatement_ReportsDiagnostic()
    {
        const string code = """
class C {
    static func ==(a: C, b: C) -> bool {
        return true
    }
}
""";

        var verifier = CreateAnalyzerVerifier<SingleStatementBlockBodyAnalyzer>(
            code,
            [
                new DiagnosticResult(SingleStatementBlockBodyAnalyzer.DiagnosticId)
                    .WithSeverity(DiagnosticSeverity.Info)
                    .WithLocation(2, 40)
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void ConversionOperator_WithSingleReturnStatement_ReportsDiagnostic()
    {
        const string code = """
class C {
    static func implicit(value: C) -> string {
        return ""
    }
}
""";

        var verifier = CreateAnalyzerVerifier<SingleStatementBlockBodyAnalyzer>(
            code,
            [
                new DiagnosticResult(SingleStatementBlockBodyAnalyzer.DiagnosticId)
                    .WithSeverity(DiagnosticSeverity.Info)
                    .WithLocation(2, 46)
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }
}
