using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class AutoPropertyInitializationAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void AutoPropertyWithoutInitializerOrConstructorAssignment_ReportsDiagnostic()
    {
        const string code = """
class C {
    Name: string { get; set; }
}
""";

        var verifier = CreateAnalyzerVerifier<AutoPropertyInitializationAnalyzer>(
            code,
            expectedDiagnostics: [
                new DiagnosticResult(AutoPropertyInitializationAnalyzer.DiagnosticId)
                    .WithSpan(2, 5, 2, 9)
                    .WithArguments("Name")
            ],
            disabledDiagnostics: ["RAV1014"]);

        verifier.Verify();
    }

    [Fact]
    public void AutoPropertyWithInitializer_NoDiagnostic()
    {
        const string code = """
class C {
    Name: string { get; set; } = "ok"
}
""";

        var verifier = CreateAnalyzerVerifier<AutoPropertyInitializationAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: ["RAV1014"]);

        verifier.Verify();
    }

    [Fact]
    public void AutoPropertyAssignedInConstructor_NoDiagnostic()
    {
        const string code = """
class C {
    Name: string { get; set; }

    public init() {
        Name = "ok"
    }
}
""";

        var verifier = CreateAnalyzerVerifier<AutoPropertyInitializationAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: ["RAV1014"]);

        verifier.Verify();
    }

    [Fact]
    public void RequiredAutoProperty_NoDiagnostic()
    {
        const string code = """
class C {
    public required Name: string { get; set; }
}
""";

        var verifier = CreateAnalyzerVerifier<AutoPropertyInitializationAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: ["RAV1014"]);

        verifier.Verify();
    }
}
