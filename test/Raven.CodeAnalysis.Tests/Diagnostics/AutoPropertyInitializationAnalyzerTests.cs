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
    var Name: string { get; set; }
}
""";

        var verifier = CreateAnalyzerVerifier<AutoPropertyInitializationAnalyzer>(
            code,
            expectedDiagnostics: [
                new DiagnosticResult(AutoPropertyInitializationAnalyzer.DiagnosticId)
                    .WithSpan(2, 9, 2, 13)
                    .WithArguments("Name")
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void AutoPropertyWithInitializer_NoDiagnostic()
    {
        const string code = """
class C {
    var Name: string { get; set; } = "ok"
}
""";

        var verifier = CreateAnalyzerVerifier<AutoPropertyInitializationAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void AutoPropertyAssignedInConstructor_NoDiagnostic()
    {
        const string code = """
class C {
    var Name: string { get; set; }

    public init() {
        Name = "ok"
    }
}
""";

        var verifier = CreateAnalyzerVerifier<AutoPropertyInitializationAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void RequiredAutoProperty_NoDiagnostic()
    {
        const string code = """
class C {
    public required var Name: string { get; set; }
}
""";

        var verifier = CreateAnalyzerVerifier<AutoPropertyInitializationAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }
}
