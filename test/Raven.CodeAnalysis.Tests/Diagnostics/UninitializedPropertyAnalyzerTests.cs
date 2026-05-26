using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class UninitializedPropertyAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void PropertyWithoutInitializerOrConstructorAssignment_ReportsDiagnostic()
    {
        const string code = """
class C {
    var Name: string { get; set; }
}
""";

        var verifier = CreateAnalyzerVerifier<UninitializedPropertyAnalyzer>(
            code,
            expectedDiagnostics: [
                new DiagnosticResult(UninitializedPropertyAnalyzer.DiagnosticId)
                    .WithSpan(2, 9, 2, 13)
                    .WithArguments("Name")
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void PrivateStoredValPropertyWithoutInitializerOrConstructorAssignment_ReportsDiagnostic()
    {
        const string code = """
class UiStackPanel {
}

class UiWindow {
    private val title: string

    init(content: UiStackPanel, title: string) {
        Content = content
    }

    val Content: UiStackPanel
    val Title: string => title
}
""";

        var verifier = CreateAnalyzerVerifier<UninitializedPropertyAnalyzer>(
            code,
            expectedDiagnostics: [
                new DiagnosticResult(UninitializedPropertyAnalyzer.DiagnosticId)
                    .WithSpan(5, 17, 5, 22)
                    .WithArguments("title")
            ],
            disabledDiagnostics: [
                CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id,
                UnusedParameterAnalyzer.DiagnosticId
            ]);

        verifier.Verify();
    }

    [Fact]
    public void PrivateStoredValPropertyAssignedInConstructor_NoDiagnostic()
    {
        const string code = """
class UiWindow {
    private val title: string

    init(title: string) {
        self.title = title
    }

    val Title: string => title
}
""";

        var verifier = CreateAnalyzerVerifier<UninitializedPropertyAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [
                CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id,
                UnusedParameterAnalyzer.DiagnosticId
            ]);

        verifier.Verify();
    }

    [Fact]
    public void PropertyWithInitializer_NoDiagnostic()
    {
        const string code = """
class C {
    var Name: string { get; set; } = "ok"
}
""";

        var verifier = CreateAnalyzerVerifier<UninitializedPropertyAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void PropertyAssignedInConstructor_NoDiagnostic()
    {
        const string code = """
class C {
    var Name: string { get; set; }

    init() {
        Name = "ok"
    }
}
""";

        var verifier = CreateAnalyzerVerifier<UninitializedPropertyAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void RequiredProperty_NoDiagnostic()
    {
        const string code = """
class C {
    required var Name: string { get; set; }
}
""";

        var verifier = CreateAnalyzerVerifier<UninitializedPropertyAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void PropertyWithExplicitGetterImplementation_NoDiagnostic()
    {
        const string code = """
class C {
    val Name: string {
        get => "ok"
    }
}
""";

        var verifier = CreateAnalyzerVerifier<UninitializedPropertyAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void PropertyWithExplicitGetterBlockImplementation_NoDiagnostic()
    {
        const string code = """
class C {
    val Name: string {
        get {
            "ok"
        }
    }
}
""";

        var verifier = CreateAnalyzerVerifier<UninitializedPropertyAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void PropertyAssignedInPrimaryInitializerBlock_NoDiagnostic()
    {
        const string code = """
class C(name: string) {
    {
        Score = 1
    }

    var Score: int { get; set; }
}
""";

        var verifier = CreateAnalyzerVerifier<UninitializedPropertyAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }
}
