using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class UninitializedFieldAnalyzerTests : AnalyzerTestBase
{
    [Theory]
    [InlineData("init() { title = \"ok\" }\ninit(value: int) { title = \"other\" }", false)]
    [InlineData("init() { title = \"ok\" }\ninit(value: int) { }", true)]
    [InlineData("init(value: bool) { if value { title = \"ok\" } }", true)]
    public void FieldRequiresEveryConstructorPath(string constructors, bool reportsDiagnostic)
    {
        var code = "class C {\n    private field title: string\n" + constructors + "\n}";
        var expected = reportsDiagnostic
            ? new[] { new DiagnosticResult(UninitializedFieldAnalyzer.DiagnosticId)
                .WithSpan(2, 19, 2, 24).WithArguments("title") }
            : [];
        CreateAnalyzerVerifier<UninitializedFieldAnalyzer>(code,
            expectedDiagnostics: expected,
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]).Verify();
    }

    [Fact]
    public void PrivateFieldWithoutInitializerOrConstructorAssignment_ReportsDiagnostic()
    {
        const string code = """
class C {
    private field title: string
}
""";

        var verifier = CreateAnalyzerVerifier<UninitializedFieldAnalyzer>(
            code,
            expectedDiagnostics: [
                new DiagnosticResult(UninitializedFieldAnalyzer.DiagnosticId)
                    .WithSpan(2, 19, 2, 24)
                    .WithArguments("title")
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void PrivateFieldAssignedInConstructor_NoDiagnostic()
    {
        const string code = """
class C {
    private field title: string

    init(title: string) {
        self.title = title
    }
}
""";

        var verifier = CreateAnalyzerVerifier<UninitializedFieldAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [
                CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id,
                UnusedParameterAnalyzer.DiagnosticId
            ]);

        verifier.Verify();
    }

    [Fact]
    public void FieldWithInitializer_NoDiagnostic()
    {
        const string code = """
class C {
    private field title: string = "ok"
}
""";

        var verifier = CreateAnalyzerVerifier<UninitializedFieldAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }
}
