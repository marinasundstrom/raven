using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public sealed class ConstructorParameterNamingAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void ConstructorParameter_PascalCase_ReportsWarning()
    {
        const string code = """
class Foo {
    init(Name: int) {
    }
}
""";

        var verifier = CreateAnalyzerVerifier<ConstructorParameterNamingAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(ConstructorParameterNamingAnalyzer.DiagnosticId)
                    .WithLocation(2, 10)
                    .WithArguments("Name", "camelCase naming")
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void PrimaryConstructorPromotedParameter_CamelCase_ReportsWarning()
    {
        const string code = """
class Foo(var name: string)
""";

        var verifier = CreateAnalyzerVerifier<ConstructorParameterNamingAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(ConstructorParameterNamingAnalyzer.DiagnosticId)
                    .WithLocation(1, 15)
                    .WithArguments("name", "PascalCase naming")
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void PrimaryConstructorCapturedParameter_PascalCase_ReportsWarning()
    {
        const string code = """
class Foo(Name: string) {
    func Get() -> string => Name
}
""";

        var verifier = CreateAnalyzerVerifier<ConstructorParameterNamingAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(ConstructorParameterNamingAnalyzer.DiagnosticId)
                    .WithLocation(1, 11)
                    .WithArguments("Name", "camelCase naming")
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void CorrectlyNamedConstructorParameters_DoNotReport()
    {
        const string code = """
class Foo(value: int, var Name: string) {
    init(seed: int) {
        val x = value + seed
    }
}
""";

        var verifier = CreateAnalyzerVerifier<ConstructorParameterNamingAnalyzer>(
            code,
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void PrimaryConstructorNonPublicPromotedParameter_CamelCase_DoesNotReport()
    {
        const string code = """
class Foo(private var name: string)
""";

        var verifier = CreateAnalyzerVerifier<ConstructorParameterNamingAnalyzer>(
            code,
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }
}
