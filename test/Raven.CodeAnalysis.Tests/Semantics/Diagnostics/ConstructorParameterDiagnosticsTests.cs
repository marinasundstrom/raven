using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class ConstructorParameterDiagnosticsTests : DiagnosticTestBase
{
    [Fact]
    public void PrimaryConstructor_MissingTypeAnnotation_ReportsDiagnostic()
    {
        var code = """
record class Foo(
    val A,
    val B
)
""";

        var verifier = CreateVerifier(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(CompilerDiagnostics.ParameterTypeAnnotationRequired.Id).WithAnySpan().WithArguments("A"),
                new DiagnosticResult(CompilerDiagnostics.ParameterTypeAnnotationRequired.Id).WithAnySpan().WithArguments("B")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void Constructor_MissingTypeAnnotation_ReportsDiagnostic()
    {
        var code = """
class Foo {
    init(a, b: int) { }
}
""";

        var verifier = CreateVerifier(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(CompilerDiagnostics.ParameterTypeAnnotationRequired.Id).WithAnySpan().WithArguments("a")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void Constructor_ValOrVarParameter_ReportsPromotionDiagnostic()
    {
        var code = """
class Foo {
    init(val a: int, var b: int) { }
}
""";

        var verifier = CreateVerifier(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(CompilerDiagnostics.ConstructorParameterPromotionRequiresPrimaryConstructor.Id).WithAnySpan().WithArguments("a", "val"),
                new DiagnosticResult(CompilerDiagnostics.ConstructorParameterPromotionRequiresPrimaryConstructor.Id).WithAnySpan().WithArguments("b", "var")
            ]);

        verifier.Verify();
    }
}
