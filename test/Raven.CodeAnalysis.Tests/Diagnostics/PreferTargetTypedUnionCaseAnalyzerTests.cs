using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class PreferTargetTypedUnionCaseAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void QualifiedUnionCaseConstruction_ReportsConcreteRewrite()
    {
        const string code = """
func Test() {
    val v = Option<int>.Some(0)
}

union Option<T> {
    Some(value: T)
    None
}
""";

        var verifier = CreateAnalyzerVerifier<PreferTargetTypedUnionCaseAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(PreferTargetTypedUnionCaseAnalyzer.DiagnosticId)
                    .WithSpan(2, 13, 2, 24)
                    .WithArguments(
                        "Some",
                        "Option<int>")
            ],
            disabledDiagnostics: ["RAV1014"],
            enableSuggestions: true);

        verifier.Verify();
    }

    [Fact]
    public void VarQualifiedUnionCaseConstruction_WarnsCaseTypeInference()
    {
        const string code = """
func Test() {
    var d = Shape.Circle(2)
}

union Shape {
    Circle(radius: int)
    Rectangle(width: int, height: int)
}
""";

        var verifier = CreateAnalyzerVerifier<PreferTargetTypedUnionCaseAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(PreferTargetTypedUnionCaseAnalyzer.DiagnosticId)
                    .WithSpan(2, 13, 2, 18)
                    .WithArguments(
                        "Circle",
                        "Shape")
            ],
            disabledDiagnostics: ["RAV1014"],
            enableSuggestions: true);

        verifier.Verify();
    }

    [Fact]
    public void ExplicitUnionTypeAnnotation_DoesNotReportDiagnostic()
    {
        const string code = """
func Test() {
    var d: Shape = Shape.Circle(2)
}

union Shape {
    Circle(radius: int)
    Rectangle(width: int, height: int)
}
""";

        var verifier = CreateAnalyzerVerifier<PreferTargetTypedUnionCaseAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: ["RAV1014"],
            enableSuggestions: true);

        verifier.Verify();
    }

    [Fact]
    public void ExplicitUnionTypeAnnotation_WithTargetTypedCase_DoesNotReportDiagnostic()
    {
        const string code = """
func Test() {
    var c: Shape = .Circle(2)
}

union Shape {
    Circle(radius: int)
    Rectangle(width: int, height: int)
}
""";

        var verifier = CreateAnalyzerVerifier<PreferTargetTypedUnionCaseAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: ["RAV1014"],
            enableSuggestions: true);

        verifier.Verify();
    }
}
