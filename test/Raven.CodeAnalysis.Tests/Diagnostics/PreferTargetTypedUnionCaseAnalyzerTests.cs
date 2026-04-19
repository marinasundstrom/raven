using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class PreferTargetTypedUnionCaseAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void QualifiedUnionCaseConstruction_DoesNotReportDiagnostic()
    {
        const string code = """
func Test() {
    val v = Option<int>.Some(0)
}

union Option<T> {
    case Some(value: T)
    case None
}
""";

        var verifier = CreateAnalyzerVerifier<PreferTargetTypedUnionCaseAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id],
            enableSuggestions: true);

        verifier.Verify();
    }

    [Fact]
    public void VarQualifiedUnionCaseConstruction_DoesNotReportDiagnostic()
    {
        const string code = """
func Test() {
    var d = Shape.Circle(2)
}

union Shape {
    case Circle(radius: int)
    case Rectangle(width: int, height: int)
}
""";

        var verifier = CreateAnalyzerVerifier<PreferTargetTypedUnionCaseAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id],
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
    case Circle(radius: int)
    case Rectangle(width: int, height: int)
}
""";

        var verifier = CreateAnalyzerVerifier<PreferTargetTypedUnionCaseAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id],
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
    case Circle(radius: int)
    case Rectangle(width: int, height: int)
}
""";

        var verifier = CreateAnalyzerVerifier<PreferTargetTypedUnionCaseAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id],
            enableSuggestions: true);

        verifier.Verify();
    }
}
