using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class PreferTargetTypedUnionCaseInTargetTypedContextAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void InvocationArgument_WithKnownUnionParameter_PrefersTargetTypedCaseSyntax()
    {
        const string code = """
func Foo(x: Shape) {}

func Test() {
    Foo(Shape.Circle(2))
}

union Shape {
    Circle(radius: int)
    Rectangle(width: int, height: int)
}
""";

        var verifier = CreateAnalyzerVerifier<PreferTargetTypedUnionCaseInTargetTypedContextAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(PreferTargetTypedUnionCaseInTargetTypedContextAnalyzer.DiagnosticId)
                    .WithSpan(4, 9, 4, 24)
                    .WithArguments("Shape")
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id],
            enableSuggestions: true);

        verifier.Verify();
    }

    [Fact]
    public void InvocationArgument_AlreadyTargetTypedCaseSyntax_NoDiagnostic()
    {
        const string code = """
func Foo(x: Shape) {}

func Test() {
    Foo(.Circle(2))
}

union Shape {
    Circle(radius: int)
    Rectangle(width: int, height: int)
}
""";

        var verifier = CreateAnalyzerVerifier<PreferTargetTypedUnionCaseInTargetTypedContextAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id],
            enableSuggestions: true);

        verifier.Verify();
    }

    [Fact]
    public void ExplicitUnionTypedLocal_WithQualifiedUnionCase_PrefersTargetTypedCaseSyntax()
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

        var verifier = CreateAnalyzerVerifier<PreferTargetTypedUnionCaseInTargetTypedContextAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(PreferTargetTypedUnionCaseInTargetTypedContextAnalyzer.DiagnosticId)
                    .WithSpan(2, 20, 2, 35)
                    .WithArguments("Shape")
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id],
            enableSuggestions: true);

        verifier.Verify();
    }

    [Fact]
    public void MixedSample_MatchesExpectedInvocationPreference()
    {
        const string code = """
import System.*

var a = Shape.Circle(2)
var d: Shape = Shape.Circle(2)
var c: Shape = .Circle(2)

Foo(.Circle(2))
Foo(Shape.Circle(2))

func Foo(shape: Shape) {}

union Shape {
    Circle(radius: int)
    Rectangle(width: int, height: int)
}
""";

        var verifier = CreateAnalyzerVerifier<PreferTargetTypedUnionCaseInTargetTypedContextAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(PreferTargetTypedUnionCaseInTargetTypedContextAnalyzer.DiagnosticId)
                    .WithSpan(4, 16, 4, 31)
                    .WithArguments("Shape"),
                new DiagnosticResult(PreferTargetTypedUnionCaseInTargetTypedContextAnalyzer.DiagnosticId)
                    .WithSpan(8, 5, 8, 20)
                    .WithArguments("Shape")
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id],
            enableSuggestions: true);

        verifier.Verify();
    }

    [Fact]
    public void ConditionalAccessReceiverBindingInvocation_DoesNotThrowOrReportDiagnostic()
    {
        const string code = """
import System.Console.*

val f: Foo? = Foo(21)
val s = f?(2)

WriteLine("$s")

public class Foo(b: int) {
    public self(factor: int) -> int {
        return b * factor
    }
}
""";

        var verifier = CreateAnalyzerVerifier<PreferTargetTypedUnionCaseInTargetTypedContextAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id],
            enableSuggestions: true);

        verifier.Verify();
    }
}
