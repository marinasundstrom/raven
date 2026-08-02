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

    [Fact]
    public void Function_ValOrVarParameter_ReportsBindingKeywordDiagnostic()
    {
        var code = """
func F(val a: int, var b: int) -> int {
    a + b
}
""";

        var verifier = CreateVerifier(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(CompilerDiagnostics.ParameterBindingKeywordNotAllowed.Id).WithAnySpan().WithArguments("val", "a"),
                new DiagnosticResult(CompilerDiagnostics.ParameterBindingKeywordNotAllowed.Id).WithAnySpan().WithArguments("var", "b")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void Method_ValOrVarParameter_ReportsBindingKeywordDiagnostic()
    {
        var code = """
class C {
    func M(val a: int, var b: int) -> int {
        a + b
    }
}
""";

        var verifier = CreateVerifier(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(CompilerDiagnostics.ParameterBindingKeywordNotAllowed.Id).WithAnySpan().WithArguments("val", "a"),
                new DiagnosticResult(CompilerDiagnostics.ParameterBindingKeywordNotAllowed.Id).WithAnySpan().WithArguments("var", "b")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void Operator_ValOrVarParameter_ReportsBindingKeywordDiagnostic()
    {
        var code = """
class C {
    public static func +(val left: C, var right: C) -> C {
        left
    }
}
""";

        var verifier = CreateVerifier(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(CompilerDiagnostics.ParameterBindingKeywordNotAllowed.Id).WithAnySpan().WithArguments("val", "left"),
                new DiagnosticResult(CompilerDiagnostics.ParameterBindingKeywordNotAllowed.Id).WithAnySpan().WithArguments("var", "right")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void IndexerParameter_ValOrVar_ReportsBindingKeywordDiagnostic()
    {
        var code = """
class C {
    private var data: int[] = [0, 1, 2]

    public var self[val index: int, var step: int]: int {
        get => data[index]
        set => data[index] = value + step
    }
}
""";

        var verifier = CreateVerifier(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(CompilerDiagnostics.ParameterBindingKeywordNotAllowed.Id).WithAnySpan().WithArguments("val", "index"),
                new DiagnosticResult(CompilerDiagnostics.ParameterBindingKeywordNotAllowed.Id).WithAnySpan().WithArguments("var", "step")
            ]);

        verifier.Verify();
    }
}
