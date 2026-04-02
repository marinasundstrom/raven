using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class TypeModifierDiagnosticsTests : DiagnosticTestBase
{
    [Fact]
    public void OpenModifierOnInterface_ReportsDiagnostic()
    {
        const string source = """
open interface I {}
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult("RAV0332").WithAnySpan().WithArguments("open", "interface", "I")]);

        verifier.Verify();
    }

    [Fact]
    public void SealedModifierOnEnum_ReportsDiagnostic()
    {
        const string source = """
sealed enum E {
    A
}
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult("RAV0332").WithAnySpan().WithArguments("sealed", "enum", "E")]);

        verifier.Verify();
    }

    [Fact]
    public void AbstractModifierOnStruct_ReportsDiagnostic()
    {
        const string source = """
abstract struct S {}
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult("RAV0332").WithAnySpan().WithArguments("abstract", "struct", "S")]);

        verifier.Verify();
    }

    [Fact]
    public void PrivateTopLevelClass_ReportsDiagnostic()
    {
        const string source = """
private class C {}
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult("RAV0332").WithAnySpan().WithArguments("private", "class", "C")]);

        verifier.Verify();
    }

    [Fact]
    public void PrivateNestedClass_DoesNotReportModifierDiagnostic()
    {
        const string source = """
class Outer {
    private class Inner {}
}
""";

        var verifier = CreateVerifier(source);
        verifier.Verify();
    }

    [Fact]
    public void PublicLocalClass_ReportsDiagnostic()
    {
        const string source = """
func outer() {
    public class Helper {}
}
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult("RAV0332").WithAnySpan().WithArguments("public", "local class", "Helper")]);

        verifier.Verify();
    }

    [Fact]
    public void PartialLocalStruct_ReportsDiagnostic()
    {
        const string source = """
func outer() {
    partial struct Helper {}
}
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult("RAV0332").WithAnySpan().WithArguments("partial", "local struct", "Helper")]);

        verifier.Verify();
    }
}
