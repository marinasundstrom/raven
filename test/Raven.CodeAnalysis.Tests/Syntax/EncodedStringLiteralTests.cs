using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Syntax.Tests;

public sealed class EncodedStringLiteralTests : DiagnosticTestBase
{
    [Fact]
    public void EncodedStringLiteral_Utf8_WithPlainString_HasNoDiagnostics()
    {
        const string source = """
val bytes = "Hello"u8
""";

        var verifier = CreateVerifier(source);
        verifier.Verify();
    }

    [Fact]
    public void EncodedStringLiteral_Ascii_WithNonAsciiCharacter_ReportsDiagnostic()
    {
        const string source = """
val bytes = "Pågen"ascii
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.EncodedStringLiteralAsciiOutOfRange.Id)
                    .WithAnySpan()
                    .WithArguments("å")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void EncodedStringLiteral_WithInterpolation_ReportsDiagnostic()
    {
        const string source = """
val name = "World"
val bytes = "Hello ${name}"u8
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.EncodedStringLiteralInterpolationNotSupported.Id)
                    .WithAnySpan()
            ]);

        verifier.Verify();
    }
}
