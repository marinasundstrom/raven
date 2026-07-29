using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Syntax.Tests;

public sealed class EncodedStringLiteralTests : DiagnosticTestBase
{
    [Fact]
    public void EncodedStringLiteral_Utf8_WithPlainString_HasNoDiagnostics()
    {
        const string source = """
let bytes = "Hello"u8
""";

        var verifier = CreateVerifier(source);
        verifier.Verify();
    }

    [Fact]
    public void EncodedStringLiteral_Ascii_WithNonAsciiCharacter_ReportsDiagnostic()
    {
        const string source = """
let bytes = "Pågen"ascii
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
let name = "World"
let bytes = "Hello ${name}"u8
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
