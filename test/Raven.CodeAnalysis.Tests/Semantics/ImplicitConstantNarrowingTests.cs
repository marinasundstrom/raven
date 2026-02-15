using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ImplicitConstantNarrowingTests : DiagnosticTestBase
{
    // ── In-range assignments (should compile without errors) ─────────────

    [Fact]
    public void Val_ByteFromInRangeLiteral_Succeeds()
    {
        var verifier = CreateVerifier("val x: byte = 255");
        verifier.Verify();
    }

    [Fact]
    public void Val_ByteFromZeroLiteral_Succeeds()
    {
        var verifier = CreateVerifier("val x: byte = 0");
        verifier.Verify();
    }

    [Fact]
    public void Val_SByteFromPositiveLiteral_Succeeds()
    {
        var verifier = CreateVerifier("val x: sbyte = 127");
        verifier.Verify();
    }

    [Fact]
    public void Val_SByteFromNegativeLiteral_Succeeds()
    {
        var verifier = CreateVerifier("val x: sbyte = -128");
        verifier.Verify();
    }

    [Fact]
    public void Val_ShortFromInRangeLiteral_Succeeds()
    {
        var verifier = CreateVerifier("val x: short = 32767");
        verifier.Verify();
    }

    [Fact]
    public void Val_ShortFromNegativeLiteral_Succeeds()
    {
        var verifier = CreateVerifier("val x: short = -32768");
        verifier.Verify();
    }

    [Fact]
    public void Val_UShortFromInRangeLiteral_Succeeds()
    {
        var verifier = CreateVerifier("val x: ushort = 65535");
        verifier.Verify();
    }

    [Fact]
    public void Val_UIntFromInRangeLiteral_Succeeds()
    {
        var verifier = CreateVerifier("val x: uint = 42");
        verifier.Verify();
    }

    [Fact]
    public void Val_ULongFromInRangeLiteral_Succeeds()
    {
        var verifier = CreateVerifier("val x: ulong = 42");
        verifier.Verify();
    }

    [Fact]
    public void Val_CharFromInRangeLiteral_Succeeds()
    {
        var verifier = CreateVerifier("val x: char = 65");
        verifier.Verify();
    }

    // ── Out-of-range assignments (should report RAV1504) ────────────────

    [Fact]
    public void Val_ByteFromOverflowLiteral_ReportsError()
    {
        var verifier = CreateVerifier(
            "val x: byte = 256",
            [new DiagnosticResult("RAV1504").WithAnySpan().WithArguments("'int'", "'byte'")]);
        verifier.Verify();
    }

    [Fact]
    public void Val_ByteFromNegativeLiteral_ReportsError()
    {
        var verifier = CreateVerifier(
            "val x: byte = -1",
            [new DiagnosticResult("RAV1504").WithAnySpan().WithArguments("'int'", "'byte'")]);
        verifier.Verify();
    }

    [Fact]
    public void Val_SByteFromOverflowLiteral_ReportsError()
    {
        var verifier = CreateVerifier(
            "val x: sbyte = 128",
            [new DiagnosticResult("RAV1504").WithAnySpan().WithArguments("'int'", "'sbyte'")]);
        verifier.Verify();
    }

    [Fact]
    public void Val_SByteFromUnderflowLiteral_ReportsError()
    {
        var verifier = CreateVerifier(
            "val x: sbyte = -129",
            [new DiagnosticResult("RAV1504").WithAnySpan().WithArguments("'int'", "'sbyte'")]);
        verifier.Verify();
    }

    [Fact]
    public void Val_ShortFromOverflowLiteral_ReportsError()
    {
        var verifier = CreateVerifier(
            "val x: short = 32768",
            [new DiagnosticResult("RAV1504").WithAnySpan().WithArguments("'int'", "'short'")]);
        verifier.Verify();
    }

    [Fact]
    public void Val_UShortFromNegativeLiteral_ReportsError()
    {
        var verifier = CreateVerifier(
            "val x: ushort = -1",
            [new DiagnosticResult("RAV1504").WithAnySpan().WithArguments("'int'", "'ushort'")]);
        verifier.Verify();
    }

    [Fact]
    public void Val_UShortFromOverflowLiteral_ReportsError()
    {
        var verifier = CreateVerifier(
            "val x: ushort = 65536",
            [new DiagnosticResult("RAV1504").WithAnySpan().WithArguments("'int'", "'ushort'")]);
        verifier.Verify();
    }

    [Fact]
    public void Val_UIntFromNegativeLiteral_ReportsError()
    {
        var verifier = CreateVerifier(
            "val x: uint = -1",
            [new DiagnosticResult("RAV1504").WithAnySpan().WithArguments("'int'", "'uint'")]);
        verifier.Verify();
    }

    [Fact]
    public void Val_ULongFromNegativeLiteral_ReportsError()
    {
        var verifier = CreateVerifier(
            "val x: ulong = -1",
            [new DiagnosticResult("RAV1504").WithAnySpan().WithArguments("'int'", "'ulong'")]);
        verifier.Verify();
    }
}
