using System.Linq;

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

    // Out-of-range assignments report the explicit-conversion hint used by assignment conversion diagnostics.

    [Fact]
    public void Val_ByteFromOverflowLiteral_ReportsError()
    {
        var verifier = CreateVerifier(
            "val x: byte = 256",
            [
                new DiagnosticResult(CompilerDiagnostics.CannotAssignFromTypeToType.Id).WithAnySpan().WithArguments("int", "byte"),
                new DiagnosticResult(CompilerDiagnostics.ExplicitConversionExists.Id).WithAnySpan().WithArguments("int", "byte")
            ]);
        verifier.Verify();
    }

    [Fact]
    public void Val_ByteFromNegativeLiteral_ReportsError()
    {
        var verifier = CreateVerifier(
            "val x: byte = -1",
            [
                new DiagnosticResult(CompilerDiagnostics.CannotAssignFromTypeToType.Id).WithAnySpan().WithArguments("int", "byte"),
                new DiagnosticResult(CompilerDiagnostics.ExplicitConversionExists.Id).WithAnySpan().WithArguments("int", "byte")
            ]);
        verifier.Verify();
    }

    [Theory]
    [InlineData("val x: sbyte = 128")]
    [InlineData("val x: sbyte = -129")]
    [InlineData("val x: short = 32768")]
    [InlineData("val x: ushort = -1")]
    [InlineData("val x: ushort = 65536")]
    [InlineData("val x: uint = -1")]
    [InlineData("val x: ulong = -1")]
    public void Val_OutOfRangeLiteral_ReportsAssignmentOrExplicitConversionDiagnostic(string source)
    {
        var diagnostics = CreateVerifier(source)
            .GetResult()
            .UnexpectedDiagnostics;

        Assert.Contains(diagnostics, static diagnostic =>
            diagnostic.Id is "RAV1504" or "RAV1507");
        Assert.All(diagnostics, static diagnostic =>
            Assert.Contains(diagnostic.Id, new[] { "RAV1504", "RAV1507" }));
    }
}
