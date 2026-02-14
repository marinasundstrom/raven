using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ThrowStatementDiagnosticsTests : DiagnosticTestBase
{
    [Fact]
    public void ThrowExpressionOperandMustDeriveFromException()
    {
        var code = """
func Main() {
    throw 42;
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV1020").WithSpan(2, 11, 2, 13).WithArguments("int")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void ThrowExpressionMustDeriveFromException()
    {
        var code = """
func Main() {
    val value = "name" ?? throw 42;
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV1020").WithSpan(2, 33, 2, 35).WithArguments("int")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void ThrowExpressionWithUnknownType_ReportsNameNotInScope()
    {
        var code = """
func Main() {
    throw MissingError("name");
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV0103").WithSpan(2, 11, 2, 23).WithArguments("MissingError")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void ThrowExpressionWithNonExceptionType_ReportsRav1020()
    {
        var code = """
func Main() {
    throw Error("name");
}

record Error(Message: string)
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV1020").WithSpan(2, 11, 2, 24).WithArguments("Error")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void ThrowExpressionWithImportedConsoleError_ReportsNonInvocableMember()
    {
        var code = """
import System.*
import System.Console.*

func Main() {
    throw Error("name");
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV1955").WithSpan(5, 11, 5, 16).WithArguments("Error")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void ThrowExpressionWithMissingConstructorOverload_ReportsNoOverload()
    {
        var code = """
func Main() {
    throw Error(42);
}

record Error(Message: string)
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV1501").WithSpan(2, 11, 2, 20).WithArguments("constructor for type", "Error", "1"),
                new DiagnosticResult("RAV1020").WithSpan(2, 11, 2, 20).WithArguments("Error")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void ThrowStatementInBlockExpressionInitializer_IsAllowed()
    {
        var code = """
func Main() {
    val value = {
        throw System.InvalidOperationException("fail")
        ()
    };
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV0162").WithSpan(4, 9, 4, 11)
            ]);

        verifier.Verify();
    }
}
