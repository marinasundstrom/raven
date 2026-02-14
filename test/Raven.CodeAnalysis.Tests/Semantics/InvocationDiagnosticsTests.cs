using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class InvocationDiagnosticsTests : DiagnosticTestBase
{
    [Fact]
    public void FunctionInvocation_BindsAsInvocable()
    {
        var code = """
func Parse(x: int) -> int {
    return x
}

func Main() {
    Parse(1)
}
""";

        var verifier = CreateVerifier(code, expectedDiagnostics: []);
        verifier.Verify();
    }

    [Fact]
    public void TypeInvocation_BindsAsConstructor()
    {
        var code = """
record Error(Message: string)

func Main() {
    Error("x")
}
""";

        var verifier = CreateVerifier(code, expectedDiagnostics: []);
        verifier.Verify();
    }

    [Fact]
    public void ImportedConsoleErrorInvocation_ReportsNonInvocableMember()
    {
        var code = """
import System.*
import System.Console.*

func Main() {
    Error("x");
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV1955").WithSpan(5, 5, 5, 10).WithArguments("Error")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void LocalValueInvocation_ReportsNonInvocableMember()
    {
        var code = """
func Main() {
    val x = 1
    x()
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV1955").WithSpan(3, 5, 3, 6).WithArguments("x")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void ParenthesizedExpressionInvocation_ReportsInvalidInvocation()
    {
        var code = """
func Main() {
    (1 + 2)()
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV0030").WithSpan(2, 5, 2, 12)
            ]);

        verifier.Verify();
    }

    [Fact]
    public void ConditionalAccessOnNonInvocableMember_ReportsNonInvocableMember()
    {
        var code = """
func Main() {
    val box = Box()
    box?.Value()
}

class Box {
    public Value: int { get => 1 }
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV1955").WithSpan(3, 9, 3, 17).WithArguments("Value")
            ],
            disabledDiagnostics: ["RAV9001"]);

        verifier.Verify();
    }

    [Fact]
    public void MemberInvocationWithMissingMember_ReportsMemberMissing()
    {
        var code = """
func Main() {
    val box = Box()
    box.Missing(2)
}

class Box {
    public Value: int { get => 1 }
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV0117").WithSpan(3, 9, 3, 16).WithArguments("Box", "Missing")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void ConditionalMemberInvocationWithMissingMember_ReportsMemberMissing()
    {
        var code = """
func Main() {
    val box = Box()
    box?.Missing(2)
}

class Box {
    public Value: int { get => 1 }
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV0117").WithSpan(3, 10, 3, 17).WithArguments("Box", "Missing")
            ],
            disabledDiagnostics: ["RAV9001"]);

        verifier.Verify();
    }

    [Fact]
    public void MethodInvocationWithWrongArity_ReportsNoOverload()
    {
        var code = """
func Parse(value: int) {}

func Main() {
    Parse()
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV1501").WithSpan(4, 5, 4, 12).WithArguments("method", "Parse", "0")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void ConstructorInvocationWithWrongArity_ReportsNoOverload()
    {
        var code = """
record Error(Message: string)

func Main() {
    Error()
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV1501").WithSpan(4, 5, 4, 12).WithArguments("constructor for type", "Error", "0")
            ]);

        verifier.Verify();
    }
}
