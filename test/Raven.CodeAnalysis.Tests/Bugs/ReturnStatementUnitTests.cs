using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Bugs;

public class ReturnStatementUnitTests : DiagnosticTestBase
{
    [Fact]
    public void UnitMethod_ExplicitReturn_NoDiagnostics()
    {
        var code = """
class Foo {
    Test() -> unit {
        return;
    }
}
""";
        var verifier = CreateVerifier(code);
        verifier.Verify();
    }

    [Fact]
    public void NonUnitMethod_EmptyReturn_ReportsDiagnostic()
    {
        var code = """
class Foo {
    Test() -> int {
        return;
    }
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics: [
                new DiagnosticResult(CompilerDiagnostics.CannotConvertFromTypeToType.Id)
                    .WithSpan(3, 9, 3, 16)
                    .WithArguments("unit", "int")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void NonUnitMethod_ReturnExpression_NotAssignable_ReportsDiagnostic()
    {
        var code = """
class Foo {
    Test() -> int {
        return "";
    }
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics: [
                new DiagnosticResult(CompilerDiagnostics.CannotConvertFromTypeToType.Id)
                    .WithSpan(3, 16, 3, 18)
                    .WithArguments("\"\"", "int")
            ]);

        verifier.Verify();
    }
}
