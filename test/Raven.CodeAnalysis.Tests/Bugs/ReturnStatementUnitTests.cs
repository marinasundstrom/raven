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
}
