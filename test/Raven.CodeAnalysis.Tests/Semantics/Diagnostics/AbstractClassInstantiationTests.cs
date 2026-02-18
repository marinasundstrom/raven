using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class AbstractClassInstantiationTests : DiagnosticTestBase
{
    [Fact]
    public void InstantiatingAbstractClass_ReportsDiagnostic()
    {
        const string source = """
abstract class Foo {}

val value = Foo();
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult("RAV0611").WithAnySpan().WithArguments("Foo")]);

        verifier.Verify();
    }

    [Fact]
    public void InstantiatingSealedHierarchyRoot_ReportsDiagnostic()
    {
        const string source = """
sealed class Expr {}
class Lit : Expr {}

val value = Expr();
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult("RAV0611").WithAnySpan().WithArguments("Expr")]);

        verifier.Verify();
    }
}
