using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class StaticClassInstantiationTests : DiagnosticTestBase
{
    [Fact]
    public void InstantiatingStaticClass_ReportsDiagnostic()
    {
        const string source = """
static class Foo {
    static func Test() -> unit {}
}

val value = Foo();
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult("RAV2810").WithSpan(5, 1, 5, 4).WithArguments("Foo"),
                new DiagnosticResult("RAV0329").WithSpan(5, 13, 5, 18).WithArguments("Foo")
            ]);

        verifier.Verify();
    }
}
