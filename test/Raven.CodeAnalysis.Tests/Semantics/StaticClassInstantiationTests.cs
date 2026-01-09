using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class StaticClassInstantiationTests : DiagnosticTestBase
{
    [Fact]
    public void InstantiatingStaticClass_ReportsDiagnostic()
    {
        const string source = """
static class Foo {
    static Test() -> unit {}
}

let value = Foo();
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult("RAV0329").WithAnySpan().WithArguments("Foo")]);

        verifier.Verify();
    }
}
