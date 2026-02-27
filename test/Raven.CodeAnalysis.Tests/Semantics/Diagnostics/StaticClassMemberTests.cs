using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class StaticClassMemberTests : DiagnosticTestBase
{
    [Fact]
    public void InstanceMembersInStaticClass_ReportDiagnostics()
    {
        const string source = """
static class Foo {
    val Value: int = 0;
    func Test() -> unit {}
    init() {}
}
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult("RAV0327").WithAnySpan().WithArguments("Foo", "Value"),
                new DiagnosticResult("RAV0327").WithAnySpan().WithArguments("Foo", "Test"),
                new DiagnosticResult("RAV0327").WithAnySpan().WithArguments("Foo", "init"),
            ]);

        verifier.Verify();
    }
}
