using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class MethodBindingTests : DiagnosticTestBase
{
    [Fact]
    public void InvokingInstanceMethodAsStatic_ReportsMemberDoesNotContainDefinition()
    {
        string testCode = """
        class Foo {
            f() {}
        }
        Foo.f()
        """;

        var verifier = CreateVerifier(testCode,
            [new DiagnosticResult("RAV0117").WithSpan(4, 5, 4, 6).WithArguments("Foo", "f")]);

        verifier.Verify();
    }
}
