using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ObjectCreationTests : DiagnosticTestBase
{
    [Fact]
    public void InvocationWithoutNewCreatesObject()
    {
        string testCode =
            """
            class Foo {}
            Foo();
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }
}

