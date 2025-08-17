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

    [Fact]
    public void InvocationPrefersMethodOverConstructor()
    {
        string testCode =
            """
            class Foo {}
            int Foo(int x) => x;
            int i = Foo(3);
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void InvocationFallsBackToConstructorWhenMethodNotApplicable()
    {
        string testCode =
            """
            class Foo { public Foo(int x) {} }
            void Foo() {}
            Foo(1);
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }
}

