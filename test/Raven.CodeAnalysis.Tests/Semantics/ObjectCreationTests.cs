using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ObjectCreationTests : DiagnosticTestBase
{
    [Fact]
    public void InvocationWithoutNewCreatesObject()
    {
        string testCode =
            """
            class Foo {
                init () {}
            }

            let foo = Foo();
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void InvocationPrefersMethodOverConstructor()
    {
        string testCode =
            """
            class Foo {
                init () {}
            }

            func Foo(x: int) -> int {
                return x;
            }

            let i = Foo(3);
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void InvocationFallsBackToConstructorWhenMethodNotApplicable()
    {
        string testCode =
            """
            class Foo {
                init () {}

                init (x: int) {} 
            }

            func Foo() -> void {}
            
            let i = Foo(1);
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }
}

