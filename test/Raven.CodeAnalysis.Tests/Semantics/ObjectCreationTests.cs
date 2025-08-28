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

            func Foo() -> unit {}
            
            let i = Foo(1);
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void NamedConstructorCreatesObject()
    {
        string testCode =
            """
            class Person {
                var name: string;

                public init WithName(name: string) {
                    self.name = name;
                }

                public GetName() -> string => name;
            }

            let p = Person.WithName("John");
            let n = p.GetName();
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void NamedConstructorWithLocalDeclarationCreatesObject()
    {
        string testCode =
            """
            class Person {
                var name: string;

                public init WithName(name: string) {
                    let temp = name;
                    self.name = temp;
                }

                public GetName() -> string => name;
            }

            let p = Person.WithName("John");
            let n = p.GetName();
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void DefaultConstructorSynthesizedWhenMissing()
    {
        string testCode =
            """
            class Foo {
                var x: int;
            }

            let f = Foo();
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void GenericTypeInvocationCreatesObject()
    {
        string testCode =
            """
            import System.Collections.Generic.List<>

            let list = List<int>()
            list.Add(1)
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }
}

