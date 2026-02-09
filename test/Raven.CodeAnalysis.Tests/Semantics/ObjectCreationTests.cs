using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ObjectCreationTests : DiagnosticTestBase
{
    [Fact]
    public void InvocationWithoutNewCreatesObject()
    {
        string testCode =
            """
            val foo = Foo();

            class Foo {
                init () {}
            }
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void InvocationPrefersMethodOverConstructor()
    {
        string testCode =
            """
            val i = Foo(3);

            class Foo {
                init () {}
            }

            func Foo(x: int) -> int {
                return x;
            }
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void InvocationFallsBackToConstructorWhenMethodNotApplicable()
    {
        string testCode =
            """
            val i = Foo(1);

            func Foo() -> unit {}

            class Foo {
                init () {}

                init (x: int) {} 
            }
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void NamedConstructorCreatesObject()
    {
        string testCode =
            """
            val p = Person.WithName("John");
            val n = p.GetName();

            class Person {
                var name: string;

                public init WithName(name: string) {
                    self.name = name;
                }

                public GetName() -> string => name;
            }
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void NamedConstructorWithLocalDeclarationCreatesObject()
    {
        string testCode =
            """
            val p = Person.WithName("John");
            val n = p.GetName();

            class Person {
                var name: string;

                public init WithName(name: string) {
                    val temp = name;
                    self.name = temp;
                }

                public GetName() -> string => name;
            }
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void DefaultConstructorSynthesizedWhenMissing()
    {
        string testCode =
            """
            val f = Foo();

            class Foo {
                var x: int;
            }
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

            val list = List<int>()
            list.Add(1)
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void GenericTypeInvocationWithNamespaceImport_ResolvesMembers()
    {
        string testCode =
            """
            import System.Collections.Generic.*

            val list = List<int>()
            list.Add(1)
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void PrimaryConstructor_CreatesInstanceFields()
    {
        string testCode =
            """
            val person = Person("John")
            val name = person.GetName()

            class Person(name: string)
            {
                public GetName() -> string => name
            }
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }
}

