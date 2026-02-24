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
                public init () {}
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
                public init () {}

                public init (x: int) {} 
            }
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void StaticFactoryMethodCreatesObject()
    {
        string testCode =
            """
            val p = Person.WithName("John");
            val n = p.GetName();

            class Person {
                var name: string;

                public static func WithName(name: string) -> Person {
                    return Person(name);
                }

                public init (name: string) {
                    self.name = name
                }

                public func GetName() -> string => name;
            }
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void StaticFactoryMethodWithLocalDeclarationCreatesObject()
    {
        string testCode =
            """
            val p = Person.WithName("John");
            val n = p.GetName();

            class Person {
                var name: string;

                public static func WithName(name: string) -> Person {
                    val temp = name;
                    return Person(temp);
                }

                public init (name: string) {
                    self.name = name;
                }

                public func GetName() -> string => name;
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
                public func GetName() -> string => name
            }
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }
}
