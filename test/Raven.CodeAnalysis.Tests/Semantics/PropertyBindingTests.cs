using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class PropertyBindingTests : DiagnosticTestBase
{
    [Fact]
    public void AccessingInstancePropertyAsStatic_ProducesDiagnostic()
    {
        string testCode =
            """
            class Foo {
                public Bar: int {
                    get => 0
                }
            }
            Foo.Bar
            """;

        var verifier = CreateVerifier(testCode,
            [new DiagnosticResult("RAV0117").WithSpan(6, 5, 6, 8).WithArguments("Foo", "Bar")]);

        verifier.Verify();
    }

    [Fact]
    public void AccessingStaticPropertyAsInstance_ProducesDiagnostic()
    {
        string testCode =
            """
            class Foo {
                public static Bar: int {
                    get => 0
                }
            }
            val f = Foo()
            f.Bar;
            """;

        var verifier = CreateVerifier(testCode,
            [new DiagnosticResult("RAV0103").WithSpan(7, 3, 7, 6).WithArguments("Bar")]);

        verifier.Verify();
    }

    [Fact]
    public void GetterOnlyAutoProperty_AssignableInConstructor()
    {
        string testCode =
            """
            class Person {
                public Name: string { get; }
                init (name: string) {
                    Name = name;
                }
            }
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void GetterOnlyAutoProperty_AssignableInNamedConstructor()
    {
        string testCode =
            """
            class Person {
                public Name: string { get; }
                public init WithName(name: string) {
                    self.Name = name;
                }
            }
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void GetterOnlyAutoProperty_AssignmentOutsideConstructor_ProducesDiagnostic()
    {
        string testCode =
            """
            class Person {
                public Name: string { get; }
                public SetName(name: string) -> unit {
                    Name = name;
                }
            }
            """;

        var verifier = CreateVerifier(testCode,
            [new DiagnosticResult("RAV0200").WithSpan(4, 9, 4, 13).WithArguments("Name")]);

        verifier.Verify();
    }

    [Fact]
    public void GetterOnlyPropertyWithAccessorBody_AssignmentInConstructor_ProducesDiagnostic()
    {
        string testCode =
            """
            class Person {
                public Name: string {
                    get => ""
                }
                init (name: string) {
                    Name = name;
                }
            }
            """;

        var verifier = CreateVerifier(testCode,
            [new DiagnosticResult("RAV0200").WithSpan(6, 9, 6, 13).WithArguments("Name")]);

        verifier.Verify();
    }
}
