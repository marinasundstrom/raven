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
            [new DiagnosticResult("RAV0117").WithLocation(6, 5).WithArguments("Foo", "Bar")]);

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
            let f = Foo()
            f.Bar;
            """;

        var verifier = CreateVerifier(testCode,
            [new DiagnosticResult("RAV0103").WithLocation(7, 3).WithArguments("Bar")]);

        verifier.Verify();
    }
}
