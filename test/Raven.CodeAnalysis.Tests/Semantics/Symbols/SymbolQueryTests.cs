using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class SymbolQueryTests : DiagnosticTestBase
{
    [Fact]
    public void CallingInstanceMethodAsStatic_ProducesDiagnostic()
    {
        string testCode =
            """
            class Foo {
                M() {}
            }

            Foo.M();
            """;

        var verifier = CreateVerifier(testCode,
            [new DiagnosticResult(CompilerDiagnostics.MemberDoesNotContainDefinition.Id).WithSpan(5, 5, 5, 6).WithArguments("Foo", "M")]);

        verifier.Verify();
    }

    [Fact]
    public void NestedType_IsAvailableThroughConstructedTypeExpression()
    {
        string testCode =
            """
            class Foo<T>
            {
                public class Bar
                {
                    val value: T
                }
            }

            val bar = Foo<int>.Bar();
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }
}
