using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Semantics.Diagnostics;

public class UninitializedLocalDiagnosticsTests : DiagnosticTestBase
{
    [Fact]
    public void VarDeclarationWithoutInitializer_ProducesDiagnostic()
    {
        const string code = """
        class Foo {
            func Test() -> unit {
                var x: int;
            }
        }
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult("RAV0166").WithAnySpan().WithArguments("x")
        ]);
        verifier.Verify();
    }

    [Fact]
    public void LetDeclarationWithoutInitializer_ProducesDiagnostic()
    {
        const string code = """
        class Foo {
            func Test() -> unit {
                val x: int;
            }
        }
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult("RAV0166").WithAnySpan().WithArguments("x")
        ]);
        verifier.Verify();
    }
}
