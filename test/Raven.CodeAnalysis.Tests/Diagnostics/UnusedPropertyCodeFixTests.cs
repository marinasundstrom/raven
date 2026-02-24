using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class UnusedPropertyCodeFixTests : CodeFixTestBase
{
    [Fact]
    public void AppliesCodeFix_RemovesUnusedProperty()
    {
        const string code = """
val x = 0

class C {
    val Name: string = "Raven"
}
""";

        const string fixedCode = """
val x = 0

class C {}
""";

        var verifier = CreateCodeFixVerifier<UnusedPropertyAnalyzer, UnusedPropertyCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(UnusedPropertyAnalyzer.DiagnosticId).WithAnySpan()]);

        verifier.Verify();
    }
}
