using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class MergeStringLiteralConcatenationCodeFixTests : CodeFixTestBase
{
    [Fact]
    public void AppliesCodeFix_MergesStringLiteralChain()
    {
        const string code = """
func message() -> string {
    return "Hello" + ", " + "world"
}
""";

        const string fixedCode = """
func message() -> string {
    return "Hello, world"
}
""";

        var verifier = CreateCodeFixVerifier<MergeStringLiteralConcatenationAnalyzer, MergeStringLiteralConcatenationCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(MergeStringLiteralConcatenationAnalyzer.DiagnosticId).WithAnySpan()],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }
}
