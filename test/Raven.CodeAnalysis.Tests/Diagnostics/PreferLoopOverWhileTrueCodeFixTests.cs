using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public sealed class PreferLoopOverWhileTrueCodeFixTests : CodeFixTestBase
{
    [Fact]
    public void AppliesCodeFix_RewritesWhileTrueToLoop()
    {
        const string code = "while true { break }";
        const string fixedCode = "loop { break }";

        var verifier = CreateCodeFixVerifier<PreferLoopOverWhileTrueAnalyzer, PreferLoopOverWhileTrueCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(PreferLoopOverWhileTrueAnalyzer.DiagnosticId).WithAnySpan()],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void AppliesCodeFix_RemovesParenthesizedTrueCondition()
    {
        const string code = "while ((true)) { break }";
        const string fixedCode = "loop { break }";

        var verifier = CreateCodeFixVerifier<PreferLoopOverWhileTrueAnalyzer, PreferLoopOverWhileTrueCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(PreferLoopOverWhileTrueAnalyzer.DiagnosticId).WithAnySpan()],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }
}
