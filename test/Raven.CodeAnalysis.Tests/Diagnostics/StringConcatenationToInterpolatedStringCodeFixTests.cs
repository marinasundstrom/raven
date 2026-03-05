using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class StringConcatenationToInterpolatedStringCodeFixTests : CodeFixTestBase
{
    [Fact]
    public void AppliesCodeFix_InterpolatesVariableAndMergesConstants()
    {
        const string code = """
func message(x: string) -> string {
    return "test " + 1 + " " + x
}
""";

        const string fixedCode = """
func message(x: string) -> string {
    return "test 1 ${x}"
}
""";

        var verifier = CreateCodeFixVerifier<StringConcatenationAnalyzer, StringConcatenationToInterpolatedStringCodeFixProvider>(
            code,
            fixedCode,
            [
                new DiagnosticResult(StringConcatenationAnalyzer.DiagnosticId).WithAnySpan(),
                new DiagnosticResult(StringConcatenationAnalyzer.MergeDiagnosticId).WithAnySpan()
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void DoesNotApplyCodeFix_WhenOnlyStringAndConstant()
    {
        const string code = """
func message() -> string {
    return "test " + 1
}
""";

        var verifier = CreateCodeFixVerifier<StringConcatenationAnalyzer, StringConcatenationToInterpolatedStringCodeFixProvider>(
            code,
            code,
            [new DiagnosticResult(StringConcatenationAnalyzer.MergeDiagnosticId).WithAnySpan()],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id],
            expectedAppliedFixCount: 0);

        verifier.Verify();
    }
}
