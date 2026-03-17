using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class PragmaWarningSuppressionCodeFixProviderTests : CodeFixTestBase
{
    [Fact]
    public void DoesNotApplyCodeFix_ForErrorDiagnostic()
    {
        const string code = """
func Test() {
    missing
}
""";

        var verifier = CreateCodeFixVerifier<NoOpAnalyzer, PragmaWarningSuppressionCodeFixProvider>(
            code,
            code,
            [new DiagnosticResult(CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id).WithAnySpan()],
            expectedAppliedFixCount: 0);

        verifier.Verify();
    }

    [Fact]
    public void AppliesCodeFix_ForBuiltInAnalyzerId_RAV9012()
    {
        const string code = """
func Test(obj: Foo?) -> () {
    val o = obj
}

class Foo {}
""";

        const string fixedCode = """
#pragma warning disable-next-line RAV9012
func Test(obj: Foo?) -> () {
    #pragma warning disable-next-line RAV9012
    val o = obj
}

class Foo {}
""";

        var verifier = CreateCodeFixVerifier<NonNullDeclarationsAnalyzer, PragmaWarningSuppressionCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult("RAV9012").WithAnySpan()],
            expectedAppliedFixCount: 2);

        verifier.Verify();
    }

    [Fact]
    public void DoesNotApplyCodeFix_ForUnreachableCodeDiagnostic()
    {
        const string code = """
func Test() -> () {
label:
    goto label
label:
    return
}
""";

        var verifier = CreateCodeFixVerifier<NoOpAnalyzer, PragmaWarningSuppressionCodeFixProvider>(
            code,
            code,
            [
                new DiagnosticResult("RAV2500").WithAnySpan(),
                new DiagnosticResult("RAV0162").WithAnySpan()
            ],
            expectedAppliedFixCount: 0);

        verifier.Verify();
    }

    [Fact]
    public void AppliesCodeFix_ForFutureUserAnalyzerDiagnostic()
    {
        const string code = """
func Test() -> () {
    val x = 1
}
""";

        const string fixedCode = """
func Test() -> () {
    #pragma warning disable-next-line USR1234
    val x = 1
}
""";

        var verifier = CreateCodeFixVerifier<UserAnalyzerStub, PragmaWarningSuppressionCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult("USR1234").WithAnySpan()]);

        verifier.Verify();
    }

    private sealed class NoOpAnalyzer : DiagnosticAnalyzer
    {
        public override void Initialize(AnalysisContext context)
        {
        }
    }

    private sealed class UserAnalyzerStub : DiagnosticAnalyzer
    {
        private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
            id: "USR1234",
            title: "User analyzer diagnostic",
            description: null,
            helpLinkUri: string.Empty,
            messageFormat: "User analyzer diagnostic",
            category: "Testing",
            defaultSeverity: DiagnosticSeverity.Warning);

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterSyntaxTreeAction(ctx =>
            {
                var root = ctx.SyntaxTree.GetRoot();
                var token = root.DescendantTokens().FirstOrDefault(t => t.ValueText == "x");
                if (token.Kind != SyntaxKind.IdentifierToken)
                    return;

                ctx.ReportDiagnostic(Diagnostic.Create(Descriptor, token.GetLocation()));
            });
        }
    }
}
