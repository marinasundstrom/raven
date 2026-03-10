using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class ConversionCastCodeFixTests : CodeFixTestBase
{
    [Fact]
    public void ExplicitConversionHint_AddsCast()
    {
        const string code = """
class Flag {
    static func explicit(value: Flag) -> bool { return true }
}

func Test(flag: Flag) -> () {
    if flag {
    }
}
""";

        const string fixedCode = """
class Flag {
    static func explicit(value: Flag) -> bool { return true }
}

func Test(flag: Flag) -> () {
    if (bool)flag {
    }
}
""";

        var verifier = CreateCodeFixVerifier<NoOpAnalyzer, ConversionCastCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(CompilerDiagnostics.ExplicitConversionExists.Id).WithAnySpan()]);

        verifier.Verify();
    }

    [Fact]
    public void RedundantCastHint_RemovesCast()
    {
        const string code = """
val x = (double)1
""";

        const string fixedCode = """
val x = 1
""";

        var verifier = CreateCodeFixVerifier<NoOpAnalyzer, ConversionCastCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(CompilerDiagnostics.RedundantExplicitCast.Id).WithAnySpan()]);

        verifier.Verify();
    }

    private sealed class NoOpAnalyzer : DiagnosticAnalyzer
    {
        public override void Initialize(AnalysisContext context)
        {
        }
    }
}
