using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class RemoveRedundantImportCodeFixTests : CodeFixTestBase
{
    [Fact]
    public void AppliesCodeFix_RemovesImportAlreadyCoveredByGlobalImport()
    {
        const string code = """
import System.*
import System.Console.*

global {
    import System.*
}

WriteLine("ok")
""";

        const string fixedCode = """
import System.Console.*

global {
    import System.*
}

WriteLine("ok")
""";

        var verifier = CreateCodeFixVerifier<NoOpAnalyzer, RemoveRedundantImportCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(CompilerDiagnostics.ImportDirectiveRedundantWithGlobalImport.Id).WithAnySpan()]);

        verifier.Verify();
    }

    [Fact]
    public void AppliesCodeFix_RemovesAllImportsAlreadyCoveredByGlobalImports()
    {
        const string code = """
import System.*
import System.Console.*
import System.Text.*

global {
    import System.*
    import System.Console.*
}

WriteLine("ok")
""";

        const string fixedCode = """
import System.Text.*

global {
    import System.*
    import System.Console.*
}

WriteLine("ok")
""";

        var verifier = CreateCodeFixVerifier<NoOpAnalyzer, RemoveRedundantImportCodeFixProvider>(
            code,
            fixedCode,
            [
                new DiagnosticResult(CompilerDiagnostics.ImportDirectiveRedundantWithGlobalImport.Id).WithAnySpan(),
                new DiagnosticResult(CompilerDiagnostics.ImportDirectiveRedundantWithGlobalImport.Id).WithAnySpan()
            ]);

        verifier.Verify();
    }

    private sealed class NoOpAnalyzer : DiagnosticAnalyzer
    {
        public override void Initialize(AnalysisContext context)
        {
        }
    }
}
