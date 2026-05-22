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

    [Fact]
    public void AppliesCodeFix_RemovesUnusedImport()
    {
        const string code = """
import System.Text.*
import System.*

val text: String = "ok"
""";

        const string fixedCode = """
import System.*

val text: String = "ok"
""";

        var verifier = CreateCodeFixVerifier<UnusedImportDirectiveAnalyzer, RemoveRedundantImportCodeFixProvider>(
            code,
            fixedCode,
            [
                new DiagnosticResult(UnusedImportDirectiveAnalyzer.DiagnosticId)
                    .WithAnySpan()
                    .WithArguments("System.Text")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void AppliesCodeFix_RemovesAllUnusedImports()
    {
        const string code = """
import System.Text.*
import System.Net.*
import System.*

val text: String = "ok"
""";

        const string fixedCode = """
import System.*

val text: String = "ok"
""";

        var verifier = CreateCodeFixVerifier<UnusedImportDirectiveAnalyzer, RemoveRedundantImportCodeFixProvider>(
            code,
            fixedCode,
            [
                new DiagnosticResult(UnusedImportDirectiveAnalyzer.DiagnosticId)
                    .WithAnySpan()
                    .WithArguments("System.Text"),
                new DiagnosticResult(UnusedImportDirectiveAnalyzer.DiagnosticId)
                    .WithAnySpan()
                    .WithArguments("System.Net")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void AppliesCodeFix_RemovesAllRedundantAndUnusedImports()
    {
        const string code = """
import System.*
import System.Text.*
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

        var verifier = CreateCodeFixVerifier<UnusedImportDirectiveAnalyzer, RemoveRedundantImportCodeFixProvider>(
            code,
            fixedCode,
            [
                new DiagnosticResult(CompilerDiagnostics.ImportDirectiveRedundantWithGlobalImport.Id)
                    .WithAnySpan(),
                new DiagnosticResult(UnusedImportDirectiveAnalyzer.DiagnosticId)
                    .WithAnySpan()
                    .WithArguments("System.Text")
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
