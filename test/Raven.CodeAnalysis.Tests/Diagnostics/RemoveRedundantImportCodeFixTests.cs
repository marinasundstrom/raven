using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Text;

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

    [Fact]
    public void RemoveAllCodeFix_UsesProvidedDiagnosticsWithoutRecomputingAnalyzers()
    {
        CountingAnalyzer.AnalyzeCallCount = 0;

        const string code = """
import System.Text.*
import System.Net.*
import System.*

val text: String = "ok"
""";

        var workspace = RavenWorkspace.Create(targetFramework: TestTargetFramework.Default);
        var projectId = workspace.AddProject("Test");
        var documentId = DocumentId.CreateNew(projectId);
        var solution = workspace.CurrentSolution.AddDocument(documentId, "test.rvn", SourceText.From(code));
        workspace.TryApplyChanges(solution);

        var project = workspace.CurrentSolution.GetProject(projectId)!;
        project = project.AddAnalyzerReference(new AnalyzerReference(new CountingAnalyzer()));
        workspace.TryApplyChanges(project.Solution);

        var document = workspace.CurrentSolution.GetDocument(documentId)!;
        var syntaxTree = document.GetSyntaxTreeAsync().GetAwaiter().GetResult()!;
        var root = (CompilationUnitSyntax)syntaxTree.GetRoot();
        var descriptor = DiagnosticDescriptor.Create(
            UnusedImportDirectiveAnalyzer.DiagnosticId,
            "Import directive is unused",
            null,
            string.Empty,
            "Import directive '{0}' is unused within this scope.",
            "Usage",
            DiagnosticSeverity.Hidden);
        var diagnostics = new[]
        {
            Diagnostic.Create(descriptor, root.Imports[0].Name.GetLocation(), "System.Text"),
            Diagnostic.Create(descriptor, root.Imports[1].Name.GetLocation(), "System.Net")
        };

        var fixes = workspace.GetCodeFixes(projectId, [new RemoveRedundantImportCodeFixProvider()], diagnostics);

        Assert.Equal(0, CountingAnalyzer.AnalyzeCallCount);
        Assert.Contains(fixes, fix => fix.Action.Title == "Remove all redundant or unused imports");
    }

    private sealed class NoOpAnalyzer : DiagnosticAnalyzer
    {
        public override void Initialize(AnalysisContext context)
        {
        }
    }

    private sealed class CountingAnalyzer : DiagnosticAnalyzer
    {
        public static int AnalyzeCallCount;

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterSyntaxTreeAction(_ => AnalyzeCallCount++);
        }
    }
}
