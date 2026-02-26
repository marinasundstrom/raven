using System.Linq;

using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class MethodFuncKeywordDiagnosticsTests : CompilationTestBase
{
    [Fact]
    public void MethodDeclaration_WithoutFuncKeyword_ReportsParserErrors()
    {
        const string source = """
class C {
    M() -> unit { return; }
}
""";

        var (compilation, _) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Severity == DiagnosticSeverity.Error);
        Assert.DoesNotContain(diagnostics, d => d.Id == CompilerDiagnostics.MethodDeclarationMissingFuncKeyword.Id);
    }

    [Fact]
    public void MethodDeclaration_WithFuncKeyword_DoesNotReportDiagnostic()
    {
        const string source = """
class C {
    func M() -> unit { return; }
}
""";

        var (compilation, _) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var diagnostics = compilation.GetDiagnostics()
            .Where(d => d.Id == CompilerDiagnostics.MethodDeclarationMissingFuncKeyword.Id)
            .ToArray();

        Assert.Empty(diagnostics);
    }
}
