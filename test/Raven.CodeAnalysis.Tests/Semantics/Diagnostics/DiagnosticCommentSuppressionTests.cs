using System.Linq;

using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class DiagnosticCommentSuppressionTests : CompilationTestBase
{
    [Fact]
    public void PragmaDisableComment_SuppressesMatchingDiagnostic()
    {
        var source = """
func Main() {
    #pragma warning disable RAV0103
    missing
}
""";

        var (compilation, _) = CreateCompilation(source);

        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Id == "RAV0103");
    }

    [Fact]
    public void PragmaDisableAndRestoreComment_OnlySuppressesBetweenDirectives()
    {
        var source = """
func Main() {
    // pragma warning disable RAV0103
    missing1
    #pragma warning restore RAV0103
    missing2
}
""";

        var (compilation, _) = CreateCompilation(source);

        var diagnostics = compilation.GetDiagnostics().Where(diagnostic => diagnostic.Id == "RAV0103").ToArray();
        var diagnostic = Assert.Single(diagnostics);
        Assert.Equal("missing2", diagnostic.Location.SourceTree!.GetText()!.ToString(diagnostic.Location.SourceSpan));
    }

    [Fact]
    public void PragmaDisableComment_WithReportSuppressedDiagnostics_ReturnsSuppressedDiagnostic()
    {
        var source = """
func Main() {
    // pragma warning disable RAV0103
    missing
}
""";

        var (compilation, _) = CreateCompilation(source);

        var diagnostics = compilation.GetDiagnostics(new CompilationWithAnalyzersOptions(reportSuppressedDiagnostics: true));
        var diagnostic = Assert.Single(diagnostics, item => item.Id == "RAV0103");
        Assert.True(diagnostic.IsSuppressed);
    }

    [Fact]
    public void PragmaDisableComment_WithoutIds_SuppressesAllDiagnosticsUntilRestore()
    {
        var source = """
func Main() {
    // pragma warning disable
    missing1
    // pragma warning restore
    missing2
}
""";

        var (compilation, _) = CreateCompilation(source);

        var diagnostics = compilation.GetDiagnostics().Where(diagnostic => diagnostic.Id == "RAV0103").ToArray();
        var diagnostic = Assert.Single(diagnostics);
        Assert.Equal("missing2", diagnostic.Location.SourceTree!.GetText()!.ToString(diagnostic.Location.SourceSpan));
    }

    [Fact]
    public void PragmaDisableNextLine_SuppressesOnlyFollowingLine()
    {
        var source = """
func Main() {
    #pragma warning disable-next-line RAV0103
    missing1
    missing2
}
""";

        var (compilation, _) = CreateCompilation(source);

        var diagnostics = compilation.GetDiagnostics().Where(diagnostic => diagnostic.Id == "RAV0103").ToArray();
        var diagnostic = Assert.Single(diagnostics);
        Assert.Equal("missing2", diagnostic.Location.SourceTree!.GetText()!.ToString(diagnostic.Location.SourceSpan));
    }
}
