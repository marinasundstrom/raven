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
    val x = 1
    #pragma warning disable RAV0168
    val x = 2
}
""";

        var (compilation, _) = CreateCompilation(source);

        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Id == "RAV0168");
    }

    [Fact]
    public void PragmaDisableAndRestoreComment_OnlySuppressesBetweenDirectives()
    {
        var source = """
func Main() {
    val x = 1
    // pragma warning disable RAV0168
    val x = 2
    #pragma warning restore RAV0168
    val x = 3
}
""";

        var (compilation, _) = CreateCompilation(source);

        var diagnostics = compilation.GetDiagnostics().Where(diagnostic => diagnostic.Id == "RAV0168").ToArray();
        var diagnostic = Assert.Single(diagnostics);
        Assert.Equal("x", diagnostic.Location.SourceTree!.GetText()!.ToString(diagnostic.Location.SourceSpan));
    }

    [Fact]
    public void PragmaDisableComment_WithReportSuppressedDiagnostics_ReturnsSuppressedDiagnostic()
    {
        var source = """
func Main() {
    val x = 1
    // pragma warning disable RAV0168
    val x = 2
}
""";

        var (compilation, _) = CreateCompilation(source);

        var diagnostics = compilation.GetDiagnostics(new CompilationWithAnalyzersOptions(reportSuppressedDiagnostics: true));
        var diagnostic = Assert.Single(diagnostics, item => item.Id == "RAV0168");
        Assert.True(diagnostic.IsSuppressed);
    }

    [Fact]
    public void PragmaDisableComment_WithoutIds_SuppressesAllDiagnosticsUntilRestore()
    {
        var source = """
func Main() {
    val x = 1
    // pragma warning disable
    val x = 2
    // pragma warning restore
    val x = 3
}
""";

        var (compilation, _) = CreateCompilation(source);

        var diagnostics = compilation.GetDiagnostics().Where(diagnostic => diagnostic.Id == "RAV0168").ToArray();
        var diagnostic = Assert.Single(diagnostics);
        Assert.Equal("x", diagnostic.Location.SourceTree!.GetText()!.ToString(diagnostic.Location.SourceSpan));
    }

    [Fact]
    public void PragmaDisableNextLine_SuppressesOnlyFollowingLine()
    {
        var source = """
func Main() {
    val x = 1
    #pragma warning disable-next-line RAV0168
    val x = 2
    val x = 3
}
""";

        var (compilation, _) = CreateCompilation(source);

        var diagnostics = compilation.GetDiagnostics().Where(diagnostic => diagnostic.Id == "RAV0168").ToArray();
        var diagnostic = Assert.Single(diagnostics);
        Assert.Equal("x", diagnostic.Location.SourceTree!.GetText()!.ToString(diagnostic.Location.SourceSpan));
    }

    [Fact]
    public void PragmaDisableComment_DoesNotSuppressErrorDiagnostic()
    {
        var source = """
func Main() {
    #pragma warning disable RAV0103
    missing
}
""";

        var (compilation, _) = CreateCompilation(source);

        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, diagnostic => diagnostic.Id == "RAV0103");
    }

    [Fact]
    public void PragmaDisableComment_DoesNotSuppressUnreachableCode()
    {
        var source = """
func Main() {
    #pragma warning disable RAV0162
label:
    goto label
label:
    return
}
""";

        var (compilation, _) = CreateCompilation(source);

        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, diagnostic => diagnostic.Id == "RAV0162");
    }
}
