using System.Linq;

using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class DiagnosticCommentSuppressionTests : CompilationTestBase
{
    [Fact]
    public void PragmaDisableComment_SuppressesMatchingDiagnostic()
    {
        var source = """
func Test(x: int) {
    #pragma warning disable RAV0168
    {
        let x = 2
    }
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
func Test(x: int) {
    // pragma warning disable RAV0168
    {
        let x = 2
    }
    #pragma warning restore RAV0168
    {
        let x = 3
    }
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
func Test(x: int) {
    // pragma warning disable RAV0168
    {
        let x = 2
    }
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
func Test(x: int) {
    // pragma warning disable
    {
        let x = 2
    }
    // pragma warning restore
    {
        let x = 3
    }
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
func Test(x: int) {
    {
        #pragma warning disable-next-line RAV0168
        let x = 2
    }
    {
        let x = 3
    }
}
""";

        var (compilation, _) = CreateCompilation(source);

        var diagnostics = compilation.GetDiagnostics().Where(diagnostic => diagnostic.Id == "RAV0168").ToArray();
        var diagnostic = Assert.Single(diagnostics);
        Assert.Equal("x", diagnostic.Location.SourceTree!.GetText()!.ToString(diagnostic.Location.SourceSpan));
    }

    [Fact]
    public void PragmaDisable_SupportsMultipleDiagnosticIdsOnOneLine()
    {
        var source = """
func Test(x: int) {
    #pragma warning disable RAV0168 RAV9019
    {
        let x = 1
    }
    func unused() -> () {}
}
""";

        var (compilation, _) = CreateCompilation(source);

        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Id == "RAV0168");
        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Id == "RAV9019");
    }

    [Fact]
    public void PragmaDisableNextLine_SupportsMultipleDiagnosticIdsOnOneLine()
    {
        var source = """
func Test(x: int) {
    {
        #pragma warning disable-next-line RAV0168 RAV9012
        let x = 2
    }
    {
        let x = 3
    }
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
