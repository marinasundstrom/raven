using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class GotoStatementDiagnosticsTests : DiagnosticTestBase
{
    [Fact]
    public void DuplicateLabel_ReportsDiagnostic()
    {
        var code = """
func main() {
label:
    goto label
label:
    return
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics: [
                new DiagnosticResult("RAV2500").WithSpan(4, 1, 4, 6).WithArguments("label")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void GotoUndefinedLabel_ReportsDiagnostic()
    {
        var code = """
func main() {
    goto missing
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics: [
                new DiagnosticResult("RAV2501").WithSpan(2, 10, 2, 17).WithArguments("missing")
            ]);

        verifier.Verify();
    }
}

public class GotoStatementSemanticTests : CompilationTestBase
{
    [Fact]
    public void GetDeclaredSymbol_ForLabel_ReturnsLabelSymbol()
    {
        var code = """
func main() {
label:
    return
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var model = compilation.GetSemanticModel(tree);
        var labeled = tree.GetRoot().DescendantNodes().OfType<LabeledStatementSyntax>().Single();

        var symbol = Assert.IsAssignableFrom<ILabelSymbol>(model.GetDeclaredSymbol(labeled));
        Assert.Equal("label", symbol.Name);
    }

    [Fact]
    public void GetSymbolInfo_ForGoto_ReturnsLabelSymbol()
    {
        var code = """
func main() {
label:
    goto label
    return
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var model = compilation.GetSemanticModel(tree);
        var gotoStatement = tree.GetRoot().DescendantNodes().OfType<GotoStatementSyntax>().Single();

        var info = model.GetSymbolInfo(gotoStatement);
        var symbol = Assert.IsAssignableFrom<ILabelSymbol>(info.Symbol);
        Assert.Equal("label", symbol.Name);
    }

    [Fact]
    public void AnalyzeControlFlow_GotoEnteringRegion_ReportsEntry()
    {
        var code = """
func main() {
    goto target
target:
    return
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var model = compilation.GetSemanticModel(tree);
        var labeled = tree.GetRoot().DescendantNodes().OfType<LabeledStatementSyntax>().Single();

        var analysis = model.AnalyzeControlFlow(labeled);

        var entry = Assert.Single(analysis.EntryPoints);
        Assert.Same(labeled, entry);
    }

    [Fact]
    public void HasExternalGotoToLabel_GotoOutsideRegion_ReturnsTrue()
    {
        var code = """
func main() {
    goto target
target:
    return
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var model = compilation.GetSemanticModel(tree);
        var labeled = tree.GetRoot().DescendantNodes().OfType<LabeledStatementSyntax>().Single();

        var region = new ControlFlowRegion(labeled);

        Assert.True(model.HasExternalGotoToLabel(labeled, region));
    }
}
