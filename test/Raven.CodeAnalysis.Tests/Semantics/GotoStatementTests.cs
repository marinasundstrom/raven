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
                new DiagnosticResult("RAV2500").WithSpan(2, 1, 2, 6).WithArguments("label")
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

    [Fact]
    public void LabelUsingReservedWord_ReportsDiagnostic()
    {
        var code = """
func main() {
    int:
        return
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics: [
                new DiagnosticResult("RAV2502").WithSpan(2, 5, 2, 8).WithArguments("int")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void GotoUsingReservedWord_ReportsDiagnostic()
    {
        var code = """
func main() {
    goto int
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics: [
                new DiagnosticResult("RAV2502").WithSpan(2, 10, 2, 13).WithArguments("int")
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
    public void GetSymbolInfo_ForGotoWithEscapedIdentifier_ReturnsLabelSymbol()
    {
        var code = """
func main() {
@loop:
    goto @loop
    return
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var model = compilation.GetSemanticModel(tree);
        var labeled = tree.GetRoot().DescendantNodes().OfType<LabeledStatementSyntax>().Single();
        var gotoStatement = tree.GetRoot().DescendantNodes().OfType<GotoStatementSyntax>().Single();

        var declared = Assert.IsAssignableFrom<ILabelSymbol>(model.GetDeclaredSymbol(labeled));
        Assert.Equal("loop", declared.Name);

        var info = model.GetSymbolInfo(gotoStatement);
        var symbol = Assert.IsAssignableFrom<ILabelSymbol>(info.Symbol);
        Assert.Equal("loop", symbol.Name);
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
