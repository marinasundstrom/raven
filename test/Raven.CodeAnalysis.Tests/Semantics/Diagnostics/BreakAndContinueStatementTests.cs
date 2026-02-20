using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class BreakStatementDiagnosticsTests : DiagnosticTestBase
{
    [Fact]
    public void BreakOutsideLoop_ReportsDiagnostic()
    {
        var code = """
func Main() {
    break;
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics:
            [
                new DiagnosticResult(CompilerDiagnostics.BreakStatementNotWithinLoop.Id).WithSpan(2, 5, 2, 10)
            ]);

        verifier.Verify();
    }

    [Fact]
    public void BreakInExpressionContext_ReportsDiagnostic()
    {
        var code = """
func Main() {
    val value = {
        break;
        ()
    };
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics:
            [
                new DiagnosticResult(CompilerDiagnostics.BreakStatementNotWithinLoop.Id).WithSpan(3, 9, 3, 14),
                new DiagnosticResult(CompilerDiagnostics.UnreachableCodeDetected.Id).WithSpan(4, 9, 4, 11)
            ]);

        verifier.Verify();
    }
}

public class ContinueStatementDiagnosticsTests : DiagnosticTestBase
{
    [Fact]
    public void ContinueOutsideLoop_ReportsDiagnostic()
    {
        var code = """
func Main() {
    continue;
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics:
            [
                new DiagnosticResult(CompilerDiagnostics.ContinueStatementNotWithinLoop.Id).WithSpan(2, 5, 2, 13)
            ]);

        verifier.Verify();
    }

    [Fact]
    public void ContinueInExpressionContext_ReportsDiagnostic()
    {
        var code = """
func Main() {
    val value = {
        continue;
        ()
    };
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics:
            [
                new DiagnosticResult(CompilerDiagnostics.ContinueStatementNotWithinLoop.Id).WithSpan(3, 9, 3, 17),
                new DiagnosticResult(CompilerDiagnostics.UnreachableCodeDetected.Id).WithSpan(4, 9, 4, 11)
            ]);

        verifier.Verify();
    }
}

public class BreakStatementSemanticTests : CompilationTestBase
{
    [Fact]
    public void BreakInWhileLoop_BindsToBoundBreakStatement()
    {
        var code = """
func Main() {
    while true {
        break;
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var model = compilation.GetSemanticModel(tree);
        var breakSyntax = tree.GetRoot().DescendantNodes().OfType<BreakStatementSyntax>().Single();

        var bound = model.GetBoundNode(breakSyntax);
        Assert.IsType<BoundBreakStatement>(bound);
    }
}

public class ContinueStatementSemanticTests : CompilationTestBase
{
    [Fact]
    public void ContinueInWhileLoop_BindsToBoundContinueStatement()
    {
        var code = """
func Main() {
    while true {
        continue;
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var model = compilation.GetSemanticModel(tree);
        var continueSyntax = tree.GetRoot().DescendantNodes().OfType<ContinueStatementSyntax>().Single();

        var bound = model.GetBoundNode(continueSyntax);
        Assert.IsType<BoundContinueStatement>(bound);
    }
}
