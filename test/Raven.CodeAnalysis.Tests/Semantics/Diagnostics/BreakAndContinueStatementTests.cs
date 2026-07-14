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

    [Fact]
    public void BreakToNonLoopLabel_ReportsDiagnostic()
    {
        var code = """
func Main() {
outer: {
        loop {
            break outer;
        }
    }
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics:
            [
                new DiagnosticResult(CompilerDiagnostics.LabelDoesNotIdentifyEnclosingLoop.Id).WithSpan(5, 19, 5, 24)
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

    [Fact]
    public void ContinueToNonEnclosingLoopLabel_ReportsDiagnostic()
    {
        var code = """
func Main() {
outer: loop {
        break;
    }

    loop {
        continue outer;
    }
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics:
            [
                new DiagnosticResult(CompilerDiagnostics.LabelDoesNotIdentifyEnclosingLoop.Id).WithSpan(8, 18, 8, 23)
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

    [Fact]
    public void BreakInLoopStatement_BindsToBoundBreakStatement()
    {
        var code = """
func Main() {
    loop {
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

    [Fact]
    public void BreakInForLoop_BindsToBoundBreakStatement()
    {
        var code = """
import System.Collections.Generic.*

func Main() {
    val values = List<int>()
    for value in values {
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

    [Fact]
    public void BreakInAwaitForLoop_BindsToBoundBreakStatement()
    {
        var code = """
import System.Collections.Generic.*
import System.Threading.Tasks.*

class C {
    async func Main(values: IAsyncEnumerable<int>) -> Task {
        await for value in values {
            break;
        }
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var model = compilation.GetSemanticModel(tree);
        var breakSyntax = tree.GetRoot().DescendantNodes().OfType<BreakStatementSyntax>().Single();

        var bound = model.GetBoundNode(breakSyntax);
        Assert.IsType<BoundBreakStatement>(bound);
    }

    [Fact]
    public void LabeledBreak_BindsTargetLabel()
    {
        var code = """
func Main() {
outer: loop {
        loop {
            break outer;
        }
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var model = compilation.GetSemanticModel(tree);
        var breakSyntax = tree.GetRoot().DescendantNodes().OfType<BreakStatementSyntax>().Single();

        var bound = Assert.IsType<BoundBreakStatement>(model.GetBoundNode(breakSyntax));
        Assert.Equal("outer", bound.TargetLabel?.Name);
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

    [Fact]
    public void ContinueInLoopStatement_BindsToBoundContinueStatement()
    {
        var code = """
func Main() {
    loop {
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

    [Fact]
    public void ContinueInForLoop_BindsToBoundContinueStatement()
    {
        var code = """
import System.Collections.Generic.*

func Main() {
    val values = List<int>()
    for value in values {
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

    [Fact]
    public void ContinueInAwaitForLoop_BindsToBoundContinueStatement()
    {
        var code = """
import System.Collections.Generic.*
import System.Threading.Tasks.*

class C {
    async func Main(values: IAsyncEnumerable<int>) -> Task {
        await for value in values {
            continue;
        }
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var model = compilation.GetSemanticModel(tree);
        var continueSyntax = tree.GetRoot().DescendantNodes().OfType<ContinueStatementSyntax>().Single();

        var bound = model.GetBoundNode(continueSyntax);
        Assert.IsType<BoundContinueStatement>(bound);
    }

    [Fact]
    public void LabeledContinue_BindsTargetLabel()
    {
        var code = """
func Main() {
outer: loop {
        loop {
            continue outer;
        }
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var model = compilation.GetSemanticModel(tree);
        var continueSyntax = tree.GetRoot().DescendantNodes().OfType<ContinueStatementSyntax>().Single();

        var bound = Assert.IsType<BoundContinueStatement>(model.GetBoundNode(continueSyntax));
        Assert.Equal("outer", bound.TargetLabel?.Name);
    }
}
