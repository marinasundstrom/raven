using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class SemanticModelDiagnosticsTests : CompilationTestBase
{
    [Fact]
    public void GetDiagnostics_CollectsMethodBodyDiagnostics()
    {
        var source = """
class Test {
    func M() {
        1();
    }
}
""";
        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(syntaxTree);
        var model = compilation.GetSemanticModel(syntaxTree);

        var diagnostics = model.GetDiagnostics();

        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.InvalidInvocation);
    }

    [Fact]
    public void GetDiagnostics_IncompleteStatement_DoesNotCrashBinder()
    {
        const string source = """
func Main() {
    if true {
        )
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(syntaxTree);
        var model = compilation.GetSemanticModel(syntaxTree);

        var diagnostics = compilation.GetDiagnostics();
        _ = diagnostics.Length;

        var incompleteStatement = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<IncompleteStatementSyntax>()
            .Single();

        Assert.IsType<BoundExpressionStatement>(model.GetBoundNode(incompleteStatement));
    }

    [Fact]
    public void GetDiagnostics_MalformedInvocationInMatchArm_DoesNotCrashAndReportsMissingParen()
    {
        const string source = """
import System.*
import System.Console.*

func Main() -> () {
    val x = 1
    match x {
        2 => WriteLine(("Yes")
        _ => WriteLine(("No")
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(syntaxTree);
        _ = compilation.GetSemanticModel(syntaxTree);

        var diagnostics = compilation.GetDiagnostics();
        _ = diagnostics.Length;

        Assert.Contains(
            diagnostics,
            diagnostic => diagnostic.Descriptor.Id == CompilerDiagnostics.CharacterExpected.Id
                          && diagnostic.GetMessage().Contains("')' expected", System.StringComparison.Ordinal));
    }
}
