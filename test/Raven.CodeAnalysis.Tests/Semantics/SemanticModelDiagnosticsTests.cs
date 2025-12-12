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
    M() {
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
func main() {
    if true {
        )
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(syntaxTree);
        var model = compilation.GetSemanticModel(syntaxTree);

        var diagnostics = model.GetDiagnostics();
        _ = diagnostics.Count;

        var incompleteStatement = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<IncompleteStatementSyntax>()
            .Single();

        Assert.IsType<BoundExpressionStatement>(model.GetBoundNode(incompleteStatement));
    }
}
