using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class MultipleSyntaxTreeTests
{
    [Fact]
    public void MultipleSyntaxTrees_ReferenceAcrossTrees_ProducesNoDiagnostics()
    {
        var tree1 = SyntaxTree.ParseText("""
class Helper {
    init () {}

    public GetValue() -> int => 42;
}
""");

        var tree2 = SyntaxTree.ParseText("""
class Program {
    static Main() -> unit {
        let helper = Helper();
        let value = helper.GetValue();
        return;
    }
}
""");

        var compilation = Compilation.Create(
            "app",
            new[] { tree1, tree2 },
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var semanticModel = compilation.GetSemanticModel(tree2);
        var diagnostics = semanticModel.GetDiagnostics();

        Assert.DoesNotContain(diagnostics, d => d.Severity == DiagnosticSeverity.Error);
    }
}
