using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class DiscardExpressionSemanticTests : DiagnosticTestBase
{
    [Fact]
    public void DiscardDeclarationAndAssignment_DoNotDeclareWritableLocal()
    {
        const string source = """
func Main() -> unit {
    val _ = 1
    _ = 2
}
""";

        var verifier = CreateVerifier(source);
        var result = verifier.GetResult();
        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var declarator = root.DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var assignment = root.DescendantNodes().OfType<AssignmentStatementSyntax>().Single();

        Assert.Null(model.GetDeclaredSymbol(declarator));

        var analysis = model.AnalyzeDataFlow(assignment);
        Assert.True(analysis.Succeeded);
        Assert.Empty(analysis.WrittenInside);
        Assert.Empty(analysis.DataFlowsOut);

        verifier.Verify();
    }

    [Fact]
    public void DiscardExpression_ReportsDiagnostic()
    {
        const string source = "_ + 2";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.DiscardExpressionNotAllowed.Id)
                    .WithSpan(1, 1, 1, 2)
            ]);

        verifier.Verify();
    }
}
