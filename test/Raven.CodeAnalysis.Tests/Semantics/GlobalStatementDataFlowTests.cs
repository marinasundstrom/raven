using System.Linq;
using Raven.CodeAnalysis.Syntax;
using Shouldly;

namespace Raven.CodeAnalysis.Tests.Semantics;

public class GlobalStatementDataFlowTests
{
    [Fact]
    public void GlobalForEach_AnalyzesAsImperativeLoop()
    {
        var code = """
import System.Console.*
let numbers = [1, 2, 3]
let total = 0
for each number in numbers {
    total = total + number
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var forExpression = syntaxTree.GetRoot().DescendantNodes().OfType<ForExpressionSyntax>().Single();
        var block = (BlockSyntax)forExpression.Body;
        var totalDeclarator = syntaxTree.GetRoot().DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(d => d.Identifier.ValueText == "total");

        semanticModel.GetDeclaredSymbol(totalDeclarator).ShouldNotBeNull();

        var analysis = semanticModel.AnalyzeDataFlow(block);

        analysis.Succeeded.ShouldBeTrue();
        analysis.DefinitelyAssignedOnEntry.Select(s => s.Name).ShouldContain("total");
    }
}
