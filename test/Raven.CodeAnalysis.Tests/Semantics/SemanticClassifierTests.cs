using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;
using System.Linq;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class SemanticClassifierTests : CompilationTestBase
{
    [Fact]
    public void ClassifiesTokensBySymbol()
    {
        var source = """
namespace N { class C { method M() -> unit {} } let x = M() }
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var result = SemanticClassifier.Classify(tree.GetRoot(), model);

        var nsToken = result.Tokens.Keys.First(t => t.Text == "N");
        result.Tokens[nsToken].ShouldBe(SemanticClassification.Namespace);

        var typeToken = result.Tokens.Keys.First(t => t.Text == "C");
        result.Tokens[typeToken].ShouldBe(SemanticClassification.Type);

        var methodToken = result.Tokens.Keys.First(t => t.Text == "M");
        result.Tokens[methodToken].ShouldBe(SemanticClassification.Method);
    }
}
