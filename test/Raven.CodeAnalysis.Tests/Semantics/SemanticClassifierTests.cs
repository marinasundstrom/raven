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
namespace N { class C { let f = 0 public P: int => f method M(p: int) -> int { let l = p l } } }
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

        var fieldToken = result.Tokens.Keys.First(t => t.Text == "f");
        result.Tokens[fieldToken].ShouldBe(SemanticClassification.Field);

        var propertyToken = result.Tokens.Keys.First(t => t.Text == "P");
        result.Tokens[propertyToken].ShouldBe(SemanticClassification.Property);

        var parameterToken = result.Tokens.Keys.First(t => t.Text == "p");
        result.Tokens[parameterToken].ShouldBe(SemanticClassification.Parameter);

        var localToken = result.Tokens.Keys.First(t => t.Text == "l");
        result.Tokens[localToken].ShouldBe(SemanticClassification.Local);
    }
}
