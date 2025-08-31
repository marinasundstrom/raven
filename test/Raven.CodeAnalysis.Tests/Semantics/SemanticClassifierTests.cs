using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

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

    [Fact]
    public void InterpolatedString_ClassifiesDelimiters()
    {
        var source = """
class C {
    M(name: string) -> string {
        return "Hello ${name}";
    }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var result = SemanticClassifier.Classify(tree.GetRoot(), model);

        var tokens = tree.GetRoot().DescendantTokens().ToList();
        var startQuote = tokens.First(t => t.Kind == SyntaxKind.StringStartToken);
        var endQuote = tokens.First(t => t.Kind == SyntaxKind.StringEndToken);
        var dollar = tokens.First(t => t.Kind == SyntaxKind.DollarToken);
        var openBrace = tokens.First(t => t.Kind == SyntaxKind.OpenBraceToken && t.Parent is InterpolationSyntax);
        var closeBrace = tokens.First(t => t.Kind == SyntaxKind.CloseBraceToken && t.Parent is InterpolationSyntax);

        result.Tokens[startQuote].ShouldBe(SemanticClassification.StringLiteral);
        result.Tokens[endQuote].ShouldBe(SemanticClassification.StringLiteral);
        result.Tokens[dollar].ShouldBe(SemanticClassification.StringLiteral);
        result.Tokens[openBrace].ShouldBe(SemanticClassification.StringLiteral);
        result.Tokens[closeBrace].ShouldBe(SemanticClassification.StringLiteral);
    }
}
