using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class SemanticClassifierTests : CompilationTestBase
{
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

    [Fact]
    public void AliasDirective_ClassifiesQualifiedNameParts()
    {
        var source = "alias foo = System.Text.StringBuilder";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var result = SemanticClassifier.Classify(tree.GetRoot(), model);

        var tokens = tree.GetRoot().DescendantTokens().Where(t => t.Kind == SyntaxKind.IdentifierToken).ToList();
        var system = tokens.Single(t => t.Text == "System");
        var text = tokens.Single(t => t.Text == "Text");
        var sb = tokens.Single(t => t.Text == "StringBuilder");

        result.Tokens[system].ShouldBe(SemanticClassification.Namespace);
        result.Tokens[text].ShouldBe(SemanticClassification.Namespace);
        result.Tokens[sb].ShouldBe(SemanticClassification.Type);
    }
}
