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
    func M(name: string) -> string {
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
        result.Tokens[dollar].ShouldBe(SemanticClassification.Interpolation);
        result.Tokens[openBrace].ShouldBe(SemanticClassification.Interpolation);
        result.Tokens[closeBrace].ShouldBe(SemanticClassification.Interpolation);
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

        result.Tokens[system].ShouldBe(SemanticClassification.Type);
        result.Tokens[text].ShouldBe(SemanticClassification.Type);
        result.Tokens[sb].ShouldBe(SemanticClassification.Type);
    }

    [Fact]
    public void LabeledStatementTokens_ClassifiedAsLabel()
    {
        var source = """
func Main() {
label:
    goto label
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var result = SemanticClassifier.Classify(tree.GetRoot(), model);

        var labels = tree.GetRoot().DescendantTokens()
            .Where(t => t.Kind == SyntaxKind.IdentifierToken && t.Text == "label")
            .ToList();

        Assert.Equal(2, labels.Count);
        result.Tokens[labels[0]].ShouldBe(SemanticClassification.Label);
        result.Tokens[labels[1]].ShouldBe(SemanticClassification.Label);
    }

    [Fact]
    public void MatchCasePattern_ClassifiesCaseIdentifierAsType()
    {
        var source = """
	union Result {
	    Case(value: int)
	}

	func Render(result: Result) -> int {
	    return result match {
	        .Case(val value) => value
	    }
	}
	""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var result = SemanticClassifier.Classify(tree.GetRoot(), model);

        var caseToken = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberPatternPathSyntax>()
            .Select(path => path.Identifier)
            .Single(token => token.Text == "Case");

        result.Tokens[caseToken].ShouldBe(SemanticClassification.Type);
    }

    [Fact]
    public void MatchCasePattern_UnqualifiedCaseIdentifier_ClassifiesAsType()
    {
        var source = """
union Option<T> {
    Some(T)
    None
}

func Render(input: Option<int>) -> int {
    return input match {
        None => 0
        .Some(val value) => value
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var result = SemanticClassifier.Classify(tree.GetRoot(), model);

        var noneToken = tree.GetRoot()
            .DescendantTokens()
            .Single(token => token.Kind == SyntaxKind.IdentifierToken
                             && token.Text == "None"
                             && token.Parent is not null
                             && token.Parent.AncestorsAndSelf().Any(node => node.Kind == SyntaxKind.MatchArm));

        result.Tokens[noneToken].ShouldBe(SemanticClassification.Type);
    }

    [Fact]
    public void ReturnExpressionKeyword_IsClassifiedAsKeyword()
    {
        var source = """
func M(name: string?) -> string {
    val value = name ?? return "fallback"
    return value
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var result = SemanticClassifier.Classify(tree.GetRoot(), model);

        var returnExpressionKeyword = tree.GetRoot()
            .DescendantNodes()
            .OfType<ReturnExpressionSyntax>()
            .Select(n => n.ReturnKeyword)
            .Single();

        result.Tokens[returnExpressionKeyword].ShouldBe(SemanticClassification.Keyword);
    }

    [Fact]
    public void ThrowExpressionKeyword_IsClassifiedAsKeyword()
    {
        var source = """
import System.*

func M(name: string?) -> string {
    return name ?? throw InvalidOperationException("missing")
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var result = SemanticClassifier.Classify(tree.GetRoot(), model);

        var throwExpressionKeyword = tree.GetRoot()
            .DescendantNodes()
            .OfType<ThrowExpressionSyntax>()
            .Select(n => n.ThrowKeyword)
            .Single();

        result.Tokens[throwExpressionKeyword].ShouldBe(SemanticClassification.Keyword);
    }

    [Fact]
    public void DefaultVarianceAndConversionKeywords_AreClassifiedAsKeywords()
    {
        var source = """
interface Producer<out T> {}
interface Consumer<in T> {}

class Box {
    static func implicit(value: Box) -> string => ""
    static func explicit(value: Box) -> int => 0
}

func Main() {
    val box: Box = default
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var result = SemanticClassifier.Classify(tree.GetRoot(), model);

        var root = tree.GetRoot();
        var defaultKeyword = root.DescendantNodes()
            .OfType<DefaultExpressionSyntax>()
            .Select(n => n.DefaultKeyword)
            .Single();
        var varianceKeywords = root.DescendantNodes()
            .OfType<TypeParameterSyntax>()
            .Select(n => n.VarianceKeyword)
            .Where(t => t.Kind is SyntaxKind.InKeyword or SyntaxKind.OutKeyword)
            .ToArray();
        var conversionKeywords = root.DescendantNodes()
            .OfType<ConversionOperatorDeclarationSyntax>()
            .Select(n => n.ConversionKindKeyword)
            .ToArray();

        result.Tokens[defaultKeyword].ShouldBe(SemanticClassification.Keyword);
        Assert.Equal(2, varianceKeywords.Length);
        Assert.All(varianceKeywords, keyword => result.Tokens[keyword].ShouldBe(SemanticClassification.Keyword));
        Assert.Equal(2, conversionKeywords.Length);
        Assert.All(conversionKeywords, keyword => result.Tokens[keyword].ShouldBe(SemanticClassification.Keyword));
    }
}
