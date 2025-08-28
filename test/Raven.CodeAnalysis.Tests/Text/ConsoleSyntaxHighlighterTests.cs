using System.Text.RegularExpressions;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;
using Raven.CodeAnalysis.Tests;
using Xunit;

namespace Raven.CodeAnalysis.Text.Tests;

public class ConsoleSyntaxHighlighterTests
{
    [Fact]
    public void UnderlinesDiagnosticSpans_WhenEnabled()
    {
        var source = """
import System.*
Console.WriteLine(Console.WriteLine());
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));
        var root = tree.GetRoot();

        var text = root.WriteNodeToText(compilation, includeDiagnostics: true);

        Assert.Contains("\u001b[91m", text);
        Assert.Contains("~", text);
    }

    [Fact]
    public void InterpolatedString_WritesDelimiters()
    {
        var source = """
class Test {
    GetInfo(name: string) -> string {
        return "Hello ${name}";
    }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));
        var root = tree.GetRoot();

        var text = root.WriteNodeToText(compilation);

        Assert.Contains('$', text);
        Assert.Contains('"', text);
    }

    [Fact]
    public void FullyQualifiedNamespace_IsColorized()
    {
        var source = "import System.Collections.Generic.*";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));
        var root = tree.GetRoot();

        var text = root.WriteNodeToText(compilation);

        var matches = Regex.Matches(text, "\u001b\\[9[56]m");
        Assert.Equal(3, matches.Count);
    }

    [Fact]
    public void ImportDirective_ColorsNamespaceAndTypeDifferently()
    {
        var source = "import System.Console";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));
        var root = tree.GetRoot();

        var result = SemanticClassifier.Classify(root, compilation.GetSemanticModel(tree));
        var identifiers = root.DescendantTokens().Where(t => t.Kind == SyntaxKind.IdentifierToken).ToArray();

        Assert.Equal(SemanticClassification.Namespace, result.Tokens[identifiers[0]]);
        Assert.Equal(SemanticClassification.Type, result.Tokens[identifiers[1]]);
    }

    [Fact]
    public void AliasDirective_ColorsNamespaceAndType()
    {
        var source = "alias Print = System.Console";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));
        var root = tree.GetRoot();

        var result = SemanticClassifier.Classify(root, compilation.GetSemanticModel(tree));
        var identifiers = root.DescendantTokens().Where(t => t.Kind == SyntaxKind.IdentifierToken).ToArray();

        // identifiers: Print, System, Console
        Assert.Equal(SemanticClassification.Namespace, result.Tokens[identifiers[1]]);
        Assert.Equal(SemanticClassification.Type, result.Tokens[identifiers[2]]);
    }
}
