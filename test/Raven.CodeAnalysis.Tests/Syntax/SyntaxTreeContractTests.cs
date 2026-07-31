using System.Text;

using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Syntax.Tests;

public sealed class SyntaxTreeContractTests
{
    [Fact]
    public void ParsedTree_AlwaysExposesItsSourceTextAndRoot()
    {
        var sourceText = SourceText.From("func Main() {}", Encoding.UTF8);
        var tree = SyntaxTree.ParseText(sourceText);

        Assert.Same(sourceText, tree.GetText());
        Assert.True(tree.TryGetText(out var availableText));
        Assert.Same(sourceText, availableText);
        Assert.Same(tree, tree.GetRoot().SyntaxTree);
    }

    [Fact]
    public void CreatedTree_AlwaysExposesMaterializedSourceTextAndRoot()
    {
        var root = SyntaxTree.ParseText("func Main() {}").GetRoot();
        var tree = SyntaxTree.Create(root);

        Assert.Equal(root.ToFullString(), tree.GetText().ToString());
        Assert.Same(tree, tree.GetRoot().SyntaxTree);
    }
}
