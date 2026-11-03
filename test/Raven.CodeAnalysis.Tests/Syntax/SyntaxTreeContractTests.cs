using System.Text;

using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Syntax.Tests;

public sealed class SyntaxTreeContractTests
{
    [Fact]
    public void ZeroWidthEndOfFileDiagnosticsRemainVisibleThroughSpanQueries()
    {
        var tree = SyntaxTree.ParseText("class C {");
        var root = tree.GetRoot();
        var atEnd = tree.GetDiagnostics().Where(d => d.Location.SourceSpan.Start == tree.Length &&
            d.Location.SourceSpan.Length == 0).ToArray();
        Assert.NotEmpty(atEnd);
        Assert.All(atEnd, diagnostic =>
        {
            Assert.Contains(diagnostic, root.GetDiagnostics());
            Assert.Contains(diagnostic, tree.GetDiagnostics(root.FullSpan));
            Assert.Contains(diagnostic, tree.GetDiagnostics(root.EndOfFileToken));
            Assert.DoesNotContain(diagnostic, tree.GetDiagnostics(new TextSpan(0, 1)));
        });
    }

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
