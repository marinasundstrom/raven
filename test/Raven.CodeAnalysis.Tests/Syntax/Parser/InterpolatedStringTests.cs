using System.Linq;
using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class InterpolatedStringTests
{
    [Fact]
    public void InterpolatedStringText_DecodesUnicodeEscapes()
    {
        var source = "let s = \"Start \\u{1F600} ${name} End\";";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();
        var interpolated = root.DescendantNodes().OfType<InterpolatedStringExpressionSyntax>().Single();

        var leadingText = Assert.IsType<InterpolatedStringTextSyntax>(interpolated.Contents[0]);
        Assert.Equal("Start \U0001F600 ", leadingText.Token.ValueText);
    }

    [Fact]
    public void InterpolatedStringText_TrailingSegmentDecodesUnicode()
    {
        var source = "let s = \"${value} \\u0041\";";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();
        var interpolated = root.DescendantNodes().OfType<InterpolatedStringExpressionSyntax>().Single();

        var trailingText = Assert.IsType<InterpolatedStringTextSyntax>(interpolated.Contents[^1]);
        Assert.Equal(" A", trailingText.Token.ValueText);
    }
}
