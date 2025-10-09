using System;
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

    [Fact]
    public void InterpolatedStringText_PreservesLeftToRightUnicodeSegments()
    {
        var source = "let s = \"こんにちは ${name} 世界\";";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();
        var interpolated = root.DescendantNodes().OfType<InterpolatedStringExpressionSyntax>().Single();

        var leadingText = Assert.IsType<InterpolatedStringTextSyntax>(interpolated.Contents[0]);
        Assert.Equal("こんにちは ", leadingText.Token.ValueText);

        var trailingText = Assert.IsType<InterpolatedStringTextSyntax>(interpolated.Contents[^1]);
        Assert.Equal(" 世界", trailingText.Token.ValueText);
    }

    [Fact]
    public void InterpolatedStringText_PreservesRightToLeftUnicodeSegments()
    {
        var source = "let s = \"\u200Fمرحبا ${name} بالعالم\";";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();
        var interpolated = root.DescendantNodes().OfType<InterpolatedStringExpressionSyntax>().Single();

        var leadingText = Assert.IsType<InterpolatedStringTextSyntax>(interpolated.Contents[0]);
        Assert.Equal("\u200Fمرحبا ", leadingText.Token.ValueText);

        var trailingText = Assert.IsType<InterpolatedStringTextSyntax>(interpolated.Contents[^1]);
        Assert.Equal(" بالعالم", trailingText.Token.ValueText);
    }

    [Fact]
    public void InterpolatedStringText_AllowsEscapedQuotes()
    {
        var source = "let s = \"Saw \\\"${text}\\\"\";";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();
        var interpolated = root.DescendantNodes().OfType<InterpolatedStringExpressionSyntax>().Single();

        Assert.Collection(
            interpolated.Contents,
            first =>
            {
                var leading = Assert.IsType<InterpolatedStringTextSyntax>(first);
                Assert.Equal("Saw \"", leading.Token.ValueText);
            },
            second => Assert.IsType<InterpolationSyntax>(second),
            third =>
            {
                var trailing = Assert.IsType<InterpolatedStringTextSyntax>(third);
                Assert.Equal("\"", trailing.Token.ValueText);
            });
    }

    [Fact]
    public void InterpolatedStringText_AllowsQuotesInsideExpressions()
    {
        var source = "let s = \"Foo: ${\"Hej\"}\";";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();
        var interpolated = root.DescendantNodes().OfType<InterpolatedStringExpressionSyntax>().Single();

        Assert.Collection(
            interpolated.Contents,
            first =>
            {
                var text = Assert.IsType<InterpolatedStringTextSyntax>(first);
                Assert.Equal("Foo: ", text.Token.ValueText);
            },
            second =>
            {
                var interpolation = Assert.IsType<InterpolationSyntax>(second);
                var literal = Assert.IsType<LiteralExpressionSyntax>(interpolation.Expression);
                Assert.Equal("Hej", literal.Token.ValueText);
            });
    }

    [Fact]
    public void InterpolatedStringText_AllowsQuotesInsideExpressionsWithOperators()
    {
        var source = "let s = \"Foo: ${\"Hej\" + \" Bob\"}\";";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();
        var interpolated = root.DescendantNodes().OfType<InterpolatedStringExpressionSyntax>().Single();

        Assert.Collection(
            interpolated.Contents,
            first =>
            {
                var text = Assert.IsType<InterpolatedStringTextSyntax>(first);
                Assert.Equal("Foo: ", text.Token.ValueText);
            },
            second =>
            {
                var interpolation = Assert.IsType<InterpolationSyntax>(second);
                var binary = Assert.IsType<BinaryExpressionSyntax>(interpolation.Expression);

                var leftLiteral = Assert.IsType<LiteralExpressionSyntax>(binary.Left);
                Assert.Equal("Hej", leftLiteral.Token.ValueText);

                var rightLiteral = Assert.IsType<LiteralExpressionSyntax>(binary.Right);
                Assert.Equal(" Bob", rightLiteral.Token.ValueText);
            });
    }

    [Fact]
    public void InterpolatedStringText_AllowsEscapedSingleQuotesAndTabs()
    {
        var source = "let s = \"It\\'s ${text}\\'\\t\";";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();
        var interpolated = root.DescendantNodes().OfType<InterpolatedStringExpressionSyntax>().Single();

        Assert.Collection(
            interpolated.Contents,
            first =>
            {
                var leading = Assert.IsType<InterpolatedStringTextSyntax>(first);
                Assert.Equal("It's ", leading.Token.ValueText);
            },
            second => Assert.IsType<InterpolationSyntax>(second),
            third =>
            {
                var trailing = Assert.IsType<InterpolatedStringTextSyntax>(third);
                Assert.Equal("'" + "\t", trailing.Token.ValueText);
            });
    }

    [Fact]
    public void InterpolatedStringText_SpansAccountForEscapes()
    {
        var source = """
let text = "value"
let message = "Saw \"${text}\""
let result = describe(null)
""";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();
        var invocation = root.DescendantNodes().OfType<InvocationExpressionSyntax>().Single();

        var expectedStart = source.IndexOf("describe(null)", StringComparison.Ordinal);
        Assert.Equal(expectedStart, invocation.Span.Start);
        Assert.Equal("describe(null)".Length, invocation.Span.Length);
    }
}
