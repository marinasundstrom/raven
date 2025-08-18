using Raven.CodeAnalysis.Text;
using Xunit;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class IncrementalSyntaxTreeUpdatesTest
{
    private static void AssertIncrementalParse(SourceText original, SourceText updated)
    {
        var originalTree = SyntaxTree.ParseText(original);
        var incrementalTree = originalTree.WithChangedText(updated);
        var expectedTree = SyntaxTree.ParseText(updated);

        var normalizedExpected = expectedTree.GetRoot().NormalizeWhitespace().ToFullString();
        var normalizedActual = incrementalTree.GetRoot().NormalizeWhitespace().ToFullString();

        Assert.Equal(normalizedExpected, normalizedActual);
    }

    [Fact]
    public void ApplyTextChangeToSyntaxTree()
    {
        var sourceText = SourceText.From(
            """
            if (foo)  {
                return 0;
            } else if (bar ) {
                return 1;
            }
            """);

        var textChange = new TextChange(
            new TextSpan(4, 3),
            "x"
        );

        var changedSourceText = sourceText.WithChange(textChange);

        AssertIncrementalParse(sourceText, changedSourceText);
    }

    [Fact]
    public void ApplyChangedTextToSyntaxTree()
    {
        var sourceText = SourceText.From(
            """
            if (foo)  {
                return 0;
            }
            """);

        var changedSourceText = SourceText.From(
            """
            if (foo)  {
                return 0;
            } else if (bar ) {
                return 1;
            }
            """);

        AssertIncrementalParse(sourceText, changedSourceText);
    }

    [Fact]
    public void ApplyChangedTextToSyntaxTree_Advanced()
    {
        var sourceText = SourceText.From(
            """
            {
                if (foo)  {
                    return 0;
                }
            }
            """);

        var changedSourceText = SourceText.From(
            """
            {
                if (foo)  {
                    return 0;
                } else if (bar ) {
                    return 1;
                }
            }
            """);

        AssertIncrementalParse(sourceText, changedSourceText);
    }

    [Fact]
    public void ApplyChangedTextToSyntaxTree3()
    {
        var sourceText = SourceText.From(
            """
            if (foo)  {
                return 0;
            }
            """);

        var changedSourceText = SourceText.From(
            """
            if (foo)  {
                return bar;
            }
            """);

        AssertIncrementalParse(sourceText, changedSourceText);
    }

    [Fact]
    public void ApplyChangedTextToSyntaxTree4()
    {
        var sourceText = SourceText.From(
            """
            if (foo)  {
                return 0;
            }
            """);

        var changedSourceText = SourceText.From(
            """
            if (foo)  {
                if(x) {}
            }
            """);

        AssertIncrementalParse(sourceText, changedSourceText);
    }

    [Fact]
    public void ApplyChangedTextToSyntaxTree5()
    {
        var sourceText = SourceText.From(
            """
            if (foo)  {
                if(x) {}
            }
            """);

        var changedSourceText = SourceText.From(
            """
            if (foo)  {
                if(x) {

                }
                else {}
            }
            """);

        AssertIncrementalParse(sourceText, changedSourceText);
    }
}

