using System;

using Raven.CodeAnalysis.Text;

using Xunit;
using Xunit.Abstractions;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class IncrementalSyntaxTreeUpdatesTest(ITestOutputHelper output)
{
    private static readonly PrinterOptions s_treeDumpOptions = new()
    {
        IncludeNames = true,
        IncludeTokens = true,
        IncludeTrivia = true,
        IncludeSpans = true,
        IncludeLocations = false,
        Colorize = false,
        ExpandListsAsProperties = true
    };

    private static void AssertIncrementalParse(SourceText original, SourceText updated)
    {
        var originalTree = SyntaxTree.ParseText(original);
        var incrementalTree = originalTree.WithChangedText(updated);
        var expectedTree = SyntaxTree.ParseText(updated);

        var normalizedExpected = expectedTree.GetRoot().NormalizeWhitespace().ToFullString();
        var normalizedActual = incrementalTree.GetRoot().NormalizeWhitespace().ToFullString();

        Assert.Equal(normalizedExpected, normalizedActual);
    }

    private void AssertIncrementalStepMatchesFullParse(SyntaxTree previousTree, SourceText updated, string label, out SyntaxTree incrementalTree)
    {
        incrementalTree = previousTree.WithChangedText(updated);
        var expectedTree = SyntaxTree.ParseText(updated, previousTree.Options, previousTree.FilePath);

        output.WriteLine($"==== {label} source ====");
        output.WriteLine(updated.ToString());
        output.WriteLine($"==== {label} incremental tree ====");
        output.WriteLine(incrementalTree.GetRoot().GetSyntaxTreeRepresentation(s_treeDumpOptions));

        Assert.Equal(updated.ToString(), incrementalTree.GetRoot().ToFullString());
        Assert.Equal(
            expectedTree.GetRoot().NormalizeWhitespace().ToFullString(),
            incrementalTree.GetRoot().NormalizeWhitespace().ToFullString());
    }

    [Fact]
    public void ChangedTextPolicy_UsesIncrementalParseForSmallSingleChange()
    {
        var original = SourceText.From("func Main() -> unit {}\n");
        var updated = original.Replace(original.Length - 1, 0, "// comment\n");
        var ranges = updated.GetChangeRanges(original);

        Assert.False(SyntaxTree.ShouldFullyReparseChangedText(original, updated, ranges));
    }

    [Fact]
    public void ChangedTextPolicy_UsesFullParseForWholeDocumentChange()
    {
        var original = SourceText.From("func Main() -> unit {}\n");
        var updated = original.Replace(new TextSpan(0, original.Length), "class C {}\n");
        var ranges = updated.GetChangeRanges(original);

        Assert.True(SyntaxTree.ShouldFullyReparseChangedText(original, updated, ranges));
    }

    [Fact]
    public void ChangedTextPolicy_UsesFullParseForLargeInsert()
    {
        var original = SourceText.From("func Main() -> unit {}\n");
        var insertedText = new string(' ', SyntaxTree.IncrementalParseMaxChangeLength + 1);
        var updated = original.Replace(original.Length - 1, 0, insertedText);
        var ranges = updated.GetChangeRanges(original);

        Assert.True(SyntaxTree.ShouldFullyReparseChangedText(original, updated, ranges));
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
    public void GetChanges_ReturnsChangesFromOldTreeToCurrentTree()
    {
        var sourceText = SourceText.From("val value = 1\n");
        var changedSourceText = sourceText.Replace(new TextSpan("val value = ".Length, 1), "42");
        var originalTree = SyntaxTree.ParseText(sourceText);
        var changedTree = originalTree.WithChangedText(changedSourceText);

        var change = Assert.Single(changedTree.GetChanges(originalTree));

        Assert.Equal(new TextSpan("val value = ".Length, 1), change.Span);
        Assert.Equal("42", change.NewText);
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

    [Fact]
    public void ApplyChangedTextToSyntaxTree6()
    {
        var sourceText = SourceText.From(
            """
            if (foo)  {
                return 0;
            } else if (bar ) {
                return 1;
            }
            """);

        var changedSourceText = SourceText.From(
            """
            if (foo)  {
                return 0;
            } else {
                return 1;
            }
            """);

        AssertIncrementalParse(sourceText, changedSourceText);
    }

    [Fact]
    public void ApplyChangedTextToSyntaxTree7()
    {
        var sourceText = SourceText.From(
            """
            try {
                return 0;
            } catch (Foo ex) {
                return 1;
            }
            """);

        var changedSourceText = SourceText.From(
            """
            try {
                return 0;
            } catch (Bar ex) {
                return 1;
            }
            """);

        AssertIncrementalParse(sourceText, changedSourceText);
    }

    [Fact]
    public void ApplyChangedTextToSyntaxTree_AddParameter()
    {
        var sourceText = SourceText.From(
            """
            fn add(a:int) {
                return a;
            }
            """);

        var changedSourceText = SourceText.From(
            """
            fn add(a:int, b:int) {
                return a + b;
            }
            """);

        AssertIncrementalParse(sourceText, changedSourceText);
    }

    [Fact]
    public void ApplyChangedTextToSyntaxTree_InsertAtStatementBoundaryInBlock()
    {
        var source = """
            fn compute() {
                return 1;
            }
            """;

        var sourceText = SourceText.From(source);
        var insertionPosition = source.IndexOf("return 1;", StringComparison.Ordinal) + "return 1;".Length;
        var changedSourceText = sourceText.WithChange(
            new TextChange(new TextSpan(insertionPosition, 0), "\n    return 2;"));

        AssertIncrementalParse(sourceText, changedSourceText);
    }

    [Fact]
    public void SequentialSameDocumentEdits_OutputActualTreeAfterEachIncrementalChange()
    {
        var text = SourceText.From(
            """
            func Main() -> unit {
                val first = 1
                val second = first + 1
                System.Console.WriteLine(second)
            }
            """);
        var tree = SyntaxTree.ParseText(text, path: "/tmp/live.rav");

        var firstEdit = text.Replace(
            text.ToString().IndexOf("first + 1", StringComparison.Ordinal),
            "first + 1".Length,
            "first + 2");
        AssertIncrementalStepMatchesFullParse(tree, firstEdit, "edit 1", out tree);

        var secondEdit = firstEdit.Replace(
            firstEdit.ToString().IndexOf("System.Console.WriteLine(second)", StringComparison.Ordinal),
            "System.Console.WriteLine(second)".Length,
            "System.Console.WriteLine(first)");
        AssertIncrementalStepMatchesFullParse(tree, secondEdit, "edit 2", out tree);

        var thirdEdit = secondEdit.Replace(
            secondEdit.ToString().IndexOf("val second = first + 2", StringComparison.Ordinal),
            "val second = first + 2".Length,
            "val second = first + 3\n    val third = second + first");
        AssertIncrementalStepMatchesFullParse(tree, thirdEdit, "edit 3", out tree);
    }
}
