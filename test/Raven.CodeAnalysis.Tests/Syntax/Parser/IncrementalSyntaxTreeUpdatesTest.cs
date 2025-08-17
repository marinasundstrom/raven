using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class IncrementalSyntaxTreeUpdatesTest(ITestOutputHelper testOutputHelper)
{
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

        var syntaxTree = SyntaxTree.ParseText(sourceText);

        var textChange = new TextChange(
            new TextSpan(4, 3), // Span of the text to replace
            "x"   // New text to insert
        );

        var changedSourceText = sourceText.WithChange(textChange);

        var updatedTree = syntaxTree.WithChangedText(changedSourceText);

        var newRoot = updatedTree.GetRoot();

        testOutputHelper.WriteLine(newRoot.ToFullString());

        testOutputHelper.WriteLine(newRoot.GetSyntaxTreeRepresentation(new PrinterOptions { IncludeNames = true, IncludeTokens = true, IncludeTrivia = true, IncludeSpans = false, IncludeLocations = true, Colorize = true, ExpandListsAsProperties = true }));
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

        var syntaxTree = SyntaxTree.ParseText(sourceText);

        var changedSourceText = SourceText.From(
            """
            if (foo)  {
                return 0;
            } else if (bar ) {
                return 1;
            }
            """);

        var updatedTree = syntaxTree.WithChangedText(changedSourceText);

        var newRoot = updatedTree.GetRoot();

        testOutputHelper.WriteLine(newRoot.ToFullString());

        testOutputHelper.WriteLine(newRoot.GetSyntaxTreeRepresentation(new PrinterOptions { IncludeNames = true, IncludeTokens = true, IncludeTrivia = true, IncludeSpans = false, IncludeLocations = true, Colorize = true, ExpandListsAsProperties = true }));
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

        var syntaxTree = SyntaxTree.ParseText(sourceText);

        var oldRoot = syntaxTree.GetRoot();

        testOutputHelper.WriteLine(oldRoot.ToFullString());

        testOutputHelper.WriteLine(oldRoot.GetSyntaxTreeRepresentation(new PrinterOptions { IncludeNames = true, IncludeTokens = true, IncludeTrivia = true }));

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

        var updatedTree = syntaxTree.WithChangedText(changedSourceText);

        var newRoot = updatedTree.GetRoot();

        testOutputHelper.WriteLine(newRoot.ToFullString());

        testOutputHelper.WriteLine(newRoot.GetSyntaxTreeRepresentation(new PrinterOptions { IncludeNames = true, IncludeTokens = true, IncludeTrivia = true }));
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

        var syntaxTree = SyntaxTree.ParseText(sourceText);

        var oldRoot = syntaxTree.GetRoot();

        testOutputHelper.WriteLine(oldRoot.ToFullString());

        testOutputHelper.WriteLine(oldRoot.GetSyntaxTreeRepresentation(new PrinterOptions { IncludeNames = true, IncludeTokens = true, IncludeTrivia = true }));

        var changedSourceText = SourceText.From(
            """
            if (foo)  {
                return bar;
            }
            """);

        var updatedTree = syntaxTree.WithChangedText(changedSourceText);

        var newRoot = updatedTree.GetRoot();

        testOutputHelper.WriteLine(newRoot.ToFullString());

        testOutputHelper.WriteLine(newRoot.GetSyntaxTreeRepresentation(new PrinterOptions { IncludeNames = true, IncludeTokens = true, IncludeTrivia = true }));
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

        var syntaxTree = SyntaxTree.ParseText(sourceText);

        var oldRoot = syntaxTree.GetRoot();

        testOutputHelper.WriteLine(oldRoot.ToFullString());

        testOutputHelper.WriteLine(oldRoot.GetSyntaxTreeRepresentation(new PrinterOptions { IncludeNames = true, IncludeTokens = true, IncludeTrivia = true }));

        var changedSourceText = SourceText.From(
            """
            if (foo)  {
                if(x) {}
            }
            """);

        var updatedTree = syntaxTree.WithChangedText(changedSourceText);

        var newRoot = updatedTree.GetRoot();

        testOutputHelper.WriteLine(newRoot.ToFullString());

        testOutputHelper.WriteLine(newRoot.GetSyntaxTreeRepresentation(new PrinterOptions { IncludeNames = true, IncludeTokens = true, IncludeTrivia = true }));
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

        var syntaxTree = SyntaxTree.ParseText(sourceText);

        var oldRoot = syntaxTree.GetRoot();

        testOutputHelper.WriteLine(oldRoot.ToFullString());

        testOutputHelper.WriteLine(oldRoot.GetSyntaxTreeRepresentation(new PrinterOptions { IncludeNames = true, IncludeTokens = true, IncludeTrivia = true }));

        var changedSourceText = SourceText.From(
            """
            if (foo)  {
                if(x) {
                
                } 
                else {}
            }
            """);

        var updatedTree = syntaxTree.WithChangedText(changedSourceText);

        var newRoot = updatedTree.GetRoot();

        testOutputHelper.WriteLine(newRoot.ToFullString());

        testOutputHelper.WriteLine(newRoot.GetSyntaxTreeRepresentation(new PrinterOptions { IncludeNames = true, IncludeTokens = true, IncludeTrivia = true, IncludeSpans = false, IncludeLocations = true, Colorize = true, ExpandListsAsProperties = true }));
    }
}