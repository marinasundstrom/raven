using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using Xunit.Abstractions;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class IncrementalSyntaxTreeUpdatesTest(ITestOutputHelper testOutputHelper)
{
    [Fact]
    public void IncrementalParsing()
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

        testOutputHelper.WriteLine(newRoot.GetSyntaxTreeRepresentation(true));
    }

    [Fact]
    public void IncrementalParsing2()
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

        testOutputHelper.WriteLine(newRoot.GetSyntaxTreeRepresentation(true));
    }

    [Fact]
    public void IncrementalParsing3()
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

        testOutputHelper.WriteLine(oldRoot.GetSyntaxTreeRepresentation(includeTrivia: false));

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

        testOutputHelper.WriteLine(newRoot.GetSyntaxTreeRepresentation(includeTrivia: false));
    }

    [Fact]
    public void IncrementalParsing4()
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

        testOutputHelper.WriteLine(oldRoot.GetSyntaxTreeRepresentation(includeTrivia: false));

        var changedSourceText = SourceText.From(
            """
            if (foo)  {
                return bar;
            }
            """);

        var updatedTree = syntaxTree.WithChangedText(changedSourceText);

        var newRoot = updatedTree.GetRoot();

        testOutputHelper.WriteLine(newRoot.ToFullString());

        testOutputHelper.WriteLine(newRoot.GetSyntaxTreeRepresentation(includeTrivia: false));

        var nodes = SyntaxNodeCache.Cache;
    }

    [Fact]
    public void IncrementalParsing5()
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

        testOutputHelper.WriteLine(oldRoot.GetSyntaxTreeRepresentation(includeTrivia: false));

        var changedSourceText = SourceText.From(
            """
            if (foo)  {
                if(x) {}
            }
            """);

        var updatedTree = syntaxTree.WithChangedText(changedSourceText);

        var newRoot = updatedTree.GetRoot();

        testOutputHelper.WriteLine(newRoot.ToFullString());

        testOutputHelper.WriteLine(newRoot.GetSyntaxTreeRepresentation(includeTrivia: false));

        var nodes = SyntaxNodeCache.Cache;
    }

    [Fact]
    public void IncrementalParsing6()
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

        testOutputHelper.WriteLine(oldRoot.GetSyntaxTreeRepresentation(includeTrivia: false));

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

        testOutputHelper.WriteLine(newRoot.GetSyntaxTreeRepresentation(includeTrivia: true, includeLocation: true));

        var nodes = SyntaxNodeCache.Cache;
    }
}