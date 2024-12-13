using Raven.CodeAnalysis.Syntax;

using Xunit.Abstractions;

namespace Raven.CodeAnalysis.Parser.Tests;

public class IncrementalSyntaxTreeUpdatesTest(ITestOutputHelper testOutputHelper)
{
    [Fact]
    public void IncrementalCompile()
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
}