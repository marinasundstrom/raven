using System.Linq;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Completion;

public class CompletionContextTests
{
    [Theory]
    [InlineData("// Console.|")]
    [InlineData("// |comment")]
    [InlineData("Console.WriteLine() // |comment")]
    [InlineData("/* Console.| */")]
    [InlineData("/* Console.|")]
    [InlineData("let text = \"Console.|\"")]
    [InlineData("let text = \"Con|sole\"")]
    [InlineData("let text = \"Console.|")]
    [InlineData("let text = \"|")]
    [InlineData("let text = \"hello $name Console.|\"")]
    [InlineData("let ch = 'C|'")]
    [InlineData("let text = \"\"\"\nConsole.|\n\"\"\"")]
    [InlineData("let text = \"\"\"|")]
    public void GetCompletions_InsideCommentsAndLiteralText_ReturnsNoItems(string markedCode)
    {
        var (compilation, tree, position) = CreateContext(markedCode);
        var result = new CompletionService().GetCompletionsWithMetrics(compilation, tree, position);

        Assert.False(result.UsedFallback, result.FailureType);
        Assert.Empty(result.Items);
    }

    [Theory]
    [InlineData("Con|", "Console")]
    [InlineData("Console.|", "WriteLine")]
    [InlineData("let text = \"hello\"\ntext.|", "Length")]
    [InlineData("let text: string? = \"hello\"\ntext?.|", "Length")]
    [InlineData("let text = \"hello ${Console.|}\"", "WriteLine")]
    [InlineData("/* comment */ Con|", "Console")]
    [InlineData("// comment\nCon|", "Console")]
    public void GetCompletions_InCode_ReturnsRelevantItems(string markedCode, string label)
    {
        var (compilation, tree, position) = CreateContext(markedCode);
        var result = new CompletionService().GetCompletionsWithMetrics(compilation, tree, position);

        Assert.False(result.UsedFallback, result.FailureType);
        Assert.Contains(result.Items, item => item.DisplayText == label);
    }

    private static (Compilation compilation, SyntaxTree tree, int position) CreateContext(string markedCode)
    {
        var markedSource = "import System.*\n" + markedCode;
        var position = markedSource.IndexOf('|');
        var tree = SyntaxTree.ParseText(markedSource.Remove(position, 1));
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);
        return (compilation, tree, position);
    }
}
