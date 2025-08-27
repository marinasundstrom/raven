using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;
using Raven.CodeAnalysis.Tests;
using Xunit;

namespace Raven.CodeAnalysis.Text.Tests;

public class ConsoleSyntaxHighlighterTests
{
    [Fact]
    public void UnderlinesDiagnosticSpans_WhenEnabled()
    {
        var source = """
import System.*
Console.WriteLine(Console.WriteLine());
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));
        var root = tree.GetRoot();

        var text = root.WriteNodeToText(compilation, includeDiagnostics: true);

        Assert.Contains("\u001b[91m", text);
        Assert.Contains("~", text);
    }
}
