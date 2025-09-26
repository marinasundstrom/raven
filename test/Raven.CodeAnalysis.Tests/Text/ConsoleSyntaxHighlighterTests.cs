using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;
using Raven.CodeAnalysis.Text;

using Xunit;

namespace Raven.CodeAnalysis.Text.Tests;

public class ConsoleSyntaxHighlighterTests
{
    [Fact]
    public void UnderlinesDiagnosticSpans_WhenEnabled()
    {
        var source = """
import System.*

Console.WriteLine2("Foo")
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));
        var root = tree.GetRoot();

        var diagnostics = compilation.GetDiagnostics()
            .Where(d => d.Location.SourceTree == tree)
            .ToArray();

        Assert.NotEmpty(diagnostics);

        var text = root.WriteNodeToText(compilation, includeDiagnostics: true);

        Assert.Contains("\u001b[4:3m", text);
        Assert.Contains("\u001b[4:0m", text);
    }

    [Fact]
    public void DoesNotUnderline_WhenNoDiagnostics()
    {
        var source = """
import System.*

Console.WriteLine(Console.WriteLine())
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));
        var root = tree.GetRoot();

        var diagnostics = compilation.GetDiagnostics()
            .Where(d => d.Location.SourceTree == tree)
            .ToArray();

        Assert.Empty(diagnostics);

        var text = root.WriteNodeToText(compilation, includeDiagnostics: true);

        Assert.DoesNotContain("\u001b[4:3m", text);
        Assert.DoesNotContain("\u001b[4:0m", text);
    }

    [Fact]
    public void InterpolatedString_WritesDelimiters()
    {
        var source = """
class Test {
    GetInfo(name: string) -> string {
        return "Hello ${name}";
    }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));
        var root = tree.GetRoot();

        var text = root.WriteNodeToText(compilation);

        Assert.Contains('$', text);
        Assert.Contains('"', text);
    }

    [Fact]
    public void MethodInvocation_UsesMethodColor()
    {
        var source = """
import System.*

Console.WriteLine("Foo")
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));
        var root = tree.GetRoot();

        var originalScheme = ConsoleSyntaxHighlighter.ColorScheme;
        try
        {
            ConsoleSyntaxHighlighter.ColorScheme = ColorScheme.Dark;

            var text = root.WriteNodeToText(compilation);

            var methodColor = ConsoleSyntaxHighlighter.ColorScheme.Method;
            var methodAnsi = $"\u001b[{(int)methodColor}m";

            Assert.Contains($"{methodAnsi}WriteLine", text);
        }
        finally
        {
            ConsoleSyntaxHighlighter.ColorScheme = originalScheme;
        }
    }

    [Fact]
    public void MethodInvocation_WithDiagnostic_StillUsesMethodColor()
    {
        var source = """
import System.*

Console.WriteLine2("Foo")
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));
        var root = tree.GetRoot();

        var originalScheme = ConsoleSyntaxHighlighter.ColorScheme;
        try
        {
            ConsoleSyntaxHighlighter.ColorScheme = ColorScheme.Dark;

            var text = root.WriteNodeToText(compilation, includeDiagnostics: true);

            var methodColor = ConsoleSyntaxHighlighter.ColorScheme.Method;
            var methodAnsi = $"\u001b[{(int)methodColor}m";

            Assert.Contains($"{methodAnsi}WriteLine2", text);
        }
        finally
        {
            ConsoleSyntaxHighlighter.ColorScheme = originalScheme;
        }
    }

    [Fact]
    public void MethodInvocation_AfterInterpolatedString_UsesCorrectSpans()
    {
        var source = """
import System.*

let name = "Marina"
let city = "Ystad"
let welcome = "${name}\u200F مرحبا!  ١٥ ${city}"

System.Console.WriteLine(welcome)
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));
        var root = tree.GetRoot();

        var originalScheme = ConsoleSyntaxHighlighter.ColorScheme;
        try
        {
            ConsoleSyntaxHighlighter.ColorScheme = ColorScheme.Dark;

            var text = root.WriteNodeToText(compilation);

            var methodAnsi = $"\u001b[{(int)ConsoleSyntaxHighlighter.ColorScheme.Method}m";
            var localAnsi = $"\u001b[{(int)ConsoleSyntaxHighlighter.ColorScheme.Local}m";

            Assert.Contains($"{methodAnsi}WriteLine", text);
            Assert.Contains($"{localAnsi}welcome", text);
            Assert.DoesNotContain($"{methodAnsi}Write{localAnsi}Line", text);
        }
        finally
        {
            ConsoleSyntaxHighlighter.ColorScheme = originalScheme;
        }
    }
}
