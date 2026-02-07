using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Diagnostics;
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
    public void InterpolatedString_UsesDistinctInterpolationColor()
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

        var originalScheme = ConsoleSyntaxHighlighter.ColorScheme;
        try
        {
            ConsoleSyntaxHighlighter.ColorScheme = ColorScheme.Dark;

            var text = root.WriteNodeToText(compilation);

            var interpolationAnsi = $"\u001b[{(int)ConsoleSyntaxHighlighter.ColorScheme.Interpolation}m";
            var stringAnsi = $"\u001b[{(int)ConsoleSyntaxHighlighter.ColorScheme.StringLiteral}m";

            Assert.Contains(interpolationAnsi, text);
            Assert.Contains(stringAnsi, text);
            Assert.Contains($"{interpolationAnsi}$", text);
            Assert.DoesNotContain($"{stringAnsi}$", text);
        }
        finally
        {
            ConsoleSyntaxHighlighter.ColorScheme = originalScheme;
        }
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

    [Fact]
    public void DiagnosticsOnly_ReturnsOnlyDiagnosticLines()
    {
        var source = """
import System.*
import System.Linq.*

Console.WriteLine2("Foo")
Console.WriteLine("Bar")
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));
        var root = tree.GetRoot();

        var text = root.WriteNodeToText(compilation, includeDiagnostics: true, diagnosticsOnly: true);
        var plain = text.StripAnsiCodes();

        Assert.Contains("file(", plain);
        Assert.Contains(": error ", plain);
        Assert.Contains("Console.WriteLine2(\"Foo\")", plain);
        Assert.DoesNotContain("import System.*", plain);
        Assert.DoesNotContain("Console.WriteLine(\"Bar\")", plain);
        Assert.DoesNotContain("Console.WriteLine(\"Foo\")", plain);
    }

    [Fact]
    public void DiagnosticsOnly_NoDiagnostics_ReturnsEmptyString()
    {
        var source = """
import System.*

Console.WriteLine("Foo")
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));
        var root = tree.GetRoot();

        var text = root.WriteNodeToText(compilation, includeDiagnostics: true, diagnosticsOnly: true);

        Assert.Equal(string.Empty, text);
    }

    [Fact]
    public void DiagnosticsOnly_WithMultipleDiagnosticsOnSameLine_UnderlinesOnlyCurrentDiagnostic()
    {
        var source = """
import System.*

Missing1(); Missing2()
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));
        var root = tree.GetRoot();

        var text = root.WriteNodeToText(compilation, includeDiagnostics: true, diagnosticsOnly: true);
        var renderedSourceLines = text.Split('\n')
            .Where(line => line.StripAnsiCodes().Contains("Missing1(); Missing2()", StringComparison.Ordinal))
            .ToArray();

        Assert.Equal(2, renderedSourceLines.Length);

        foreach (var renderedLine in renderedSourceLines)
        {
            Assert.Equal(1, CountOccurrences(renderedLine, "\u001b[4:3m"));
            Assert.Equal(1, CountOccurrences(renderedLine, "\u001b[4:0m"));
        }
    }

    [Fact]
    public void WriteTextToTextLight_HighlightsByTokenKind()
    {
        var originalScheme = ConsoleSyntaxHighlighter.ColorScheme;
        try
        {
            ConsoleSyntaxHighlighter.ColorScheme = ColorScheme.Light;

            var text = ConsoleSyntaxHighlighter.WriteTextToTextLight("let value = \"hello\" // c");
            var keywordAnsi = $"\u001b[{(int)ConsoleSyntaxHighlighter.ColorScheme.Keyword}m";
            var stringAnsi = $"\u001b[{(int)ConsoleSyntaxHighlighter.ColorScheme.StringLiteral}m";
            var commentAnsi = $"\u001b[{(int)ConsoleSyntaxHighlighter.ColorScheme.Comment}m";

            Assert.Contains($"{keywordAnsi}let", text);
            Assert.Contains($"{stringAnsi}\"hello\"", text);
            Assert.Contains($"{commentAnsi}// c", text);
        }
        finally
        {
            ConsoleSyntaxHighlighter.ColorScheme = originalScheme;
        }
    }

    [Fact]
    public void DiagnosticsOnly_WithSuggestions_IncludesYouWroteAndWriteThisWithHighlighting()
    {
        var source = """
let value = 1
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));
        var root = tree.GetRoot();
        var location = tree.GetLocation(new TextSpan(0, 3));

        var descriptor = DiagnosticDescriptor.Create(
            "RAV9999",
            "Test",
            "Test",
            helpLinkUri: string.Empty,
            "Test message",
            "Testing",
            DiagnosticSeverity.Warning);

        var properties = EducationalDiagnosticProperties.CreateRewriteSuggestion(
            originalCode: "let value = 1",
            rewrittenCode: "val value = 1");
        var diagnostic = Diagnostic.Create(descriptor, location, DiagnosticSeverity.Warning, properties);

        var originalScheme = ConsoleSyntaxHighlighter.ColorScheme;
        try
        {
            ConsoleSyntaxHighlighter.ColorScheme = ColorScheme.Light;

            var text = root.WriteNodeToText(
                compilation,
                includeDiagnostics: true,
                diagnosticsOnly: true,
                diagnostics: [diagnostic],
                includeSuggestions: true);

            var plain = text.StripAnsiCodes();
            var keywordAnsi = $"\u001b[{(int)ConsoleSyntaxHighlighter.ColorScheme.Keyword}m";

            Assert.Contains("You wrote:", plain);
            Assert.Contains("Write this instead:", plain);
            Assert.Contains("let value = 1", plain);
            Assert.Contains("val value = 1", plain);
            Assert.Contains($"{keywordAnsi}let", text);
            Assert.Contains($"{keywordAnsi}val", text);
        }
        finally
        {
            ConsoleSyntaxHighlighter.ColorScheme = originalScheme;
        }
    }

    [Fact]
    public void WriteTextToTextLight_AssumesIdentifierRolesFromSyntax()
    {
        var source = """
class Widget {
    Build(item: Widget) -> Widget {
        let localValue = item
        return Build(localValue)
    }
}
""";

        var originalScheme = ConsoleSyntaxHighlighter.ColorScheme;
        try
        {
            ConsoleSyntaxHighlighter.ColorScheme = ColorScheme.Light;

            var text = ConsoleSyntaxHighlighter.WriteTextToTextLight(source);
            var methodAnsi = $"\u001b[{(int)ConsoleSyntaxHighlighter.ColorScheme.Method}m";
            var parameterAnsi = $"\u001b[{(int)ConsoleSyntaxHighlighter.ColorScheme.Parameter}m";
            var localAnsi = $"\u001b[{(int)ConsoleSyntaxHighlighter.ColorScheme.Local}m";
            var typeAnsi = $"\u001b[{(int)ConsoleSyntaxHighlighter.ColorScheme.Type}m";

            Assert.Contains($"{methodAnsi}Build", text);
            Assert.Contains($"{parameterAnsi}item", text);
            Assert.Contains($"{localAnsi}localValue", text);
            Assert.Contains($"{typeAnsi}Widget", text);
        }
        finally
        {
            ConsoleSyntaxHighlighter.ColorScheme = originalScheme;
        }
    }

    private static int CountOccurrences(string source, string value)
    {
        if (string.IsNullOrEmpty(source) || string.IsNullOrEmpty(value))
            return 0;

        var count = 0;
        var index = 0;
        while ((index = source.IndexOf(value, index, StringComparison.Ordinal)) >= 0)
        {
            count++;
            index += value.Length;
        }

        return count;
    }
}
