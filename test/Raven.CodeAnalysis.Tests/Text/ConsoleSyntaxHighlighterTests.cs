using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;
using Raven.CodeAnalysis.Text;

using Xunit;

namespace Raven.CodeAnalysis.Text.Tests;

[CollectionDefinition("ConsoleSyntaxHighlighter serial", DisableParallelization = true)]
public sealed class ConsoleSyntaxHighlighterSerialCollection;

[Collection("ConsoleSyntaxHighlighter serial")]
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

        Assert.Contains("\u001b[4m", text);
        Assert.Contains("\u001b[24m", text);
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
    func GetInfo(name: string) -> string {
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
    func GetInfo(name: string) -> string {
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
    public void ExpressionBodiedFunction_DoesNotCrashHighlighter()
    {
        var source = """
import System.Console.*

func greet(name: string) -> string => $"Hello {name}"

WriteLine(greet("Raven"))
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));
        var root = tree.GetRoot();

        var exception = Record.Exception(() => root.WriteNodeToText(compilation));

        Assert.Null(exception);
    }

    [Fact]
    public void AsyncLambdaWithExpressionBody_DoesNotCrashHighlighter()
    {
        var source = """
import System.Console.*
import System.Threading.Tasks.*

val result = await Task.Run(async () => 42)
WriteLine($"Result: {result}")
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));
        var root = tree.GetRoot();

        var exception = Record.Exception(() => root.WriteNodeToText(compilation));

        Assert.Null(exception);
    }

    [Fact]
    public void InvalidStaticHelperWithGuardedIf_DoesNotCrashHighlighter()
    {
        var source = """
import System.*
import System.Linq.*

static class VehicleAppServices {
    static func GetConnectionString() -> string {
        val environment = Environment.GetEnvironmentVariable("ConnectionStrings__VehicleCosts")
        if environment is string when environment.Trim().Length > 0 {
            return environment
        }

        return "Host=localhost"
    }

    static func PredictCosts(vehicle: VehicleEntity) -> VehicleCostPredictionResponse {
        val samples = vehicle.FuelConsumptions
            .Where(entry => entry.DistanceDrivenKm > 0 && entry.LitersFilled > 0 && entry.TotalCost > 0)
            .OrderByDescending(entry => entry.RecordedAtUtc)
            .Take(6)
            .ToArray()

        return VehicleCostPredictionResponse()
    }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));
        var root = tree.GetRoot();

        var exception = Record.Exception(() => root.WriteNodeToText(compilation));

        Assert.Null(exception);
    }

    [Fact]
    public void InvalidUnionMatchSerialization_DoesNotCrashHighlighter()
    {
        var source = """
import System.Text.Json.*

union VehicleStatus {
    Operational(driverName: string, sinceUtc: int, currentOdometerKm: int)
    Maintenance(workshop: string, startedUtc: int, expectedReadyUtc: int, reason: string)
    Decommissioned(retiredUtc: int, reason: string)
}

static class VehicleStatusJson {
    static func EncodeStatus(status: VehicleStatus) -> string {
        return status match {
            .Operational(val driverName, val sinceUtc, val currentOdometerKm) =>
                JsonSerializer.Serialize(VehicleStatusDto("operational", driverName, sinceUtc, currentOdometerKm, null, null, null, null))
            .Maintenance(val workshop, val startedUtc, val expectedReadyUtc, val reason) =>
                JsonSerializer.Serialize(VehicleStatusDto("maintenance", null, startedUtc, null, workshop, expectedReadyUtc, reason, null))
            .Decommissioned(val retiredUtc, val reason) =>
                JsonSerializer.Serialize(VehicleStatusDto("decommissioned", null, null, null, null, null, reason, retiredUtc))
        }
    }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));
        var root = tree.GetRoot();

        var exception = Record.Exception(() => root.WriteNodeToText(compilation));

        Assert.Null(exception);
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
    public void DefaultVarianceAndConversionKeywords_UseKeywordColor()
    {
        var source = """
interface Producer<out T> {}
interface Consumer<in T> {}

class Box {
    static func implicit(value: Box) -> string => ""
    static func explicit(value: Box) -> int => 0
}

func Main() {
    val box: Box = default
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
            var keywordAnsi = $"\u001b[{(int)ConsoleSyntaxHighlighter.ColorScheme.Keyword}m";

            Assert.Contains($"{keywordAnsi}default", text);
            Assert.Contains($"{keywordAnsi}in", text);
            Assert.Contains($"{keywordAnsi}out", text);
            Assert.Contains($"{keywordAnsi}implicit", text);
            Assert.Contains($"{keywordAnsi}explicit", text);
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
    public void DiagnosticsOnly_WithMultipleDiagnosticsOnSameLine_RendersEachDiagnosticContextLine()
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

        Assert.All(
            renderedSourceLines,
            renderedLine => Assert.Contains("Missing1(); Missing2()", renderedLine.StripAnsiCodes(), StringComparison.Ordinal));
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

        var properties = SuggestionsDiagnosticProperties.CreateRewriteSuggestion(
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
    func Build(item: Widget) -> Widget {
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

    [Fact]
    public void WriteTextToTextLight_WithCasePattern_RendersCaseMemberText()
    {
        var source = """
union Result {
    Case(value: int)
}

func Render(result: Result) -> int {
    return match result {
        .Case(val value) => value
    }
}
""";

        var originalScheme = ConsoleSyntaxHighlighter.ColorScheme;
        try
        {
            ConsoleSyntaxHighlighter.ColorScheme = ColorScheme.Light;

            var text = ConsoleSyntaxHighlighter.WriteTextToTextLight(source);
            Assert.Contains(".Case(", text, StringComparison.Ordinal);
        }
        finally
        {
            ConsoleSyntaxHighlighter.ColorScheme = originalScheme;
        }
    }

    [Fact]
    public void WriteNodeToText_ColorizesUnqualifiedCaseIdentifierInMatchPattern()
    {
        var source = """
union Option<T> {
    Some(T)
    None
}

func Render(input: Option<int>) -> int {
    return input match {
        None => 0
        .Some(val value) => value
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));
        var root = tree.GetRoot();

        var originalScheme = ConsoleSyntaxHighlighter.ColorScheme;
        try
        {
            ConsoleSyntaxHighlighter.ColorScheme = ColorScheme.Light;

            var text = root.WriteNodeToText(compilation);
            var typeAnsi = $"\u001b[{(int)ConsoleSyntaxHighlighter.ColorScheme.Type}m";
            var methodAnsi = $"\u001b[{(int)ConsoleSyntaxHighlighter.ColorScheme.Method}m";

            Assert.Contains($"{typeAnsi}None", text);
            Assert.DoesNotContain($"{methodAnsi}None", text);
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
