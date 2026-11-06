using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;
namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "Shared")]
public sealed class SuppressionMapContractTests
{
    [Theory]
    [InlineData("// pragma warning disable A, B", "B", true)]
    [InlineData("// RaVeN PrAgMa WaRnInG DiSaBlE a, b;", "B", true)]
    [InlineData("/*\r\n * pragma warning disable A\r\n */", "A", true)]
    [InlineData("/* pragma warning disable A */ /* pragma warning restore A */", "A", false)]
    [InlineData("/* pragma warning restore A */ /* pragma warning disable A */", "A", true)]
    [InlineData("#pragma warning disable *", "other", true)]
    [InlineData("// ordinary comment", "other", false)]
    public void ReadsDirectivesInTriviaOrder(string prefix, string id, bool expected)
    {
        var tree = SyntaxTree.ParseText(prefix + "\nlet value = 1");
        var map = SourceDiagnosticSuppressionMap.Create(tree);
        var location = Location.Create(tree, new TextSpan(prefix.Length + 1, 3));
        Assert.Equal(expected, map.IsSuppressed(Create(id, DiagnosticSeverity.Warning, location)));
    }

    [Theory]
    [InlineData("A", DiagnosticSeverity.Error)]
    [InlineData("RAV0162", DiagnosticSeverity.Warning)]
    public void ExplicitlyProtectedDiagnosticsRemainVisible(string id, DiagnosticSeverity severity)
    {
        const string source = "#pragma warning disable *\nlet value = 1";
        var tree = SyntaxTree.ParseText(source);
        var map = SourceDiagnosticSuppressionMap.Create(tree);
        Assert.False(map.IsSuppressed(Create(id, severity, Location.Create(tree, new TextSpan(source.IndexOf("value"), 5)))));
        Assert.False(map.IsSuppressed(Create("A", DiagnosticSeverity.Warning, Location.None)));
    }

    private static Diagnostic Create(string id, DiagnosticSeverity severity, Location location)
        => Diagnostic.Create(DiagnosticDescriptor.Create(id, "title", null, "", "message", "test", severity), location);

    [Theory]
    [InlineData("disable A", "A", "A", true, true)]
    [InlineData("disable A", "*", "A", true, true)]
    [InlineData("disable", "A", "A", true, true)]
    [InlineData("disable", "*", "B", true, true)]
    [InlineData("disable\n#pragma warning restore A", "*", "B", true, true)]
    [InlineData("disable\n#pragma warning restore A", "*", "A", true, false)]
    [InlineData("disable A", "B", "B", true, false)]
    [InlineData("disable A", "B", "C", false, false)]
    [InlineData("disable A", "b", "B", true, false)]
    public void NextLineOverlaysPersistentState(string earlier, string nextIds, string id, bool first, bool second)
    {
        var source = $"#pragma warning {earlier}\n#pragma warning disable-next-line {nextIds}\nlet first = 1\nlet second = 2";
        var tree = SyntaxTree.ParseText(source);
        var map = SourceDiagnosticSuppressionMap.Create(tree);
        Assert.Equal(first, map.IsSuppressed(At("first")));
        Assert.Equal(second, map.IsSuppressed(At("second")));
        Diagnostic At(string marker) => Diagnostic.Create(
            DiagnosticDescriptor.Create(id, "title", null, "", "message", "test", DiagnosticSeverity.Warning),
            Location.Create(tree, new TextSpan(source.IndexOf(marker, StringComparison.Ordinal), marker.Length)));
    }
}
