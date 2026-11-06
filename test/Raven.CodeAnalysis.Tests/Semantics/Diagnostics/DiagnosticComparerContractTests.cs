using System.Collections.Immutable;

using Raven.CodeAnalysis.Text;
namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "Shared")]
public sealed class DiagnosticComparerContractTests
{
    [Fact]
    public void NullAndIdentityHaveStableOrdering()
    {
        var diagnostic = Create();
        Assert.Equal(0, DiagnosticComparer.Instance.Compare(null, null));
        Assert.Equal(0, DiagnosticComparer.Instance.Compare(diagnostic, diagnostic));
        Assert.True(DiagnosticComparer.Instance.Compare(null, diagnostic) < 0);
        Assert.True(DiagnosticComparer.Instance.Compare(diagnostic, null) > 0);
    }

    [Theory]
    [InlineData("path")]
    [InlineData("startLine")]
    [InlineData("startCharacter")]
    [InlineData("endLine")]
    [InlineData("endCharacter")]
    [InlineData("spanStart")]
    [InlineData("spanLength")]
    [InlineData("kind")]
    [InlineData("id")]
    [InlineData("severity")]
    [InlineData("message")]
    [InlineData("suppression")]
    [InlineData("propertyCount")]
    [InlineData("propertyKey")]
    [InlineData("propertyValue")]
    public void OrdersByEachTieBreaker(string field)
    {
        var left = Create(field, false);
        var right = Create(field, true);
        Assert.True(DiagnosticComparer.Instance.Compare(left, right) < 0, field);
        Assert.True(DiagnosticComparer.Instance.Compare(right, left) > 0, field);
    }

    [Fact]
    public void PathIsCaseInsensitiveAndPropertyEnumerationOrderDoesNotMatter()
    {
        var properties = ImmutableDictionary<string, string?>.Empty.Add("z", null).Add("a", "value");
        var reverse = ImmutableDictionary<string, string?>.Empty.Add("a", "value").Add("z", null);
        var descriptor = Descriptor("D1");
        var left = new Diagnostic(descriptor, Location.Create("FILE.RVN", new TextSpan(0, 0), default), ["message"], properties: properties);
        var right = new Diagnostic(descriptor, Location.Create("file.rvn", new TextSpan(0, 0), default), ["message"], properties: reverse);
        Assert.Equal(0, DiagnosticComparer.Instance.Compare(left, right));
    }

    [Fact]
    public void PropertyNullValuePrecedesNonNullValue()
    {
        var left = new Diagnostic(Descriptor("D1"), Location.None, ["message"], properties: ImmutableDictionary<string, string?>.Empty.Add("key", null));
        var right = new Diagnostic(Descriptor("D1"), Location.None, ["message"], properties: ImmutableDictionary<string, string?>.Empty.Add("key", ""));
        Assert.True(DiagnosticComparer.Instance.Compare(left, right) < 0);
    }

    [Fact]
    public void EarlierKeysDominateLaterKeysInSorting()
    {
        string[] keys = ["path", "startLine", "startCharacter", "endLine", "endCharacter", "spanStart", "spanLength", "kind", "id", "severity", "message", "suppression", "propertyCount"];
        var diagnostics = keys.Select(key => Create(key, true)).ToArray();
        var ordered = diagnostics.OrderBy(diagnostic => diagnostic, DiagnosticComparer.Instance).ToArray();
        Assert.Equal(diagnostics.Reverse(), ordered);
        Assert.Equal(0, DiagnosticComparer.Instance.Compare(Create(), Create()));
    }

    private static Diagnostic Create(string field = "", bool higher = false)
    {
        int Offset(string name) => higher && field == name ? 1 : 0;
        var location = new TestLocation(higher && field == "path" ? "b.rvn" : "a.rvn",
            new TextSpan(Offset("spanStart"), Offset("spanLength")),
            new LinePositionSpan(new LinePosition(Offset("startLine"), Offset("startCharacter")),
                new LinePosition(2 + Offset("endLine"), 2 + Offset("endCharacter"))),
            higher && field == "kind" ? LocationKind.ExternalFile : LocationKind.None);
        var properties = ImmutableDictionary<string, string?>.Empty;
        if (field == "propertyCount" && higher) properties = properties.Add("key", "value");
        if (field == "propertyKey") properties = properties.Add(higher ? "b" : "a", "value");
        if (field == "propertyValue") properties = properties.Add("key", higher ? "b" : "a");
        return new Diagnostic(Descriptor(higher && field == "id" ? "D2" : "D1"), location,
            [higher && field == "message" ? "b" : "a"],
            higher && field == "severity" ? DiagnosticSeverity.Error : DiagnosticSeverity.Warning,
            higher && field == "suppression", properties);
    }

    private static DiagnosticDescriptor Descriptor(string id) => DiagnosticDescriptor.Create(id, "title", null, "", "{0}", "test", DiagnosticSeverity.Warning);

    private sealed class TestLocation : Location
    {
        private readonly FileLinePositionSpan _lineSpan;
        public TestLocation(string path, TextSpan span, LinePositionSpan lines, LocationKind kind)
        {
            _lineSpan = new FileLinePositionSpan(path, lines.Start, lines.End);
            SourceSpan = span;
            Kind = kind;
        }
        public override FileLinePositionSpan GetLineSpan() => _lineSpan;
    }
}
