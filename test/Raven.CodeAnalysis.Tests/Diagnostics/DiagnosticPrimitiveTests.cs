using Raven.CodeAnalysis.Syntax.InternalSyntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

[Trait("Target", "Shared")]
public class DiagnosticPrimitiveTests
{
    [Theory]
    [InlineData("")]
    [InlineData("Expected {0}: λ")]
    public void LiteralStringPreservesText(string text)
    {
        LocalizableString value = text;
        Assert.Equal(text, value.ToString());
        string converted = value;
        Assert.Equal(text, converted);
    }

    [Fact]
    public void DescriptorRetainsFieldsAndDefaults()
    {
        LocalizableString title = "Title";
        LocalizableString message = "Expected {0}";
        var descriptor = DiagnosticDescriptor.Create("TEST001", title, null, "https://example.test/help", message, "Syntax", DiagnosticSeverity.Error);
        Assert.Equal("TEST001", descriptor.Id);
        Assert.Same(title, descriptor.Title);
        Assert.Null(descriptor.Description);
        Assert.Equal("https://example.test/help", descriptor.HelpLinkUri);
        Assert.Same(message, descriptor.MessageFormat);
        Assert.Equal("Syntax", descriptor.Category);
        Assert.Equal(DiagnosticSeverity.Error, descriptor.DefaultSeverity);
        Assert.True(descriptor.IsEnabledByDefault);
        Assert.Empty(typeof(DiagnosticDescriptor).GetConstructors());
    }

    [Fact]
    public void DescriptorEqualityUsesIdOnly()
    {
        var first = Create("TEST001");
        var second = DiagnosticDescriptor.Create("TEST001", "Different", "Description", "", "Other", "Other", DiagnosticSeverity.Hidden, false);
        Assert.Equal(first, second);
        Assert.True(((IEquatable<DiagnosticDescriptor>)first).Equals(second));
        Assert.True(first == second);
        Assert.False(first != second);
        Assert.True(first != Create("TEST002"));
        Assert.False(first == null);
        Assert.False(null == first);
        Assert.True(first != null);
        Assert.True(null != first);
        DiagnosticDescriptor? missing = null;
        Assert.True(missing == null);
        Assert.False(missing != null);
        Assert.Equal(first.GetHashCode(), second.GetHashCode());
        Assert.Single(new HashSet<DiagnosticDescriptor> { first, second });
        Assert.NotEqual(first, Create("TEST002"));
        Assert.False(first.Equals(null));
        Assert.False(first.Equals("TEST001"));
        Assert.False(second.IsEnabledByDefault);
        Assert.Equal("Description", second.Description!.ToString());
    }

    [Fact]
    public void DiagnosticInfoRetainsArgumentsAndSpan()
    {
        var descriptor = Create("TEST001");
        object?[] args = ["name", 42, null];
        var info = DiagnosticInfo.Create(descriptor, new TextSpan(3, 5), args);
        Assert.Same(descriptor, info.Descriptor);
        Assert.Equal(new TextSpan(3, 5), info.Span);
        Assert.Same(args, info.Args);
        Assert.Equal(new object?[] { "name", 42, null }, info.Args);
        Assert.Empty(DiagnosticInfo.Create(descriptor, default).Args);
        Assert.Equal(new object?[] { "name", 42 }, DiagnosticInfo.Create(descriptor, default, "name", 42).Args);
    }

    [Fact]
    public void SeverityValuesRemainStable()
    {
        Assert.Equal(new[] { "Hidden", "Info", "Warning", "Error" }, Enum.GetNames<DiagnosticSeverity>());
        Assert.Equal(new[] { 0, 1, 2, 3 }, Enum.GetValues<DiagnosticSeverity>().Select(value => (int)value));
    }

    private static DiagnosticDescriptor Create(string id) =>
        DiagnosticDescriptor.Create(id, "Title", null, "", "Message", "Syntax", DiagnosticSeverity.Warning);
}
