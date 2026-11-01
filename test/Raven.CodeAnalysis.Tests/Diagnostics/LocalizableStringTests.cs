using System.Globalization;
using System.Resources;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

[Trait("Target", "DotNet")]
public sealed class LocalizableStringTests
{
    [Fact]
    public void LiteralEqualityUsesTextAndSupportsNulls()
    {
        LocalizableString first = "hello";
        LocalizableString second = "hello";
        LocalizableString different = "other";
        Assert.Equal(first, second);
        Assert.True(((IEquatable<LocalizableString>)first).Equals(second));
        Assert.True(first == second);
        Assert.False(first != second);
        Assert.True(first != different);
        Assert.Equal(first.GetHashCode(), second.GetHashCode());
        Assert.Single(new HashSet<LocalizableString> { first, second });
        Assert.False(first == null);
        Assert.False(null == first);
        Assert.True(first != null);
        LocalizableString? missing = null;
        Assert.True(missing == null);
        Assert.False(missing != null);
    }

    [Fact]
    public void ResourceTextUsesCurrentUICultureWithoutChangingIdentity()
    {
        var manager = new TestResources();
        var first = new LocalizableResourceString("Greeting", manager, typeof(LocalizableStringTests));
        var second = new LocalizableResourceString("Greeting", manager, typeof(LocalizableStringTests));
        var previous = CultureInfo.CurrentUICulture;
        try
        {
            CultureInfo.CurrentUICulture = CultureInfo.GetCultureInfo("en-US");
            Assert.Equal("Hello", first.ToString());
            var hash = first.GetHashCode();
            CultureInfo.CurrentUICulture = CultureInfo.GetCultureInfo("sv-SE");
            string text = first;
            Assert.Equal("Hej", text);
            Assert.Equal(hash, first.GetHashCode());
            Assert.Equal(first, second);
            Assert.True(first == second);
            Assert.False(first != second);
            Assert.Equal(first.GetHashCode(), second.GetHashCode());
            Assert.NotEqual(first, new LocalizableResourceString("Other", manager, typeof(LocalizableStringTests)));
            Assert.NotEqual(first, new LocalizableResourceString("Greeting", new TestResources(), typeof(LocalizableStringTests)));
            Assert.NotEqual(first, new LocalizableResourceString("Greeting", manager, typeof(string)));
            Assert.NotEqual<LocalizableString>(first, new LocalizableStringImpl("Hej"));
            Assert.Equal("", new LocalizableResourceString("Missing", manager, typeof(LocalizableStringTests)).ToString());
        }
        finally
        {
            CultureInfo.CurrentUICulture = previous;
        }
    }

    [Fact]
    public void LiteralSubclassesDoNotLoseTheirTypeInEquality()
    {
        Assert.NotEqual<LocalizableString>(new LocalizableStringImpl("text"), new DerivedLiteral("text"));
    }

    [Fact]
    public void CustomLocalizableStringsKeepReferenceIdentityUnlessOverridden()
    {
        LocalizableString first = new CustomString();
        LocalizableString second = new CustomString();
        Assert.NotEqual(first, second);
        Assert.False(first == second);
        Assert.True(first != second);
        Assert.Equal(first, first);
    }

    private sealed class CustomString : LocalizableString
    {
        public override string ToString() => "same display";
    }

    private sealed class DerivedLiteral(string text) : LocalizableStringImpl(text);

    private sealed class TestResources : ResourceManager
    {
        public override string? GetString(string name, CultureInfo? culture) => name == "Greeting"
            ? (culture?.TwoLetterISOLanguageName == "sv" ? "Hej" : "Hello")
            : null;
    }
}
