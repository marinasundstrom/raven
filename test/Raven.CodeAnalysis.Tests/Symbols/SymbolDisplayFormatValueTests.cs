using System.Reflection;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "Shared")]
public sealed class SymbolDisplayFormatValueTests
{
    [Fact]
    public void EqualConfigurationsHaveValueEquality()
    {
        var first = SymbolDisplayFormat.RavenSignatureFormat;
        var second = first.WithDelegateStyle(first.DelegateStyle);
        Assert.NotSame(first, second);
        Assert.True(first.Equals(second));
        Assert.True(first.Equals((object)second));
        Assert.True(first == second);
        Assert.False(first != second);
        Assert.Equal(first.GetHashCode(), second.GetHashCode());
        Assert.True(new HashSet<SymbolDisplayFormat> { first }.Contains(second));
    }

    [Fact]
    public void NullAndOtherObjectsAreNotEqual()
    {
        var value = new SymbolDisplayFormat();
        SymbolDisplayFormat? absent = null;
        Assert.False(value.Equals(absent));
        Assert.False(value.Equals(new object()));
        Assert.False(value == absent);
        Assert.False(absent == value);
        Assert.True(value != absent);
        Assert.True(absent == null);
        Assert.False(absent != null);
    }

    [Fact]
    public void EachWithMethodChangesOnlyItsOptionAndPreservesTheSource()
    {
        var original = new SymbolDisplayFormat();
        var properties = typeof(SymbolDisplayFormat).GetProperties(BindingFlags.Public | BindingFlags.Instance);
        Assert.Equal(11, properties.Length);
        foreach (var property in properties)
        {
            var selected = Enum.GetValues(property.PropertyType).Cast<object>().Last();
            var method = typeof(SymbolDisplayFormat).GetMethod("With" + property.Name)!;
            var changed = Assert.IsType<SymbolDisplayFormat>(method.Invoke(original, [selected]));
            Assert.NotSame(original, changed);
            Assert.True(original != changed);
            Assert.False(original.Equals(changed));
            foreach (var option in properties)
            {
                var zero = Activator.CreateInstance(option.PropertyType);
                Assert.Equal(zero, option.GetValue(original));
                Assert.Equal(option == property ? selected : zero, option.GetValue(changed));
            }
            var restored = Assert.IsType<SymbolDisplayFormat>(method.Invoke(changed, [Activator.CreateInstance(property.PropertyType)]));
            Assert.True(original == restored);
            Assert.Equal(original.GetHashCode(), restored.GetHashCode());
        }
    }

    private sealed class DerivedFormat : SymbolDisplayFormat;

    [Fact]
    public void EqualityRetainsRuntimeTypeAndPrivateMutationBoundaries()
    {
        var value = new SymbolDisplayFormat();
        var derived = new DerivedFormat();
        Assert.False(value.Equals(derived));
        Assert.False(derived.Equals(value));
        Assert.True(value != derived);
        foreach (var property in typeof(SymbolDisplayFormat).GetProperties(BindingFlags.Public | BindingFlags.Instance))
        {
            Assert.True(property.GetMethod!.IsPublic);
            Assert.True(property.SetMethod!.IsPrivate);
        }
    }

    [Fact]
    public void PresetsRetainTheirDistinctDisplayPolicies()
    {
        Assert.Equal(SymbolDisplayFormat.RavenSignatureFormat, SymbolDisplayFormat.RavenTooltipFormat);
        Assert.Equal(SymbolDisplayTypeQualificationStyle.NameOnly, SymbolDisplayFormat.MinimallyQualifiedFormat.TypeQualificationStyle);
        Assert.Equal(SymbolDisplayGlobalNamespaceStyle.Included, SymbolDisplayFormat.FullyQualifiedFormat.GlobalNamespaceStyle);
        Assert.Equal(SymbolDisplayGenericsOptions.None, SymbolDisplayFormat.RavenShortErrorMessageFormat.GenericsOptions);
        Assert.True(SymbolDisplayFormat.RavenDebuggerFormat.GenericsOptions.HasFlag(SymbolDisplayGenericsOptions.IncludeTypeConstraints));
        Assert.True(SymbolDisplayFormat.RavenCodeGenerationFormat.MiscellaneousOptions.HasFlag(SymbolDisplayMiscellaneousOptions.RemoveAttributeSuffix));
    }
}
