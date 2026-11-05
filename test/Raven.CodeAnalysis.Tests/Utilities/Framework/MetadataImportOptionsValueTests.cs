namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "Shared")]
public sealed class MetadataImportOptionsValueTests
{
    [Fact]
    public void NativeObjectProviderSelectionIsImmutableAndExclusive()
    {
        var baseline = new MetadataImportOptions("Core");
        var selected = baseline.WithObjectAssemblyName("Root");
        Assert.Null(baseline.ObjectAssemblyName);
        Assert.Equal("Root", selected.ObjectAssemblyName);
        Assert.Equal("Core", selected.CoreAssemblyName);
        Assert.Null(selected.WithObjectAssemblyName(null).ObjectAssemblyName);
        Assert.Throws<ArgumentException>(() => baseline.WithObjectAssemblyName(" "));
        Assert.Throws<ArgumentException>(() => baseline.WithObjectAssemblyName(""));
        Assert.Throws<ArgumentException>(() => new MetadataImportOptions("Core", null, null, true).WithObjectAssemblyName("Root"));
    }

    [Fact]
    public void NativeAsyncProviderSelectionIsImmutableAndValidatesIdentity()
    {
        var baseline = new MetadataImportOptions("Core");
        var selected = baseline.WithAsyncAssemblyName("Tasks");
        Assert.Null(baseline.AsyncAssemblyName);
        Assert.Equal("Tasks", selected.AsyncAssemblyName);
        Assert.Null(selected.WithAsyncAssemblyName(null).AsyncAssemblyName);
        Assert.Equal("Core", selected.CoreAssemblyName);
        Assert.Throws<ArgumentException>(() => baseline.WithAsyncAssemblyName(" "));
        Assert.Throws<ArgumentException>(() => baseline.WithAsyncAssemblyName(""));
    }

    [Fact]
    public void PrimitiveProvidersAreCopiedAndRestrictedToSupportedDeclarations()
    {
        var providers = new Dictionary<SpecialType, string> { [SpecialType.System_Single] = "Numbers", [SpecialType.System_String] = "Text" };
        var options = new MetadataImportOptions("Core", providers);
        providers[SpecialType.System_Single] = "Changed";
        Assert.Equal("Numbers", options.PrimitiveAssemblies[SpecialType.System_Single]);
        Assert.Equal("Text", options.PrimitiveAssemblies[SpecialType.System_String]);
        Assert.Empty(new MetadataImportOptions("Core").PrimitiveAssemblies);
        Assert.Throws<ArgumentException>(() => new MetadataImportOptions("Core",
            new Dictionary<SpecialType, string> { [SpecialType.System_Object] = "Numbers" }));
        Assert.Throws<ArgumentException>(() => new MetadataImportOptions("Core",
            new Dictionary<SpecialType, string> { [SpecialType.System_Double] = " " }));
    }

    [Fact]
    public void StoredPropertiesParticipateInCopiesAndValueEquality()
    {
        var original = new MetadataImportOptions("Core");
        Assert.Equal(original, new MetadataImportOptions("Core"));
        Assert.NotEqual(original, new MetadataImportOptions("Other"));
        var native = original.WithNativeMetadata();
        Assert.False(original.UseNativeMetadata);
        Assert.True(native.UseNativeMetadata);
        Assert.Equal("Core", native.CoreAssemblyName);
        Assert.NotEqual(original, native);
        Assert.Equal(original, native.WithNativeMetadata(false));
        Assert.NotEqual(original, original.WithSourceCore());
        Assert.Equal(original.GetHashCode(), new MetadataImportOptions("Core").GetHashCode());
        Assert.Null(new MetadataImportOptions().CoreAssemblyName);
    }

    [Fact]
    public void DefaultsRetainCliLoadingAndNoExplicitProviders()
    {
        var options = new MetadataImportOptions();
        Assert.Null(options.CoreAssemblyName);
        Assert.Null(options.AsyncAssemblyName);
        Assert.Null(options.ObjectAssemblyName);
        Assert.False(options.UseNativeMetadata);
        Assert.False(options.UseSourceCore);
        Assert.False(options.UseSourceObjectRoot);
        Assert.Empty(options.PrimitiveAssemblies);
        Assert.Empty(options.SourcePrimitiveTypes);
        Assert.Equal(options, new MetadataImportOptions());
    }

    [Fact]
    public void SourceProvidersAreSnapshottedAndRejectConflicts()
    {
        var source = new List<SpecialType> { SpecialType.System_Int32 };
        var providers = new Dictionary<SpecialType, string> { [SpecialType.System_String] = "Strings" };
        var options = new MetadataImportOptions("Core", providers, source);
        source.Clear();
        providers.Clear();
        Assert.Contains(SpecialType.System_Int32, options.SourcePrimitiveTypes);
        Assert.Equal("Strings", options.PrimitiveAssemblies[SpecialType.System_String]);
        Assert.Throws<ArgumentException>(() => new MetadataImportOptions("Core", null, [SpecialType.System_Object]));
        Assert.Throws<ArgumentException>(() => new MetadataImportOptions("Core",
            new Dictionary<SpecialType, string> { [SpecialType.System_Int32] = "Numbers" }, [SpecialType.System_Int32]));
    }

    [Fact]
    public void CopyChainRetainsAllStoredStateAndLeavesOriginalUntouched()
    {
        var original = new MetadataImportOptions("Core",
            new Dictionary<SpecialType, string> { [SpecialType.System_String] = "Strings" },
            [SpecialType.System_Int32], true);
        var copy = original.WithAsyncAssemblyName("Tasks").WithNativeMetadata().WithSourceCore();
        Assert.True(copy.UseSourceObjectRoot);
        Assert.True(copy.UseNativeMetadata);
        Assert.True(copy.UseSourceCore);
        Assert.Equal("Tasks", copy.AsyncAssemblyName);
        Assert.Equal("Core", copy.CoreAssemblyName);
        Assert.Same(original.PrimitiveAssemblies, copy.PrimitiveAssemblies);
        Assert.Same(original.SourcePrimitiveTypes, copy.SourcePrimitiveTypes);
        Assert.Null(original.AsyncAssemblyName);
        Assert.False(original.UseNativeMetadata);
        Assert.False(original.UseSourceCore);
        Assert.Equal(original, copy.WithAsyncAssemblyName(null).WithNativeMetadata(false).WithSourceCore(false));
    }

    [Theory]
    [InlineData("")]
    [InlineData(" ")]
    [InlineData(null)]
    public void CoreIdentityIsRequired(string? name)
    {
        Assert.ThrowsAny<ArgumentException>(() => new MetadataImportOptions(name!));
    }
}
