namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class MetadataImportOptionsMetadataTests
{
    [Fact]
    public void PublicDataSurfacePreservesSealedRecordAndInitAccessibility()
    {
        var type = typeof(MetadataImportOptions);
        Assert.True(type.IsSealed);
        Assert.Null(type.GetProperty("CoreAssemblyName")!.SetMethod);
        Assert.Null(type.GetProperty("PrimitiveAssemblies")!.SetMethod);
        foreach (var name in new[] { "AsyncAssemblyName", "ObjectAssemblyName", "UseNativeMetadata", "UseSourceCore" })
        {
            var setter = type.GetProperty(name)!.SetMethod!;
            Assert.True(setter.IsPrivate);
            Assert.Contains(typeof(System.Runtime.CompilerServices.IsExternalInit), setter.ReturnParameter.GetRequiredCustomModifiers());
        }
    }

    [Fact]
    public void SealedRecordCopyConstructorIsPrivateWithoutUnneededRequiredMembersMarker()
    {
        var type = typeof(MetadataImportOptions);
        var constructor = type.GetConstructor(System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.NonPublic,
            binder: null, [type], modifiers: null)!;
        Assert.NotNull(constructor);
        Assert.True(constructor.IsPrivate);
        Assert.DoesNotContain(constructor.CustomAttributes,
            attribute => attribute.AttributeType.FullName == "System.Diagnostics.CodeAnalysis.SetsRequiredMembersAttribute");
    }
}
