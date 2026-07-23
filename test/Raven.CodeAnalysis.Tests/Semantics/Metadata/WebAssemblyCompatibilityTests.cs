using System;

namespace Raven.CodeAnalysis.Tests.Semantics.Metadata;

public sealed class WebAssemblyCompatibilityTests
{
    [Fact]
    public void Assembly_identity_is_read_from_portable_executable_metadata()
    {
        var expected = typeof(object).Assembly.GetName();

        var actual = Compilation.ReadAssemblyNameFromMetadata(typeof(object).Assembly.Location);

        Assert.Equal(expected.Name, actual.Name);
        Assert.Equal(expected.Version, actual.Version);
        Assert.Equal(string.IsNullOrEmpty(expected.CultureName), string.IsNullOrEmpty(actual.CultureName));
        Assert.Equal(expected.GetPublicKeyToken(), actual.GetPublicKeyToken());
    }

    [Fact]
    public void Unavailable_runtime_nullability_reflection_uses_metadata_fallback()
    {
        Assert.True(ReflectionTypeLoader.IsUnavailableNullabilityReflection(new NotSupportedException()));
        Assert.True(ReflectionTypeLoader.IsUnavailableNullabilityReflection(new NotImplementedException()));
        Assert.False(ReflectionTypeLoader.IsUnavailableNullabilityReflection(new InvalidOperationException()));
    }
}
