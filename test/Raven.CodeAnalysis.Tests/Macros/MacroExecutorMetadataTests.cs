using Raven.CodeAnalysis.Macros;

namespace Raven.CodeAnalysis.Tests.Macros;

[Trait("Target", "Shared")]
public sealed class MacroExecutorMetadataTests
{
    [MacroExecutorTypeParameter("Second", 1)]
    [MacroExecutorTypeParameter("First", 0)]
    [MacroExecutorParameter("value", typeof(int), "int", MacroParameterSource.Value, 1, 0, true, "")]
    [MacroExecutorParameter("context", typeof(object), "object", MacroParameterSource.Context, 0, -1, false, "null")]
    private class Annotated { }

    private sealed class Derived : Annotated { }

    [Fact]
    public void ReflectionMetadata_IsOrderedNormalizedAndCached()
    {
        Assert.Equal(new[] { "First", "Second" }, MacroExecutorMetadata.GetTypeParameters(typeof(Annotated)));
        var parameters = MacroExecutorMetadata.GetParameters(typeof(Annotated));
        Assert.Equal(new[] { "context", "value" }, parameters.Select(p => p.Name));
        Assert.Null(parameters[0].InvocationArgumentOrdinal);
        Assert.Equal(0, parameters[1].InvocationArgumentOrdinal);
        Assert.Equal(typeof(int), parameters[1].RuntimeType);
        Assert.True(parameters[1].IsRequired);
        Assert.Equal("null", parameters[0].DefaultValueDisplay);
        Assert.True(parameters.Equals(MacroExecutorMetadata.GetParameters(typeof(Annotated))));
    }

    [Fact]
    public void ReflectionMetadata_DoesNotInheritAttributes()
    {
        Assert.Empty(MacroExecutorMetadata.GetParameters(typeof(Derived)));
        Assert.Empty(MacroExecutorMetadata.GetTypeParameters(typeof(Derived)));
    }

    [Fact]
    public void Factories_CopyInputArrays()
    {
        var names = new[] { "T" };
        var copiedNames = MacroExecutorMetadata.CreateTypeParameters(names);
        names[0] = "Changed";
        Assert.Equal("T", Assert.Single(copiedNames));
        var first = Parameter(-1);
        var parameters = new[] { first };
        var copiedParameters = MacroExecutorMetadata.CreateParameters(parameters);
        parameters[0] = Parameter(2);
        Assert.Same(first, Assert.Single(copiedParameters));
        Assert.Empty(MacroExecutorMetadata.CreateParameters());
    }

    [Fact]
    public void Parameter_HasValueEqualityAfterOrdinalNormalization()
    {
        var first = Parameter(-1);
        var second = Parameter(-2);
        Assert.Equal(first, second);
        Assert.True(first == second);
        Assert.Equal(first.GetHashCode(), second.GetHashCode());
        Assert.NotEqual(first, Parameter(0));
        Assert.Null(typeof(MacroExecutorParameter).GetProperty("Name")!.SetMethod);
        Assert.True(typeof(MacroExecutorParameter).IsSealed);
    }

    [Fact]
    public void MetadataQueries_RejectNullType()
    {
        Assert.Throws<ArgumentNullException>(() => MacroExecutorMetadata.GetParameters(null!));
        Assert.Throws<ArgumentNullException>(() => MacroExecutorMetadata.GetTypeParameters(null!));
    }

    private static MacroExecutorParameter Parameter(int ordinal)
        => new("value", typeof(int), "int", MacroParameterSource.Value, 0, ordinal, true, "");
}
