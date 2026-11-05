using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public class PEMetadataIdentityValueTests
{
    [Fact]
    public void TypeIdentityUsesAssemblyAndMetadataNameValues()
    {
        var identity = new PETypeIdentity("Library", "Example.Outer`1+Inner");
        var equal = new PETypeIdentity(new string("Library".ToCharArray()), new string("Example.Outer`1+Inner".ToCharArray()));
        Assert.Equal(identity, equal);
        Assert.True(identity == equal);
        Assert.Equal(identity.GetHashCode(), equal.GetHashCode());
        Assert.NotEqual(identity, identity with { AssemblyName = "Other" });
        Assert.NotEqual(identity, identity with { MetadataName = "Example.Other" });
        var (assembly, metadataName) = identity;
        Assert.Equal("Library", assembly);
        Assert.Equal("Example.Outer`1+Inner", metadataName);
        Assert.Equal("PETypeIdentity { AssemblyName = Library, MetadataName = Example.Outer`1+Inner }", identity.ToString());
    }

    [Fact]
    public void TypeParameterIdentityPreservesOwnerOrdinalAndName()
    {
        var identity = new PETypeParameterIdentity(TypeParameterOwnerKind.Method, 1, "Example.Owner.Method", "T");
        var equal = new PETypeParameterIdentity(TypeParameterOwnerKind.Method, 1, "Example.Owner.Method", "T");
        Assert.True(identity == equal);
        Assert.Equal(identity.GetHashCode(), equal.GetHashCode());
        Assert.NotEqual(identity, identity with { OwnerKind = TypeParameterOwnerKind.Type });
        Assert.NotEqual(identity, identity with { Ordinal = 0 });
        Assert.NotEqual(identity, identity with { OwnerIdentity = "Example.Other.Method" });
        Assert.NotEqual(identity, identity with { Name = "U" });
        var (kind, ordinal, owner, name) = identity;
        Assert.Equal(TypeParameterOwnerKind.Method, kind);
        Assert.Equal(1, ordinal);
        Assert.Equal("Example.Owner.Method", owner);
        Assert.Equal("T", name);
        Assert.Equal("PETypeParameterIdentity { OwnerKind = Method, Ordinal = 1, OwnerIdentity = Example.Owner.Method, Name = T }", identity.ToString());
    }

    [Fact]
    public void DefaultIdentitiesRetainRecordValueEquality()
    {
        Assert.Equal("PETypeIdentity { AssemblyName = , MetadataName =  }", default(PETypeIdentity).ToString());
        Assert.Equal(default(PETypeIdentity), new PETypeIdentity(null!, null!));
        Assert.Equal(default(PETypeParameterIdentity), new PETypeParameterIdentity(default, 0, null!, null!));
    }
}
