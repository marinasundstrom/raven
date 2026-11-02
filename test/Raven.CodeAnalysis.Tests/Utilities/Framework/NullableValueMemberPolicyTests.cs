using Raven.CodeAnalysis.Targets;

namespace Raven.CodeAnalysis.Tests;

public sealed class NullableValueMemberPolicyTests
{
    [Theory]
    [InlineData("GetValueOrDefault", true)]
    [InlineData("HasValue", true)]
    [InlineData("Value", true)]
    [InlineData("CompareTo", false)]
    [Trait("Target", "DotNet")]
    public void DotNet_OnlyMapsDeclaredWrapperMembers(string name, bool mapped)
    {
        var compilation = Compilation.Create("NullablePolicy", [], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var nullable = compilation.GetSpecialType(SpecialType.System_Int32).GetNullableType();
        Assert.Equal(mapped, compilation.GetNullableValueMemberLookupType(nullable, name) is not null);
        var nullableReference = compilation.GetSpecialType(SpecialType.System_String).GetNullableType();
        Assert.Null(compilation.GetNullableValueMemberLookupType(nullableReference, name));
    }

    [Fact]
    [Trait("Target", "NeoCLR")]
    public void NeoCLR_DoesNotInheritDotNetNullableWrapperMapping()
    {
        var compilation = Compilation.Create("NullablePolicyInput", [], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var nullable = compilation.GetSpecialType(SpecialType.System_Int32).GetNullableType();
        var contract = new NeoClrCliRuntimeContract(CompilationOptions.NeoCLR);
        Assert.Null(contract.GetNullableValueMemberLookupType(compilation, nullable, "GetValueOrDefault"));
    }
}
