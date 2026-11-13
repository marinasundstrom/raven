using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "NeoCLR")]
public sealed class StructuralFunctionSubstitutionTests
{
    [Fact]
    public void CallableContractRetainsImplementationAndRebuildsInvokeSignature()
    {
        var compilation = Compilation.Create("Types", [], TestMetadataReferences.Default, CompilationOptions.DotNet);
        var integer = compilation.GetSpecialType(SpecialType.System_Int32);
        var text = compilation.GetSpecialType(SpecialType.System_String);
        var root = (NamedTypeSymbol)compilation.GetSpecialType(SpecialType.System_Object);
        var original = new StructuralFunctionTypeSymbol([integer], [RefKind.Ref], integer, root);
        var contract = Assert.IsAssignableFrom<StructuralFunctionSubstitution>(original);
        Assert.Same(original, contract.Substitute(type => type));
        var changed = Assert.IsType<StructuralFunctionTypeSymbol>(contract.Substitute(_ => text));
        Assert.Same(text, changed.ReturnType);
        Assert.Equal(new[] { text }, changed.ParameterTypes);
        Assert.Equal(new[] { RefKind.Ref }, changed.ParameterRefKinds);
        Assert.Same(text, changed.InvokeMethod.ReturnType);
        var parameter = Assert.Single(changed.InvokeMethod.Parameters);
        Assert.Same(text, parameter.Type);
        Assert.Equal(RefKind.Ref, parameter.RefKind);
        Assert.Same(integer, original.ReturnType);
    }
}
