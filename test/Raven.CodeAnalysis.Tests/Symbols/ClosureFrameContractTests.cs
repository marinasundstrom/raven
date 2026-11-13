using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class ClosureFrameContractTests
{
    [Theory]
    [InlineData("M<T>.Run", "Unused", "M_T__Run")]
    [InlineData("", "Run", "Run")]
    [InlineData(" ", "", "Ordinary")]
    public void FramePreservesOwnershipAndSanitizesNames(string metadataName, string name, string suffix)
    {
        var owner = Owner();
        Set(owner, "get_MetadataName", metadataName);
        Set(owner, "get_Name", name);
        var frame = ClosureFrameSymbolFactory.Create(owner);
        Assert.Equal("<>c__DisplayClass_" + suffix, frame.Name);
        Assert.Equal(frame.Name, frame.MetadataName);
        Assert.Same(owner.ContainingSymbol, frame.ContainingSymbol);
        Assert.Same(owner.ContainingType, frame.ContainingType);
        Assert.Equal(Accessibility.Private, frame.DeclaredAccessibility);
        Assert.True(frame.IsClosed);
        Assert.Empty(frame.TypeParameters);
    }

    [Fact]
    public void DeclarationProviderOwnsCanonicalObjectLookup()
    {
        var owner = Owner();
        var assembly = Make<IProviderAssembly>(SymbolKind.Assembly, "Source");
        var services = Make<SourceDeclarationServices>(SymbolKind.Assembly, "Services");
        var objectType = Named("Test", "CanonicalObject");
        Set(services, "GetSpecialType", (Func<object?[]?, object?>)(args =>
        {
            Assert.Equal(SpecialType.System_Object, args![0]);
            return objectType;
        }));
        Set(assembly, "get_DeclarationServices", services);
        Set(owner, "get_ContainingAssembly", assembly);
        Assert.Same(objectType, ClosureFrameSymbolFactory.Create(owner).BaseType);
    }

    [Fact]
    public void ClonesSourceParametersWithConstraintsAndDeclarationIdentity()
    {
        var owner = Owner();
        var syntax = SyntaxTree.ParseText("class Constraint {}").GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var reference = syntax.GetReference();
        var parameter = new SourceTypeParameterSymbol("T", owner, null, null,
            [syntax.GetLocation()], [reference], 0, TypeParameterConstraintKind.ReferenceType,
            [reference], VarianceKind.None);
        var constraint = Named("Test", "Constraint");
        parameter.SetConstraintTypes([constraint]);
        Set(owner, "get_TypeParameters", ImmutableArray.Create<TypeParameterSymbol>(parameter));
        var frame = ClosureFrameSymbolFactory.Create(owner);
        var clone = Assert.IsType<SourceTypeParameterSymbol>(Assert.Single(frame.TypeParameters));
        Assert.NotSame(parameter, clone);
        Assert.Same(frame, clone.ContainingSymbol);
        Assert.Same(frame, clone.DeclaringTypeParameterOwner);
        Assert.Equal(parameter.ConstraintKind, clone.ConstraintKind);
        Assert.Same(reference, Assert.Single(clone.ConstraintTypeReferences));
        Assert.Same(reference, Assert.Single(clone.DeclaringSyntaxReferences));
        Assert.Same(constraint, Assert.Single(clone.ConstraintTypes));
    }

    [Fact]
    public void ImportedParameterConstraintsAreReadOnce()
    {
        var owner = Owner();
        var parameter = Parameter("T", 0);
        Set(parameter, "get_Locations", ImmutableArray<Location>.Empty);
        Set(parameter, "get_DeclaringSyntaxReferences", ImmutableArray<SyntaxReference>.Empty);
        Set(parameter, "get_ConstraintKind", TypeParameterConstraintKind.None);
        var constraint = Named("Test", "Constraint");
        Set(parameter, "get_ConstraintTypes", ImmutableArray.Create<TypeSymbol>(constraint));
        Set(owner, "get_TypeParameters", ImmutableArray.Create(parameter));
        var clone = Assert.IsType<SourceTypeParameterSymbol>(Assert.Single(ClosureFrameSymbolFactory.Create(owner).TypeParameters));
        Assert.Empty(clone.ConstraintTypeReferences);
        Assert.Same(constraint, Assert.Single(clone.ConstraintTypes));
    }

    [Fact]
    public void InvalidOwnerAndMissingObjectPreserveFailures()
    {
        Assert.Throws<ArgumentNullException>(() => ClosureFrameSymbolFactory.Create(null!));
        var owner = Owner();
        Set(owner.ContainingAssembly, "GetTypeByMetadataName", null);
        Assert.Throws<InvalidOperationException>(() => ClosureFrameSymbolFactory.Create(owner));
        Set(owner, "get_ContainingAssembly", null);
        Assert.Throws<InvalidOperationException>(() => ClosureFrameSymbolFactory.Create(owner));
        owner = Owner();
        Set(owner, "get_ContainingSymbol", null);
        Assert.Throws<InvalidOperationException>(() => ClosureFrameSymbolFactory.Create(owner));
    }

    private static MethodSymbol Owner()
    {
        var owner = Make<MethodSymbol>(SymbolKind.Method, "Run");
        var assembly = Make<AssemblySymbol>(SymbolKind.Assembly, "Imported");
        Set(assembly, "GetTypeByMetadataName", Named("Test", "System.Object"));
        Set(owner, "get_ContainingAssembly", assembly);
        Set(owner, "get_MethodKind", MethodKind.Ordinary);
        Set(owner, "get_ContainingSymbol", Make<Symbol>(SymbolKind.Type, "Container"));
        Set(owner, "get_Locations", ImmutableArray<Location>.Empty);
        Set(owner, "get_DeclaringSyntaxReferences", ImmutableArray<SyntaxReference>.Empty);
        Set(owner, "get_TypeParameters", ImmutableArray<TypeParameterSymbol>.Empty);
        return owner;
    }

    internal interface IProviderAssembly : AssemblySymbol, SourceDeclarationServiceProvider { }
}
