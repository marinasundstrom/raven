using System.Collections.Immutable;

using Raven.CodeAnalysis.Metadata;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Tests;

public sealed class ExtensionTypeInfoTests
{
    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void ProviderMemberFactsSupportDiscoveryWithoutACommonReceiver(bool constructed, bool hasExtensions)
    {
        var compilation = Compilation.Create("test", [], TestMetadataReferences.Default);
        var definition = new ProviderType(compilation) { HasMemberLevelExtensions = hasExtensions };
        INamedTypeSymbol type = constructed ? new ConstructedNamedTypeSymbol(definition, []) : definition;

        Assert.Null(type.GetExtensionReceiverType());
        Assert.Equal(hasExtensions, type.HasStaticExtensionMembers);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ProviderReceiverIsSubstitutedForConstructedTypes(bool constructed)
    {
        var compilation = Compilation.Create("test", [], TestMetadataReferences.Default);
        var definition = new ProviderType(compilation);
        var parameter = new SourceTypeParameterSymbol("T", definition, definition,
            compilation.SourceGlobalNamespace, [], [], 0, TypeParameterConstraintKind.None, [], VarianceKind.None);
        definition.SetTypeParameters([parameter]);
        var receiverDefinition = compilation.GetTypeByMetadataName("System.Collections.Generic.List`1")!;
        definition.Receiver = receiverDefinition.Construct(parameter);
        var argument = compilation.GetSpecialType(SpecialType.System_Int32);
        INamedTypeSymbol type = constructed ? new ConstructedNamedTypeSymbol(definition, [argument]) : definition;

        var receiver = Assert.IsAssignableFrom<INamedTypeSymbol>(type.GetExtensionReceiverType());
        Assert.True(SymbolEqualityComparer.Default.Equals(receiverDefinition, receiver.OriginalDefinition));
        Assert.True(SymbolEqualityComparer.Default.Equals(constructed ? argument : parameter, Assert.Single(receiver.TypeArguments)));
        Assert.True(type.HasStaticExtensionMembers);
    }

    private sealed class ProviderType(Compilation compilation)
        : SourceNamedTypeSymbol("Extensions", compilation.GetSpecialType(SpecialType.System_Object), TypeKind.Class,
            compilation.Assembly, null, compilation.SourceGlobalNamespace, [], [], addAsMember: false),
            IExtensionTypeInfo, INamespaceOrTypeSymbol
    {
        internal ITypeSymbol? Receiver { get; set; }
        ITypeSymbol? IExtensionTypeInfo.ExtensionReceiverType => Receiver;
        public bool HasMemberLevelExtensions { get; init; }
        public override ImmutableArray<AttributeData> GetAttributes()
            => throw new InvalidOperationException("Discovery must use provider facts, not bind attributes.");
        ImmutableArray<ISymbol> INamespaceOrTypeSymbol.GetMembers()
            => throw new InvalidOperationException("Discovery must not load ordinary members.");
    }
}
