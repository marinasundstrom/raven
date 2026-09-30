using System.Collections.Immutable;

using Raven.CodeAnalysis.Metadata;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Tests;

public sealed class ExtensionReceiverResolverTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void MethodReceiverResolutionUsesProviderAndPreservesMemberContext(bool constructed)
    {
        var compilation = Compilation.Create("test", [], TestMetadataReferences.Default);
        var owner = new ProviderType(compilation);
        var receiver = new SourceTypeParameterSymbol("Receiver", owner, owner,
            compilation.SourceGlobalNamespace, [], [], 0, TypeParameterConstraintKind.None, [], VarianceKind.None);
        owner.Receiver = receiver;
        var definition = new SourceMethodSymbol("Create", compilation.GetSpecialType(SpecialType.System_Object), [],
            owner, owner, compilation.SourceGlobalNamespace, [], []);
        var methodParameter = new SourceTypeParameterSymbol("T", definition, owner,
            compilation.SourceGlobalNamespace, [], [], 0, TypeParameterConstraintKind.None, [], VarianceKind.None);
        definition.SetTypeParameters([methodParameter]);
        IMethodSymbol method = constructed
            ? new ConstructedMethodSymbol(definition, [compilation.GetSpecialType(SpecialType.System_Int32)],
                new ConstructedNamedTypeSymbol(owner, []))
            : definition;

        Assert.Same(receiver, method.GetExtensionReceiverType());
        Assert.Same(method, owner.ResolvedMember);
        // Core must not reinterpret the provider's result by ordinal as method T.
        Assert.NotSame(methodParameter, method.GetExtensionReceiverType());
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void PropertyResolutionUsesProviderWhenNoAccessorSuppliesReceiver(bool hasReceiver)
    {
        var compilation = Compilation.Create("test", [], TestMetadataReferences.Default);
        var owner = new ProviderType(compilation);
        var propertyType = compilation.GetSpecialType(SpecialType.System_Object);
        owner.Receiver = hasReceiver ? propertyType : null;
        IPropertySymbol property = new SourcePropertySymbol("Empty", propertyType, owner, owner,
            compilation.SourceGlobalNamespace, [], []);

        Assert.Same(owner.Receiver, property.GetExtensionReceiverType());
        Assert.Same(property, owner.ResolvedMember);
    }

    [Fact]
    public void AccessorReceiverTakesPrecedenceOverPropertyProvider()
    {
        var compilation = Compilation.Create("test", [], TestMetadataReferences.Default);
        var owner = new ProviderType(compilation);
        var accessorOwner = new ProviderType(compilation);
        var receiver = compilation.GetSpecialType(SpecialType.System_String);
        accessorOwner.Receiver = receiver;
        owner.Receiver = compilation.GetSpecialType(SpecialType.System_Object);
        var getter = new SourceMethodSymbol("get_Empty", receiver, [], accessorOwner, accessorOwner,
            compilation.SourceGlobalNamespace, [], [], methodKind: MethodKind.PropertyGet);
        var property = new SourcePropertySymbol("Empty", receiver, owner, owner,
            compilation.SourceGlobalNamespace, [], []);
        property.SetAccessors(getter, null);

        Assert.Same(receiver, ((IPropertySymbol)property).GetExtensionReceiverType());
        Assert.Same(getter, accessorOwner.ResolvedMember);
        Assert.Null(owner.ResolvedMember);
    }

    private sealed class ProviderType(Compilation compilation)
        : SourceNamedTypeSymbol("Extensions", compilation.GetSpecialType(SpecialType.System_Object), TypeKind.Class,
            compilation.Assembly, null, compilation.SourceGlobalNamespace, [], [], addAsMember: false),
            IExtensionReceiverResolver, INamespaceOrTypeSymbol
    {
        internal ITypeSymbol? Receiver { get; set; }
        internal ISymbol? ResolvedMember { get; private set; }
        public ITypeSymbol? GetExtensionReceiverType(IMethodSymbol method)
        {
            ResolvedMember = method;
            return Receiver;
        }
        public ITypeSymbol? GetExtensionReceiverType(IPropertySymbol property)
        {
            ResolvedMember = property;
            return Receiver;
        }
        public override ImmutableArray<AttributeData> GetAttributes()
            => throw new InvalidOperationException("Receiver lookup must use provider semantics.");
        ImmutableArray<ISymbol> INamespaceOrTypeSymbol.GetMembers()
            => throw new InvalidOperationException("Receiver lookup must not enumerate members.");
    }
}
