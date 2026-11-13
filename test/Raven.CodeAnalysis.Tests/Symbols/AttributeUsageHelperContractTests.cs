using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class AttributeUsageHelperContractTests
{
    [Theory]
    [InlineData(TypeKind.Class, AttributeTargets.Class)]
    [InlineData(TypeKind.Struct, AttributeTargets.Struct)]
    [InlineData(TypeKind.Interface, AttributeTargets.Interface)]
    [InlineData(TypeKind.Enum, AttributeTargets.Enum)]
    [InlineData(TypeKind.Delegate, AttributeTargets.Delegate)]
    public void DefaultTargetPreservesTypeKind(TypeKind kind, AttributeTargets expected)
    {
        var owner = Named("Test", "Owner");
        Set(owner, "get_TypeKind", kind);
        Assert.Equal(expected, AttributeUsageHelper.GetDefaultTargetForOwner(owner));
    }

    [Fact]
    public void DefaultUsageRejectsDuplicatesPerTarget()
    {
        var provider = new Provider();
        var owner = Make<MethodSymbol>(SymbolKind.Method, "M");
        var attribute = Data(Named("Test", "TagAttribute"));
        var seen = new Dictionary<AttributeTargets, HashSet<NamedTypeSymbol>>();
        Assert.True(Validate(provider, owner, attribute, AttributeTargets.Method, seen));
        Assert.False(Validate(provider, owner, attribute, AttributeTargets.Method, seen));
        Assert.True(Validate(provider, owner, attribute, AttributeTargets.ReturnValue, seen));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void InheritedUsageControlsTargetsAndMultiplicity(bool allowMultiple)
    {
        var provider = new Provider();
        var parent = Named("Test", "BaseAttribute");
        var derived = Named("Test", "DerivedAttribute");
        Set(derived, "get_BaseType", parent);
        provider.Attributes[parent] = [Data(provider.UsageType,
            [TypedConstant.CreatePrimitive(null, (int)AttributeTargets.Method)],
            [new("AllowMultiple", TypedConstant.CreatePrimitive(null, allowMultiple))])];
        var owner = Make<MethodSymbol>(SymbolKind.Method, "M");
        var seen = new Dictionary<AttributeTargets, HashSet<NamedTypeSymbol>>();
        Assert.True(Validate(provider, owner, Data(derived), AttributeTargets.Method, seen));
        Assert.Equal(allowMultiple, Validate(provider, owner, Data(derived), AttributeTargets.Method, seen));
        Assert.False(Validate(provider, owner, Data(derived), AttributeTargets.ReturnValue, seen));
    }

    [Fact]
    public void DirectUsageReplacesInheritedUsageAndCyclesTerminate()
    {
        var provider = new Provider();
        var parent = Named("Test", "BaseAttribute");
        var derived = Named("Test", "DerivedAttribute");
        Set(derived, "get_BaseType", parent);
        Set(parent, "get_BaseType", derived);
        provider.Attributes[parent] = [Data(provider.UsageType, [TypedConstant.CreatePrimitive(null, (int)AttributeTargets.Field)])];
        provider.Attributes[derived] = [Data(provider.UsageType)];
        var owner = Make<MethodSymbol>(SymbolKind.Method, "M");
        Assert.True(Validate(provider, owner, Data(derived), AttributeTargets.Method, []));
        provider.Attributes.Clear();
        Assert.True(Validate(provider, owner, Data(derived), AttributeTargets.Method, []));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ExplicitMethodTargetUsesPrimaryConstructorContract(bool primary)
    {
        var provider = new Provider { Primary = primary };
        var owner = Make<MethodSymbol>(SymbolKind.Method, ".ctor");
        var syntax = Syntax("method");
        Assert.Equal(primary, AttributeUsageHelper.TryValidateAttribute(provider, null, owner, syntax,
            Data(Named("Test", "TagAttribute")), AttributeTargets.Constructor, [], out var target));
        Assert.Equal(AttributeTargets.Constructor, target);
    }

    [Fact]
    public void InvalidTargetDiagnosticFormatsAllAllowedTargets()
    {
        var provider = new Provider();
        var attribute = Named("Test", "TagAttribute");
        provider.Attributes[attribute] = [Data(provider.UsageType,
            [TypedConstant.CreatePrimitive(null, (int)(AttributeTargets.Class | AttributeTargets.ReturnValue | AttributeTargets.GenericParameter))])];
        var diagnostics = new DiagnosticBag();
        Assert.False(AttributeUsageHelper.TryValidateAttribute(provider, diagnostics,
            Make<MethodSymbol>(SymbolKind.Method, "M"), Syntax(), Data(attribute), AttributeTargets.Method, []));
        var message = Assert.Single(diagnostics.AsEnumerable()).GetMessage();
        Assert.Contains("class", message);
        Assert.Contains("return value", message);
        Assert.Contains("generic parameter", message);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ExplicitClassTargetRequiresNamespaceContainerContract(bool container)
    {
        var provider = new Provider { NamespaceContainer = container };
        Assert.Equal(container, AttributeUsageHelper.TryValidateAttribute(provider, null,
            Named("Test", "Container"), Syntax("class"), Data(Named("Test", "TagAttribute")), AttributeTargets.Class, []));
    }

    private static NamedTypeSymbol Named(string assembly, string name)
    {
        var type = MetadataSymbolFixture.Named(assembly, name);
        Set(type, "get_TypeParameters", ImmutableArray<TypeParameterSymbol>.Empty);
        Set(type, "get_Arity", 0);
        Set(type, "get_BaseType", null);
        Set(type, "Equals", (Func<object?[]?, object?>)(args => ReferenceEquals(type, args![0])));
        return type;
    }

    private static bool Validate(Provider provider, Symbol owner, AttributeData data, AttributeTargets target,
        Dictionary<AttributeTargets, HashSet<NamedTypeSymbol>> seen)
        => AttributeUsageHelper.TryValidateAttribute(provider, null, owner, Syntax(), data, target, seen);

    private static AttributeSyntax Syntax(string? target = null)
        => SyntaxTree.ParseText($"[{(target is null ? "" : target + ": ")}Tag] class C {{}}")
            .GetRoot().DescendantNodes().OfType<AttributeSyntax>().Single();

    private static AttributeData Data(NamedTypeSymbol type, ImmutableArray<TypedConstant> arguments = default,
        ImmutableArray<KeyValuePair<string, TypedConstant>> named = default)
        => new(type, Make<MethodSymbol>(SymbolKind.Method, ".ctor"),
            arguments.IsDefault ? [] : arguments, named.IsDefault ? [] : named, null);

    private sealed class Provider : AttributeValidationServices
    {
        public NamedTypeSymbol UsageType { get; } = Named("Runtime", "System.AttributeUsageAttribute");
        public Dictionary<Symbol, ImmutableArray<AttributeData>> Attributes { get; } = [];
        public bool Primary { get; init; }
        public bool NamespaceContainer { get; init; }

        public NamedTypeSymbol? GetTypeByMetadataName(string metadataName)
            => metadataName == "System.AttributeUsageAttribute" ? UsageType : null;

        public bool IsPrimaryConstructor(Symbol symbol) => Primary;
        public bool IsNamespaceContainer(Symbol symbol) => NamespaceContainer;
        public NamedTypeSymbol GetSpecialType(SpecialType specialType) => throw new NotSupportedException();
        public Conversion ClassifyConversion(TypeSymbol source, TypeSymbol destination, bool includeUserDefined)
            => throw new NotSupportedException();
        public ImmutableArray<AttributeData> GetAttributesForUsageValidation(Symbol symbol)
            => Attributes.GetValueOrDefault(symbol, []);
    }
}
