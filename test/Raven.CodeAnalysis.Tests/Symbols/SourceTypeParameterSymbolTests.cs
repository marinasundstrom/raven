using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class SourceTypeParameterSymbolTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void OwnershipAndUnresolvedConstraintsPreserveDeclarationIdentity(bool method)
    {
        Symbol owner = method ? Make<MethodSymbol>(SymbolKind.Method, "M") : Make<NamedTypeSymbol>(SymbolKind.Type, "C");
        var parameter = Parameter(owner: owner);
        Assert.Equal(method ? TypeParameterOwnerKind.Method : TypeParameterOwnerKind.Type, parameter.OwnerKind);
        Assert.Same(method ? owner : null, parameter.DeclaringMethodParameterOwner);
        Assert.Same(method ? null : owner, parameter.DeclaringTypeParameterOwner);
        Assert.False(parameter.HasResolvedConstraintTypes);
        Assert.Empty(parameter.ConstraintTypes);
        parameter.SetConstraintTypes(default);
        Assert.True(parameter.HasResolvedConstraintTypes);
        Assert.Empty(parameter.ConstraintTypes);
        Assert.Same(parameter, parameter.OriginalDefinition);
        Assert.NotSame(parameter, Parameter(owner: owner));
        Assert.Equal(2, parameter.Ordinal);
        Assert.Equal(VarianceKind.Out, parameter.Variance);
    }

    [Theory]
    [InlineData(TypeParameterConstraintKind.ReferenceType, true, false)]
    [InlineData(TypeParameterConstraintKind.ValueType, false, true)]
    [InlineData((TypeParameterConstraintKind)0, false, false)]
    public void ConstraintFlagsDetermineKnownKinds(TypeParameterConstraintKind kind, bool reference, bool value)
    {
        var parameter = Parameter(kind);
        Assert.Equal(reference, parameter.IsReferenceType);
        Assert.Equal(value, parameter.IsValueType);
    }

    [Fact]
    public void CyclicConstraintsTerminateAndFindAClassThroughTheCycle()
    {
        var first = Parameter();
        var second = Parameter(name: "U");
        first.SetConstraintTypes([second]);
        second.SetConstraintTypes([first]);
        Assert.False(first.IsReferenceType);
        var node = Make<NamedTypeSymbol>(SymbolKind.Type, "Node");
        Set(node, "get_TypeKind", TypeKind.Class);
        Set(node, "get_IsNullable", false);
        second.SetConstraintTypes([first, node]);
        Assert.True(first.IsReferenceType);
        Assert.True(second.IsReferenceType);
    }

    [Fact]
    public void InterfacesAreDeduplicatedAndInheritedOnlyWhenRequested()
    {
        var parent = Interface("Parent");
        var child = Interface("Child");
        Set(child, "get_AllInterfaces", ImmutableArray.Create(parent));
        var parameter = Parameter();
        parameter.SetConstraintTypes([child, child, parent]);
        Assert.Equal(new[] { child, parent }, parameter.Interfaces);
        Assert.Equal(new[] { child, parent }, parameter.AllInterfaces);
        parameter.SetConstraintTypes([child]);
        Assert.Equal(new[] { child }, parameter.Interfaces);
        Assert.Equal(new[] { child, parent }, parameter.AllInterfaces);
        Assert.False(parameter.IsReferenceType);
    }

    [Fact]
    public void DetachedParametersHaveNoSyntheticMembers()
    {
        var parameter = Parameter();
        Assert.Empty(parameter.GetMembers());
        Assert.Empty(parameter.GetMembers("Invoke"));
        Assert.False(parameter.IsMemberDefined("Invoke", out var member));
        Assert.Null(member);
        Assert.Null(parameter.LookupType("T"));
    }

    [Fact]
    public void FunctionContractMembersUseDeclarationServicesAndKeepOwnerIdentity()
    {
        var member = Make<PropertySymbol>(SymbolKind.Property, "Function");
        var services = new Services([member]);
        var assembly = new Owner(services);
        var ns = Make<NamespaceSymbol>(SymbolKind.Namespace, "Test");
        Set(ns, "get_ContainingAssembly", assembly);
        var parameter = new SourceTypeParameterSymbol("T", ns, null, ns, [], [], 0, 0, [], VarianceKind.None);
        Assert.Same(member, Assert.Single(parameter.GetMembers()));
        Assert.Same(parameter, services.LastOwner);
        Assert.Same(member, Assert.Single(parameter.GetMembers("Function")));
        Assert.Empty(parameter.GetMembers("Missing"));
        Assert.True(parameter.IsMemberDefined("Function", out var found));
        Assert.Same(member, found);
    }

    private sealed class Owner(Services services)
        : SourceSymbol(SymbolKind.Namespace, "Owner", null, null, null, [], [], addAsMember: false), SourceDeclarationServiceProvider, AssemblySymbol
    {
        public NamespaceSymbol GlobalNamespace => throw new NotSupportedException();
        public IEnumerable<ModuleSymbol> Modules => [];
        public NamedTypeSymbol? GetTypeByMetadataName(string name) => null;
        public SourceDeclarationServices DeclarationServices => services;
        public override void Accept(SymbolVisitor visitor) => visitor.DefaultVisit(this);
        public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.DefaultVisit(this);
    }

    private sealed class Services(ImmutableArray<Symbol> members) : SourceDeclarationServices
    {
        public TypeSymbol? LastOwner { get; private set; }
        public ImmutableArray<Symbol> GetFunctionContractMembers(TypeSymbol owner)
        {
            LastOwner = owner;
            return members;
        }
        public SyntaxTree[] SyntaxTrees => [];
        public bool UsesDeferredSourceAttributes => false;
        public bool SourceDeclarationsComplete => true;
        public SourceAttributeDeclarationContext GetAttributeDeclarationContext(Symbol owner) => default;
        public bool TryGetCurrentDeclaringSyntax(SyntaxReference reference, out SyntaxNode syntax)
        {
            syntax = reference.GetSyntax();
            return true;
        }
        public SourceAttributeBinding? BindAttribute(Symbol owner, AttributeSyntax syntax) => null;
        public void PublishAttributeDiagnostics(SourceAttributeBinding attribute) => throw new NotSupportedException();
        public NamedTypeSymbol? GetTypeByMetadataName(string metadataName) => null;
        public bool IsPrimaryConstructor(Symbol symbol) => false;
        public bool IsNamespaceContainer(Symbol symbol) => false;
        public NamedTypeSymbol GetSpecialType(SpecialType specialType) => throw new NotSupportedException();
        public Conversion ClassifyConversion(TypeSymbol source, TypeSymbol destination, bool includeUserDefined) => throw new NotSupportedException();
        public ImmutableArray<AttributeData> GetAttributesForUsageValidation(Symbol symbol) => [];
    }

    private static NamedTypeSymbol Interface(string name)
    {
        var type = Make<NamedTypeSymbol>(SymbolKind.Type, name);
        Set(type, "get_TypeKind", TypeKind.Interface);
        Set(type, "get_IsNullable", false);
        Set(type, "get_TypeArguments", ImmutableArray<TypeSymbol>.Empty);
        Set(type, "get_TypeParameters", ImmutableArray<TypeParameterSymbol>.Empty);
        Set(type, "get_Arity", 0);
        Set(type, "get_AllInterfaces", ImmutableArray<NamedTypeSymbol>.Empty);
        return type;
    }

    private static SourceTypeParameterSymbol Parameter(TypeParameterConstraintKind kind = 0, Symbol? owner = null, string name = "T")
        => new(name, owner ?? Make<Symbol>(SymbolKind.Module, "Owner"), null, null, [], [], 2, kind, [], VarianceKind.Out);
}
