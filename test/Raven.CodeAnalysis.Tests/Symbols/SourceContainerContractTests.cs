using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class SourceContainerContractTests
{
    [Fact]
    public void AssemblyAndModuleGlobalNamespacesAreStableUnderConcurrentAccess()
    {
        var assembly = new SourceAssemblySymbol(new Services(), "Test", []);
        var module = new SourceModuleSymbol("Main", assembly, [], [], []);
        var globals = new NamespaceSymbol[32];
        Parallel.For(0, globals.Length, i => globals[i] = assembly.GlobalNamespace);
        Assert.All(globals, item => Assert.Same(module.GlobalNamespace, item));
        Assert.Same(module, Assert.Single(assembly.Modules));
        Assert.Equal("Test", assembly.FullName);
        Assert.Same(assembly, module.ContainingAssembly);
        Assert.Same(module, module.GlobalNamespace.ContainingModule);
        Assert.Same(assembly, module.GlobalNamespace.ContainingAssembly);
    }

    [Fact]
    public void MultipleModulesMergeGlobalNamespaces()
    {
        var assembly = new SourceAssemblySymbol(new Services(), "Test", []);
        var first = new SourceModuleSymbol("First", assembly, [], [], []);
        var second = new SourceModuleSymbol("Second", assembly, [], [], []);
        Assert.IsType<MergedNamespaceSymbol>(assembly.GlobalNamespace);
        Assert.Equal(new[] { first.GlobalNamespace, second.GlobalNamespace }, ((MergedNamespaceSymbol)assembly.GlobalNamespace).GetMergedNamespaces());
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void NamespacePolicyAndDeclarationCompletionRemainOwnedByServices(bool modules)
    {
        var services = new Services { NamespacesRepresentModules = modules };
        var assembly = new SourceAssemblySymbol(services, "Test", []);
        var module = new SourceModuleSymbol("Main", assembly, [], [], []);
        var global = module.GlobalNamespace;
        var child = new SourceNamespaceSymbol("Demo", global, global, [], []);
        Assert.Equal(modules, child.IsModule);
        Assert.Equal("Demo", child.MetadataName);
        Assert.Equal("Demo", child.ToString());
        Assert.Same(child, global.LookupNamespace("Demo"));
        Assert.Empty(services.CompletionRequests);
        Assert.Null(child.LookupType("Missing"));
        Assert.Equal(new[] { true }, services.CompletionRequests);
        Assert.Same(child, module.GetModuleNamespace(child));
        Assert.Equal(new[] { true, false }, services.CompletionRequests);
        Assert.True(global.IsMemberDefined("Demo", out var member));
        Assert.Same(child, member);
    }

    [Fact]
    public void MetadataLookupHandlesGenericNestedTypesAndNamespaceCollisions()
    {
        var assembly = new SourceAssemblySymbol(new Services(), "Test", []);
        var module = new SourceModuleSymbol("Main", assembly, [], [], []);
        var global = module.GlobalNamespace;
        var ns = new SourceNamespaceSymbol("Demo", global, global, [], []);
        var inner = Type("Inner", 1);
        var outer = Type("Outer", 1, [inner]);
        ((SymbolMemberContainer)ns).AddMember(outer);
        _ = new SourceNamespaceSymbol("Outer`1", ns, ns, [], []);
        Assert.Same(outer, assembly.GetTypeByMetadataName("Demo.Outer`1"));
        Assert.Same(inner, assembly.GetTypeByMetadataName("Demo.Outer`1+Inner`1"));
        Assert.Same(inner, assembly.GetTypeByMetadataName("Demo.Outer`1.Inner`1"));
        Assert.Null(assembly.GetTypeByMetadataName("Demo.Outer`2"));
        Assert.Null(assembly.GetTypeByMetadataName(" "));
        Assert.Null(assembly.GetTypeByMetadataName("Demo.Outer`1+Missing"));
    }

    [Fact]
    public void MetadataMemberLookupUsesLocalDeclarationsBeforeReferencedAssemblies()
    {
        var services = new Services();
        var reference = Make<AssemblySymbol>(SymbolKind.Assembly, "Reference");
        var imported = Type("Imported", 0);
        services.ReferencedTypes[reference] = imported;
        var assembly = new SourceAssemblySymbol(services, "Test", []);
        var module = new SourceModuleSymbol("Main", assembly, [reference], [], []);
        Assert.Same(reference, Assert.Single(module.ReferencedAssemblySymbols));
        Assert.Same(imported, module.ResolveMetadataMember(module.GlobalNamespace, "Imported"));
        Assert.Equal(1, services.ReferenceLookupCount);
        var local = Type("Imported", 0);
        ((SymbolMemberContainer)module.GlobalNamespace).AddMember(local);
        Assert.Same(local, module.ResolveMetadataMember(module.GlobalNamespace, "Imported"));
        Assert.Null(module.ResolveMetadataMember(module.GlobalNamespace, "Missing"));
    }

    private static NamedTypeSymbol Type(string name, int arity, ImmutableArray<Symbol> members = default)
    {
        var type = Make<NamedTypeSymbol>(SymbolKind.Type, name);
        Set(type, "get_MetadataName", arity == 0 ? name : name + "`" + arity);
        Set(type, "get_TypeArguments", ImmutableArray<TypeSymbol>.Empty);
        Set(type, "get_TypeParameters", ImmutableArray<TypeParameterSymbol>.Empty);
        Set(type, "get_Arity", arity);
        Set(type, "GetMembers", (Func<object?[]?, object?>)(args => members.IsDefault ? ImmutableArray<Symbol>.Empty
            : args is { Length: > 0 } ? members.Where(item => item.Name == (string)args[0]!).ToImmutableArray() : members));
        return type;
    }
    private sealed class Services : SourceContainerServices
    {
        public bool NamespacesRepresentModules { get; init; }
        public bool IsReleaseOptimizationEnabled => true;
        public OperatorTypeProvider OperatorTypes => throw new NotSupportedException();
        public List<bool> CompletionRequests { get; } = [];
        public void EnsureSourceDeclarations(bool respectNamespaceLookupSuppression) => CompletionRequests.Add(respectNamespaceLookupSuppression);
        public Dictionary<AssemblySymbol, NamedTypeSymbol> ReferencedTypes { get; } = new(System.Collections.Generic.ReferenceEqualityComparer.Instance);
        public int ReferenceLookupCount { get; private set; }
        public NamedTypeSymbol? FindReferencedType(AssemblySymbol assembly, string metadataName)
        {
            ReferenceLookupCount++;
            return ReferencedTypes.TryGetValue(assembly, out var type) && type.MetadataName == metadataName ? type : null;
        }
        public TypeSymbol? LastOwner { get; private set; }
        public ImmutableArray<Symbol> GetFunctionContractMembers(TypeSymbol owner)
        {
            LastOwner = owner;
            return [];
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

}
