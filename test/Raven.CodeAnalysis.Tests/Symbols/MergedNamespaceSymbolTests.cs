using System.Collections.Immutable;

using Raven.CodeAnalysis.Metadata;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class MergedNamespaceSymbolTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ExtensionDiscovery_AcceptsNonPeNamespaceProvider(bool merge)
    {
        var compilation = Compilation.Create("test",
            [SyntaxTree.ParseText("namespace Shared { class Marker {} }")], TestMetadataReferences.Default);
        var sourceNamespace = compilation.GetTypeByMetadataName("Shared.Marker")!.ContainingNamespace!;
        var container = compilation.GetTypeByMetadataName("System.Linq.Enumerable")!;
        Assert.NotNull(container);
        INamespaceSymbol provider = new ExtensionNamespace("Select", [container, container]);
        if (merge)
        {
            var nested = new MergedNamespaceSymbol([provider], null);
            provider = new MergedNamespaceSymbol([nested, sourceNamespace, provider], null);
        }

        var result = compilation.SymbolLookup.GetExtensionContainers(
            provider, "Select", ExtensionMemberKinds.InstanceMethods);

        Assert.Same(container, Assert.Single(result));
        Assert.Empty(compilation.SymbolLookup.GetExtensionContainers(
            provider, "Missing", ExtensionMemberKinds.InstanceMethods));
    }

    [Fact]
    public void MergedExtensionDiscovery_PreservesProviderOrderAndRemovesDuplicates()
    {
        var compilation = Compilation.Create("test", [], TestMetadataReferences.Default);
        var first = compilation.GetTypeByMetadataName("System.Linq.Enumerable")!;
        var second = compilation.GetTypeByMetadataName("System.Linq.Queryable")!;
        Assert.NotNull(first);
        Assert.NotNull(second);
        var merged = new MergedNamespaceSymbol([
            new ExtensionNamespace("Select", [first]),
            new ExtensionNamespace("Select", [first, second])
        ], null);

        Assert.Equal(new[] { first, second }, merged.GetExtensionMethodContainers("Select"));
        Assert.Empty(merged.GetExtensionMethodContainers(" "));
        Assert.Empty(merged.GetExtensionMethodContainers("Missing"));
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void NamespaceMemberDiscovery_AcceptsProviderFactsWithoutBindingAttributes(bool merge, bool promote)
    {
        var compilation = Compilation.Create("test",
            [SyntaxTree.ParseText("namespace Shared { class Marker {} }")], TestMetadataReferences.Default);
        var sourceNamespace = compilation.GetTypeByMetadataName("Shared.Marker")!.ContainingNamespace!;
        var container = new MemberContainer(compilation, sourceNamespace, promote);
        var value = new SourceMethodSymbol("Value", compilation.GetSpecialType(SpecialType.System_Int32), [],
            container, container, sourceNamespace, [], [], isStatic: true);
        _ = new SourceMethodSymbol("Instance", compilation.GetSpecialType(SpecialType.System_Int32), [],
            container, container, sourceNamespace, [], [], isStatic: false);
        INamespaceSymbol provider = new ExtensionNamespace("", [container, container], exposeContainers: true);
        if (merge)
            provider = new MergedNamespaceSymbol([new MergedNamespaceSymbol([provider], null), sourceNamespace, provider], null);

        var all = compilation.GetNamespaceMembers(provider);
        var named = compilation.GetNamespaceMembers(provider, "Value");
        if (promote)
        {
            Assert.Same(value, Assert.Single(all));
            Assert.Same(value, Assert.Single(named));
        }
        else
        {
            Assert.Empty(all);
            Assert.Empty(named);
        }
        Assert.Empty(compilation.GetNamespaceMembers(provider, "Instance"));
        Assert.Empty(compilation.GetNamespaceMembers(provider, "Missing"));
        Assert.Empty(compilation.GetNamespaceMembers(provider, includeNamespaceMemberImports: false));
        Assert.Empty(compilation.GetNamespaceMembers(provider, "Value", includeNamespaceMemberImports: false));
    }

    // In-memory semantic provider: no PE metadata, source attributes or syntax.
    private sealed class MemberContainer(Compilation compilation, INamespaceSymbol owner, bool promote)
        : SourceNamedTypeSymbol("ProviderContainer", compilation.GetSpecialType(SpecialType.System_Object), TypeKind.Class,
            compilation.Assembly, null, owner, [], [], addAsMember: false), INamespaceMemberContainer
    {
        public bool IsNamespaceMemberContainer => promote;
        public override ImmutableArray<AttributeData> GetAttributes()
            => throw new InvalidOperationException("Namespace discovery must not bind provider attributes.");
    }

    private sealed class ExtensionNamespace(string methodName, ImmutableArray<INamedTypeSymbol> containers, bool exposeContainers = false)
        : Symbol(SymbolKind.Namespace, "Shared", null, null, null, [], []), INamespaceSymbol, INamespaceExtensionLookup
    {
        public bool IsNamespace => true;
        public bool IsType => false;
        public bool IsGlobalNamespace => false;
        public ImmutableArray<ISymbol> GetMembers() => exposeContainers ? containers.Cast<ISymbol>().ToImmutableArray() : [];
        public ImmutableArray<ISymbol> GetMembers(string name) => GetMembers().Where(member => member.Name == name).ToImmutableArray();
        public ITypeSymbol? LookupType(string name) => null;
        public INamespaceSymbol? LookupNamespace(string name) => null;
        public string ToMetadataName() => Name;
        public override void Accept(SymbolVisitor visitor) => visitor.VisitNamespace(this);
        public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitNamespace(this);
        public bool IsMemberDefined(string name, out ISymbol? symbol)
        {
            symbol = null;
            return false;
        }

        public ImmutableArray<INamedTypeSymbol> GetExtensionMethodContainers(string name)
            => name == methodName ? containers : [];
    }

    [Fact]
    public void Constructor_WithEmptyNamespaces_ThrowsArgumentException()
    {
        var ex = Assert.Throws<ArgumentException>(() => new MergedNamespaceSymbol([], null!));
        Assert.Equal("namespaces", ex.ParamName);
    }

    [Fact]
    public void Constructor_WithSingleNamespace_PreservesNamespaceName()
    {
        var compilation = Compilation.Create(
            "test",
            [],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var merged = new MergedNamespaceSymbol([compilation.GlobalNamespace], null!);

        Assert.Equal(compilation.GlobalNamespace.Name, merged.Name);
    }

    [Fact]
    public void Constructor_WithNullNamespaceEntries_IgnoresNulls()
    {
        var compilation = Compilation.Create(
            "test",
            [],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var merged = new MergedNamespaceSymbol([null!, compilation.GlobalNamespace], null!);

        Assert.Equal(compilation.GlobalNamespace.Name, merged.Name);
    }

    [Fact]
    public async Task GetSemanticModel_WhenSetupRunsConcurrently_DoesNotObserveHalfInitializedGlobalNamespace()
    {
        var tree = SyntaxTree.ParseText(
            """
            import System.*

            func Main() -> () {
                let value = 42
            }
            """,
            path: "/tmp/test.rav");

        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var tasks = Enumerable.Range(0, 8)
            .Select(_ => Task.Run(() => compilation.GetSemanticModel(tree)))
            .ToArray();

        await Task.WhenAll(tasks);

        foreach (var task in tasks)
            Assert.NotNull(task.Result);
    }
}
