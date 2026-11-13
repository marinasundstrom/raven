using System.Collections.Immutable;

using Raven.CodeAnalysis.Documentation;
using Raven.CodeAnalysis.Metadata;
using Raven.CodeAnalysis.Symbols;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class MergedNamespaceContractTests
{
    [Fact]
    public void ConstructorRejectsNullAndEmptyInputsAndFlattensNestedMerges()
    {
        Assert.Throws<ArgumentNullException>(() => new MergedNamespaceSymbol(null!, null));
        Assert.Throws<ArgumentException>(() => new MergedNamespaceSymbol([], null));
        var one = new Namespace("");
        var two = new Namespace("");
        var merged = new MergedNamespaceSymbol([new MergedNamespaceSymbol([one], null), null!, two], null);
        Assert.Equal(new[] { one, two }, merged.GetMergedNamespaces());
        Assert.True(merged.IsGlobalNamespace);
        Assert.Equal("", merged.ToMetadataName());
        Assert.Equal("<global>", merged.ToString());
    }

    [Fact]
    public void MembersMergeSameNamedNamespacesAndDeduplicateOtherSymbols()
    {
        var member = Make<Symbol>(SymbolKind.Method, "Shared");
        Set(member, "Equals", (Func<object?[]?, object?>)(args => ReferenceEquals(member, args![0])));
        var firstChild = new Namespace("Child");
        var secondChild = new Namespace("Child");
        var merged = new MergedNamespaceSymbol([
            new Namespace("", [member, firstChild]), new Namespace("", [member, secondChild])
        ], null);
        var all = merged.GetMembers();
        Assert.Equal(2, all.Length);
        Assert.Same(member, Assert.Single(all, item => item.Name == "Shared"));
        var child = Assert.IsType<MergedNamespaceSymbol>(Assert.Single(merged.GetMembers("Child")));
        Assert.Same(merged, child.ContainingNamespace);
        Assert.Equal(new[] { firstChild, secondChild }, child.GetMergedNamespaces());
        Assert.Same(member, Assert.Single(merged.GetMembers("Shared")));
        Assert.True(merged.IsMemberDefined("Shared", out var found));
        Assert.Same(member, found);
        Assert.False(merged.IsMemberDefined("Missing", out found));
        Assert.Null(found);
        Assert.Null(merged.LookupNamespace("Missing"));
        Assert.IsType<MergedNamespaceSymbol>(merged.LookupNamespace("Child"));
    }

    [Fact]
    public void ParentNamespacesAreInferredAndExplicitParentsArePreserved()
    {
        var firstParent = new Namespace("");
        var secondParent = new Namespace("");
        var first = new Namespace("Child", parent: firstParent);
        var second = new Namespace("Child", parent: secondParent, module: true);
        var merged = new MergedNamespaceSymbol([first, second], null);
        Assert.IsType<MergedNamespaceSymbol>(merged.ContainingNamespace);
        Assert.True(merged.IsModule);
        Assert.False(merged.IsGlobalNamespace);
        Assert.Equal("Child", merged.ToMetadataName());
        Assert.Equal("Child", merged.ToString());
        Assert.Same(firstParent, new MergedNamespaceSymbol([first], null).ContainingNamespace);
        Assert.Same(secondParent, new MergedNamespaceSymbol([first], secondParent).ContainingNamespace);
    }

    [Fact]
    public void TypeLookupAndExtensionProvidersPreserveSelectionAndOrdering()
    {
        var first = Type("First");
        var second = Type("Second");
        var merged = new MergedNamespaceSymbol([
            new Namespace("", [first], extensions: [first, first]),
            new Namespace("", [first, second], extensions: [first, second])
        ], null);
        Assert.Same(first, merged.LookupType("First"));
        Assert.Null(merged.LookupType("Missing"));
        Assert.Equal(new[] { first, second }, merged.GetExtensionMethodContainers("Call"));
        Assert.Empty(merged.GetExtensionMethodContainers(" "));
        Assert.Empty(merged.GetExtensionMethodContainers("Missing"));
    }

    [Fact]
    public void DocumentationMergesInProviderOrderAndPreservesSingleComments()
    {
        var first = DocumentationComment.Create(DocumentationFormat.Markdown, "First");
        var second = DocumentationComment.Create(DocumentationFormat.Markdown, "Second");
        Assert.Null(new MergedNamespaceSymbol([new Namespace("")], null).GetDocumentationComment());
        Assert.Same(first, new MergedNamespaceSymbol([new Namespace("", comment: first)], null).GetDocumentationComment());
        var merged = new MergedNamespaceSymbol([new Namespace("", comment: first), new Namespace("", comment: second)], null);
        Assert.Equal(DocumentationComment.Merge(DocumentationFormat.Markdown, [first, second]), merged.GetDocumentationComment());
    }

    [Fact]
    public void GeneratedVisitorsDispatchAsNamespaces()
    {
        var merged = new MergedNamespaceSymbol([new Namespace("")], null);
        Assert.Equal("namespace", merged.Accept(new Visitor()));
    }

    private sealed class Visitor : SymbolVisitor<string>
    {
        public override string DefaultVisit(Symbol symbol) => "default";
        public override string VisitNamespace(NamespaceSymbol symbol) => "namespace";
    }

    private static NamedTypeSymbol Type(string name)
    {
        var type = Make<NamedTypeSymbol>(SymbolKind.Type, name);
        Set(type, "get_TypeArguments", ImmutableArray<TypeSymbol>.Empty);
        Set(type, "get_TypeParameters", ImmutableArray<TypeParameterSymbol>.Empty);
        Set(type, "get_Arity", 0);
        return type;
    }

    private sealed class Namespace(string name, ImmutableArray<Symbol> members = default,
        NamespaceSymbol? parent = null, bool module = false, ImmutableArray<NamedTypeSymbol> extensions = default, DocumentationComment? comment = null)
        : SymbolBase(SymbolKind.Namespace, name, parent, null, parent, [], []), NamespaceSymbol, INamespaceExtensionLookup
    {
        public bool IsModule => module;
        public bool IsNamespace => true;
        public bool IsType => false;
        public bool IsGlobalNamespace => Name.Length == 0;
        public ImmutableArray<Symbol> GetMembers() => members.IsDefault ? [] : members;
        public ImmutableArray<Symbol> GetMembers(string name) => GetMembers().Where(item => item.Name == name).ToImmutableArray();
        public NamespaceSymbol? LookupNamespace(string name) => GetMembers(name).OfType<NamespaceSymbol>().FirstOrDefault();
        public TypeSymbol? LookupType(string name) => GetMembers(name).OfType<TypeSymbol>().FirstOrDefault();
        public bool IsMemberDefined(string name, out Symbol? symbol)
        {
            symbol = GetMembers(name).FirstOrDefault();
            return symbol is not null;
        }
        public string ToMetadataName() => Name;
        public ImmutableArray<NamedTypeSymbol> GetExtensionMethodContainers(string name)
            => name == "Call" && !extensions.IsDefault ? extensions : [];
        public override DocumentationComment? GetDocumentationComment() => comment;
        public override void Accept(SymbolVisitor visitor) => visitor.DefaultVisit(this);
        public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.DefaultVisit(this);
    }
}
