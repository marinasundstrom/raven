using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class SymbolRegistrationContractTests
{
    [Fact]
    public void NamespaceRegistration_UsesContainerContractAfterIdentityInitialization()
    {
        var owner = new RecordingNamespace("Owner");
        var child = new RecordingNamespace("Child", owner);

        Assert.Same(child, Assert.Single(owner.Members));
        Assert.Equal((SymbolKind.Namespace, "Child"), Assert.Single(owner.RegisteredIdentities));
    }

    [Fact]
    public void OptedOutNamespace_CanBeRegisteredExplicitly()
    {
        var owner = new RecordingNamespace("Owner");
        var child = new RecordingNamespace("Child", owner, addAsMember: false);

        Assert.Empty(owner.Members);
        Assert.True(child.AddAsMember(null, owner));
        Assert.Same(child, Assert.Single(owner.Members));
    }

    [Fact]
    public void FileScopedAccess_PreservesOrdinalPathsAndNullRules()
    {
        var symbol = new RecordingNamespace("Scoped");
        Assert.True(symbol.IsAccessibleFromFile(null));
        symbol.MarkFileScoped("source.rvn");
        Assert.True(symbol.IsAccessibleFromFile("source.rvn"));
        Assert.False(symbol.IsAccessibleFromFile("SOURCE.rvn"));
        Assert.False(symbol.IsAccessibleFromFile(null));
        symbol.MarkFileScoped(null);
        Assert.True(symbol.IsAccessibleFromFile(string.Empty));
        Assert.False(symbol.IsAccessibleFromFile(null));
    }

    [Fact]
    public void Defaults_PreserveIdentityAndOptionalMetadata()
    {
        var symbol = new RecordingNamespace("Root");
        Assert.Same(symbol, symbol.UnderlyingSymbol);
        Assert.True(symbol.Equals(symbol));
        Assert.False(symbol.Equals((Symbol?)null));
        Assert.Null(symbol.ContainingSymbol);
        Assert.Null(symbol.ContainingAssembly);
        Assert.Null(symbol.ContainingModule);
        Assert.Null(symbol.GetDocumentationComment());
        Assert.Empty(symbol.GetAttributes());
        Assert.Empty(symbol.Locations);
        Assert.Empty(symbol.DeclaringSyntaxReferences);
        Assert.Equal(Accessibility.NotApplicable, symbol.DeclaredAccessibility);
        Assert.True(symbol.CanBeReferencedByName);
        Assert.False(symbol.IsStatic);
        Assert.False(symbol.IsAlias);
    }

    private sealed class RecordingNamespace : SymbolBase, NamespaceSymbol, SymbolMemberContainer
    {
        public RecordingNamespace(string name, NamespaceSymbol? owner = null, bool addAsMember = true)
            : base(SymbolKind.Namespace, name, owner, null, owner, [], [],
                Accessibility.NotApplicable, addAsMember)
        {
        }

        public List<Symbol> Members { get; } = [];
        public List<(SymbolKind Kind, string Name)> RegisteredIdentities { get; } = [];
        public bool IsNamespace => true;
        public bool IsType => false;
        public bool IsGlobalNamespace => ContainingNamespace is null;

        void SymbolMemberContainer.AddMember(Symbol member)
        {
            Members.Add(member);
            RegisteredIdentities.Add((member.Kind, member.Name));
        }

        public ImmutableArray<Symbol> GetMembers() => [.. Members];
        public ImmutableArray<Symbol> GetMembers(string name) => [.. Members.Where(member => member.Name == name)];
        public TypeSymbol? LookupType(string name) => null;
        public NamespaceSymbol? LookupNamespace(string name) => null;
        public string ToMetadataName() => Name;
        public bool IsMemberDefined(string name, out Symbol? symbol)
        {
            symbol = Members.FirstOrDefault(member => member.Name == name);
            return symbol is not null;
        }

        public override void Accept(SymbolVisitor visitor) => visitor.VisitNamespace(this);
        public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitNamespace(this);
    }
}
