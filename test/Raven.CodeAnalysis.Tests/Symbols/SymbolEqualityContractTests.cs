using System.Collections.Immutable;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "Shared")]
public class SymbolEqualityContractTests
{
    [Fact]
    public void VisitorsDispatchThroughPortOwnedSymbolsAndGeneratedMethods()
    {
        var symbol = new StubTypeSymbol(TypeKind.Class);
        var counter = new CountingVisitor();
        counter.Visit(symbol);
        counter.VisitType(symbol);
        counter.Visit(null!);
        Assert.Equal(2, counter.Count);
        var names = new NameVisitor();
        Assert.Equal(symbol.Name, names.Visit(symbol));
        Assert.Equal(symbol.Name, names.VisitType(symbol));
        Assert.Null(names.Visit(null));
    }

    private sealed class CountingVisitor : SymbolVisitor
    {
        public int Count { get; private set; }
        public override void DefaultVisit(Symbol symbol) => Count++;
    }

    private sealed class NameVisitor : SymbolVisitor<string>
    {
        public override string DefaultVisit(Symbol symbol) => symbol.Name;
    }

    [Fact]
    public void NullAndReferenceIdentityAreStable()
    {
        var symbol = new StubSymbol(SymbolKind.Method, "M", "M");
        Assert.True(SymbolEqualityComparer.Default.Equals(null, null));
        Assert.False(SymbolEqualityComparer.Default.Equals(symbol, null));
        Assert.False(SymbolEqualityComparer.Default.Equals(null, symbol));
        Assert.True(SymbolEqualityComparer.Default.Equals(symbol, symbol));
    }

    [Fact]
    public void NullableReferenceIdentityHonorsComparerMode()
    {
        var plain = new StubTypeSymbol(TypeKind.Class);
        var first = new NullableIdentity(plain);
        var second = new NullableIdentity(new StubTypeSymbol(TypeKind.Class));
        Assert.True(SymbolEqualityComparer.Default.Equals(first, second));
        Assert.Equal(SymbolEqualityComparer.Default.GetHashCode(first), SymbolEqualityComparer.Default.GetHashCode(second));
        Assert.False(SymbolEqualityComparer.Default.Equals(first, plain));
        Assert.True(SymbolEqualityComparer.IgnoringNullability.Equals(first, plain));
        Assert.Equal(SymbolEqualityComparer.IgnoringNullability.GetHashCode(first), SymbolEqualityComparer.IgnoringNullability.GetHashCode(plain));
    }

    [Theory]
    [InlineData(0, true)]
    [InlineData(1, false)]
    [InlineData(2, false)]
    [InlineData(3, false)]
    public void StructuralFunctionIdentityIncludesParameterTypeRefKindAndReturnType(int change, bool equal)
    {
        var parameter = new StubTypeSymbol(TypeKind.Class);
        var other = new StubTypeSymbol(TypeKind.Interface);
        var first = new FunctionIdentity([parameter], [RefKind.None], parameter);
        var second = new FunctionIdentity([change == 1 ? other : parameter],
            [change == 2 ? RefKind.Ref : RefKind.None], change == 3 ? other : parameter);
        Assert.Equal(equal, SymbolEqualityComparer.Default.Equals(first, second));
        Assert.Equal(equal, SymbolEqualityComparer.Default.Equals(second, first));
        if (equal)
            Assert.Equal(SymbolEqualityComparer.Default.GetHashCode(first), SymbolEqualityComparer.Default.GetHashCode(second));
    }

    private sealed class NullableIdentity(TypeSymbol underlying) : StubTypeSymbol(TypeKind.Nullable), INullableTypeIdentity
    {
        public TypeSymbol UnderlyingType => underlying;
    }

    private sealed class FunctionIdentity(ImmutableArray<TypeSymbol> parameters, ImmutableArray<RefKind> refKinds, TypeSymbol result)
        : StubTypeSymbol(TypeKind.Delegate), IStructuralFunctionIdentity
    {
        public ImmutableArray<TypeSymbol> ParameterTypes => parameters;
        public ImmutableArray<RefKind> ParameterRefKinds => refKinds;
        public TypeSymbol ReturnType => result;
    }

    [Fact]
    public void Comparer_DistinguishesMetadataNameDifferences()
    {
        var comparer = SymbolEqualityComparer.Default;
        var explicitImplementation = new StubSymbol(SymbolKind.Method, "M", "IFoo.M");
        var ordinaryMethod = new StubSymbol(SymbolKind.Method, "M", "M");

        Assert.False(comparer.Equals(explicitImplementation, ordinaryMethod));

        var set = new HashSet<Symbol>(comparer) { explicitImplementation };
        Assert.DoesNotContain(ordinaryMethod, set);
    }

    [Fact]
    public void Comparer_DistinguishesTypeKinds()
    {
        var comparer = SymbolEqualityComparer.Default;

        var delegateSymbol = new StubTypeSymbol(TypeKind.Delegate);
        var classSymbol = new StubTypeSymbol(TypeKind.Class);

        Assert.False(comparer.Equals(delegateSymbol, classSymbol));

        var set = new HashSet<Symbol>(comparer) { delegateSymbol };
        Assert.DoesNotContain(classSymbol, set);
    }

    [Fact]
    public void Comparer_HandlesCyclicContainingSymbols()
    {
        var comparer = SymbolEqualityComparer.Default;

        var first = new CyclicSymbol(SymbolKind.Method, "Evaluate", "Evaluate");
        first.SetContainingSymbol(first);

        var second = new CyclicSymbol(SymbolKind.Method, "Evaluate", "Evaluate");
        second.SetContainingSymbol(second);

        Assert.True(comparer.Equals(first, second));

        var set = new HashSet<Symbol>(comparer) { first };
        Assert.Contains(second, set);

        var firstOuter = new CyclicSymbol(SymbolKind.Method, "Outer", "Outer");
        var firstInner = new CyclicSymbol(SymbolKind.Method, "Inner", "Inner");
        firstOuter.SetContainingSymbol(firstInner);
        firstInner.SetContainingSymbol(firstOuter);

        var secondOuter = new CyclicSymbol(SymbolKind.Method, "Outer", "Outer");
        var secondInner = new CyclicSymbol(SymbolKind.Method, "Inner", "Inner");
        secondOuter.SetContainingSymbol(secondInner);
        secondInner.SetContainingSymbol(secondOuter);

        Assert.True(comparer.Equals(firstOuter, secondOuter));

        set.Add(firstOuter);
        Assert.Contains(secondOuter, set);
    }

    private class StubSymbol : Symbol
    {
        public StubSymbol(
            SymbolKind kind,
            string name,
            string metadataName,
            Symbol? containingSymbol = null,
            NamedTypeSymbol? containingType = null,
            NamespaceSymbol? containingNamespace = null)
        {
            Kind = kind;
            Name = name;
            MetadataName = metadataName;
            ContainingSymbol = containingSymbol;
            ContainingType = containingType;
            ContainingNamespace = containingNamespace;
        }

        public SymbolKind Kind { get; }

        public string Name { get; }

        public string MetadataName { get; }

        public Symbol? ContainingSymbol { get; }

        public AssemblySymbol? ContainingAssembly => ContainingNamespace?.ContainingAssembly;

        public ModuleSymbol? ContainingModule => ContainingNamespace?.ContainingModule;

        public NamedTypeSymbol? ContainingType { get; }

        public NamespaceSymbol? ContainingNamespace { get; }

        public ImmutableArray<Location> Locations { get; } = ImmutableArray<Location>.Empty;

        public Accessibility DeclaredAccessibility => Accessibility.Public;

        public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences { get; } = ImmutableArray<SyntaxReference>.Empty;

        public bool IsImplicitlyDeclared => true;

        public bool IsStatic => false;

        public Symbol UnderlyingSymbol => this;

        public bool IsAlias => false;

        public ImmutableArray<AttributeData> GetAttributes() => ImmutableArray<AttributeData>.Empty;

        public bool Equals(Symbol? other, SymbolEqualityComparer comparer) => ReferenceEquals(this, other);

        public bool Equals(Symbol? other) => ReferenceEquals(this, other);

        public override bool Equals(object? obj) => ReferenceEquals(this, obj);

        public override int GetHashCode() => HashCode.Combine(Kind, Name, MetadataName);

        public void Accept(SymbolVisitor visitor) => visitor.DefaultVisit(this);

        public TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.DefaultVisit(this);
    }

    private class StubTypeSymbol : StubSymbol, TypeSymbol
    {
        public StubTypeSymbol(TypeKind typeKind)
            : base(SymbolKind.Type, "Processor", "Processor")
        {
            TypeKind = typeKind;
        }

        public NamedTypeSymbol? BaseType => null;

        public TypeSymbol? OriginalDefinition => this;

        public SpecialType SpecialType => SpecialType.None;

        public TypeKind TypeKind { get; }

        public ImmutableArray<NamedTypeSymbol> Interfaces => ImmutableArray<NamedTypeSymbol>.Empty;

        public ImmutableArray<NamedTypeSymbol> AllInterfaces => ImmutableArray<NamedTypeSymbol>.Empty;

        public bool IsNamespace => false;

        public bool IsType => true;

        public ImmutableArray<Symbol> GetMembers() => ImmutableArray<Symbol>.Empty;

        public ImmutableArray<Symbol> GetMembers(string name) => ImmutableArray<Symbol>.Empty;

        public TypeSymbol? LookupType(string name) => null;

        public bool IsMemberDefined(string name, out Symbol? symbol)
        {
            symbol = null;
            return false;
        }
    }

    private sealed class CyclicSymbol : Symbol
    {
        public CyclicSymbol(SymbolKind kind, string name, string metadataName)
        {
            Kind = kind;
            Name = name;
            MetadataName = metadataName;
        }

        public SymbolKind Kind { get; }

        public string Name { get; }

        public string MetadataName { get; }

        public Symbol? ContainingSymbol { get; private set; }

        public AssemblySymbol? ContainingAssembly => ContainingNamespace?.ContainingAssembly;

        public ModuleSymbol? ContainingModule => ContainingNamespace?.ContainingModule;

        public NamedTypeSymbol? ContainingType => null;

        public NamespaceSymbol? ContainingNamespace => null;

        public ImmutableArray<Location> Locations { get; } = ImmutableArray<Location>.Empty;

        public Accessibility DeclaredAccessibility => Accessibility.Public;

        public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences { get; } = ImmutableArray<SyntaxReference>.Empty;

        public bool IsImplicitlyDeclared => true;

        public bool IsStatic => false;

        public bool IsAlias => false;

        public Symbol UnderlyingSymbol => this;

        public void SetContainingSymbol(Symbol symbol)
        {
            ContainingSymbol = symbol;
        }

        public ImmutableArray<AttributeData> GetAttributes() => ImmutableArray<AttributeData>.Empty;

        public bool Equals(Symbol? other, SymbolEqualityComparer comparer) => ReferenceEquals(this, other);

        public bool Equals(Symbol? other) => ReferenceEquals(this, other);

        public override bool Equals(object? obj) => ReferenceEquals(this, obj);

        public override int GetHashCode() => HashCode.Combine(Kind, Name, MetadataName);

        public void Accept(SymbolVisitor visitor) => visitor.DefaultVisit(this);

        public TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.DefaultVisit(this);
    }
}
