using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class SymbolEqualityComparerTests
{
    [Fact]
    public void DefaultComparer_DistinguishesNullability()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var nullableInt = new NullableTypeSymbol(intType, null, null, null, []);

        var comparer = SymbolEqualityComparer.Default;
        Assert.False(comparer.Equals(intType, nullableInt));

        var dictionary = new Dictionary<ISymbol, int>(comparer)
        {
            [intType] = 1,
        };

        Assert.False(dictionary.ContainsKey(nullableInt));
    }

    [Fact]
    public void IgnoringNullabilityComparer_TreatsNullableAndUnderlyingAsEqual()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var nullableInt = new NullableTypeSymbol(intType, null, null, null, []);

        var comparer = SymbolEqualityComparer.IgnoringNullability;
        Assert.True(comparer.Equals(intType, nullableInt));

        var dictionary = new Dictionary<ISymbol, int>(comparer)
        {
            [intType] = 1,
        };

        Assert.True(dictionary.ContainsKey(nullableInt));
    }

    [Fact]
    public void Comparer_UnwrapsAliasSymbols()
    {
        const string source = """
            alias Text = System.String

            let value: Text = ""
            """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(tree);
        var identifier = tree
            .GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Last(id => id.Identifier.Text == "Text");

        var symbol = model.GetSymbolInfo(identifier).Symbol;
        var alias = Assert.IsAssignableFrom<IAliasSymbol>(symbol);

        var comparer = SymbolEqualityComparer.Default;
        Assert.True(comparer.Equals(alias, alias.UnderlyingSymbol));
        Assert.Equal(comparer.GetHashCode(alias.UnderlyingSymbol), comparer.GetHashCode(alias));

        var set = new HashSet<ISymbol>(comparer) { alias };
        Assert.Contains(alias.UnderlyingSymbol, set);
    }

    [Fact]
    public void Comparer_DistinguishesMetadataNameDifferences()
    {
        var comparer = SymbolEqualityComparer.Default;
        var explicitImplementation = new StubSymbol(SymbolKind.Method, "M", "IFoo.M");
        var ordinaryMethod = new StubSymbol(SymbolKind.Method, "M", "M");

        Assert.False(comparer.Equals(explicitImplementation, ordinaryMethod));

        var set = new HashSet<ISymbol>(comparer) { explicitImplementation };
        Assert.DoesNotContain(ordinaryMethod, set);
    }

    [Fact]
    public void Comparer_DistinguishesConstructedGenerics()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var listDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Collections.Generic.List`1"));

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var stringType = compilation.GetSpecialType(SpecialType.System_String);

        var listOfInt = Assert.IsAssignableFrom<INamedTypeSymbol>(listDefinition.Construct(intType));
        var listOfString = Assert.IsAssignableFrom<INamedTypeSymbol>(listDefinition.Construct(stringType));

        var comparer = SymbolEqualityComparer.Default;
        Assert.False(comparer.Equals(listOfInt, listOfString));

        var set = new HashSet<ISymbol>(comparer) { listOfInt };
        Assert.DoesNotContain(listOfString, set);
    }

    [Fact]
    public void Comparer_DistinguishesArrayElementTypes()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var stringType = compilation.GetSpecialType(SpecialType.System_String);

        var intArray = compilation.CreateArrayTypeSymbol(intType);
        var stringArray = compilation.CreateArrayTypeSymbol(stringType);

        var comparer = SymbolEqualityComparer.Default;
        Assert.False(comparer.Equals(intArray, stringArray));

        var set = new HashSet<ISymbol>(comparer) { intArray };
        Assert.DoesNotContain(stringArray, set);
    }

    private sealed class StubSymbol : ISymbol
    {
        public StubSymbol(
            SymbolKind kind,
            string name,
            string metadataName,
            ISymbol? containingSymbol = null,
            INamedTypeSymbol? containingType = null,
            INamespaceSymbol? containingNamespace = null)
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

        public ISymbol? ContainingSymbol { get; }

        public IAssemblySymbol? ContainingAssembly => ContainingNamespace?.ContainingAssembly;

        public IModuleSymbol? ContainingModule => ContainingNamespace?.ContainingModule;

        public INamedTypeSymbol? ContainingType { get; }

        public INamespaceSymbol? ContainingNamespace { get; }

        public ImmutableArray<Location> Locations { get; } = ImmutableArray<Location>.Empty;

        public Accessibility DeclaredAccessibility => Accessibility.Public;

        public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences { get; } = ImmutableArray<SyntaxReference>.Empty;

        public bool IsImplicitlyDeclared => true;

        public bool IsStatic => false;

        public ISymbol UnderlyingSymbol => this;

        public bool IsAlias => false;

        public ImmutableArray<AttributeData> GetAttributes() => ImmutableArray<AttributeData>.Empty;

        public bool Equals(ISymbol? other, SymbolEqualityComparer comparer) => ReferenceEquals(this, other);

        public bool Equals(ISymbol? other) => ReferenceEquals(this, other);

        public override bool Equals(object? obj) => ReferenceEquals(this, obj);

        public override int GetHashCode() => HashCode.Combine(Kind, Name, MetadataName);

        public void Accept(SymbolVisitor visitor) => visitor.DefaultVisit(this);

        public TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.DefaultVisit(this);
    }
}
