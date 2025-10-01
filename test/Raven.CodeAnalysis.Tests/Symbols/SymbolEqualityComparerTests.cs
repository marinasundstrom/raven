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
    public void Comparer_EquatesConstructedGenericsWithSameArguments()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var listDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Collections.Generic.List`1"));

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);

        var first = Assert.IsAssignableFrom<INamedTypeSymbol>(listDefinition.Construct(intType));
        var second = Assert.IsAssignableFrom<INamedTypeSymbol>(listDefinition.Construct(intType));

        var comparer = SymbolEqualityComparer.Default;

        Assert.True(comparer.Equals(first, second));
        Assert.Equal(comparer.GetHashCode(first), comparer.GetHashCode(second));

        var set = new HashSet<ISymbol>(comparer) { first };
        Assert.Contains(second, set);
    }

    [Fact]
    public void Comparer_EquatesOpenGenericDefinitions()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var listDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Collections.Generic.List`1"));

        var openInstance = Assert.IsAssignableFrom<INamedTypeSymbol>(
            listDefinition.Construct(listDefinition.TypeParameters.Cast<ITypeSymbol>().ToArray()));

        var comparer = SymbolEqualityComparer.Default;

        Assert.True(comparer.Equals(listDefinition, openInstance));
        Assert.Equal(comparer.GetHashCode(listDefinition), comparer.GetHashCode(openInstance));
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

    [Fact]
    public void Comparer_DistinguishesTypeKinds()
    {
        var comparer = SymbolEqualityComparer.Default;

        var delegateSymbol = new StubTypeSymbol(TypeKind.Delegate);
        var classSymbol = new StubTypeSymbol(TypeKind.Class);

        Assert.False(comparer.Equals(delegateSymbol, classSymbol));

        var set = new HashSet<ISymbol>(comparer) { delegateSymbol };
        Assert.DoesNotContain(classSymbol, set);
    }

    private class StubSymbol : ISymbol
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

    private sealed class StubTypeSymbol : StubSymbol, ITypeSymbol
    {
        public StubTypeSymbol(TypeKind typeKind)
            : base(SymbolKind.Type, "Processor", "Processor")
        {
            TypeKind = typeKind;
        }

        public INamedTypeSymbol? BaseType => null;

        public ITypeSymbol? OriginalDefinition => this;

        public SpecialType SpecialType => SpecialType.None;

        public TypeKind TypeKind { get; }

        public ImmutableArray<INamedTypeSymbol> Interfaces => ImmutableArray<INamedTypeSymbol>.Empty;

        public ImmutableArray<INamedTypeSymbol> AllInterfaces => ImmutableArray<INamedTypeSymbol>.Empty;

        public bool IsNamespace => false;

        public bool IsType => true;

        public ImmutableArray<ISymbol> GetMembers() => ImmutableArray<ISymbol>.Empty;

        public ImmutableArray<ISymbol> GetMembers(string name) => ImmutableArray<ISymbol>.Empty;

        public ITypeSymbol? LookupType(string name) => null;

        public bool IsMemberDefined(string name, out ISymbol? symbol)
        {
            symbol = null;
            return false;
        }
    }
}
