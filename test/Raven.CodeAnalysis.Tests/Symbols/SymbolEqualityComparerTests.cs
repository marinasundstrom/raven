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
    public void IgnoringNullabilityComparer_DistinguishesNullableValueTypes()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var nullableInt = new NullableTypeSymbol(intType, null, null, null, []);

        var comparer = SymbolEqualityComparer.IgnoringNullability;
        Assert.False(comparer.Equals(intType, nullableInt));

        var dictionary = new Dictionary<ISymbol, int>(comparer)
        {
            [intType] = 1,
        };

        Assert.False(dictionary.ContainsKey(nullableInt));
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
    public void Comparer_DistinguishesParametersWithSameType()
    {
        var comparer = SymbolEqualityComparer.Default;
        var containing = new StubSymbol(SymbolKind.Method, "Invoke", "Invoke");
        var parameterType = new StubTypeSymbol(TypeKind.Struct);

        var first = new StubParameterSymbol("a", containing, parameterType);
        var second = new StubParameterSymbol("b", containing, parameterType);

        Assert.False(comparer.Equals(first, second));

        var set = new HashSet<ISymbol>(comparer) { first };
        Assert.DoesNotContain(second, set);
    }

    [Fact]
    public void Comparer_PreservesEqualityAcrossCompilations()
    {
        const string source = """
class C {
    class Nested {}

    M() -> unit {
        let number = 42;
    }
}
""";

        var tree1 = SyntaxTree.ParseText(source);
        var tree2 = SyntaxTree.ParseText(source);

        var compilation1 = Compilation.Create(
            "test",
            [tree1],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var compilation2 = Compilation.Create(
            "test",
            [tree2],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var model1 = compilation1.GetSemanticModel(tree1);
        var model2 = compilation2.GetSemanticModel(tree2);

        var root1 = tree1.GetRoot();
        var root2 = tree2.GetRoot();

        var classSyntax1 = Assert.IsAssignableFrom<ClassDeclarationSyntax>(Assert.Single(root1.Members));
        var classSyntax2 = Assert.IsAssignableFrom<ClassDeclarationSyntax>(Assert.Single(root2.Members));

        var class1 = Assert.IsAssignableFrom<INamedTypeSymbol>(model1.GetDeclaredSymbol(classSyntax1));
        var class2 = Assert.IsAssignableFrom<INamedTypeSymbol>(model2.GetDeclaredSymbol(classSyntax2));

        var comparer = SymbolEqualityComparer.Default;

        Assert.True(comparer.Equals(compilation1.Assembly, compilation2.Assembly));
        Assert.True(comparer.Equals(compilation1.GlobalNamespace, compilation2.GlobalNamespace));
        Assert.True(comparer.Equals(compilation1.Assembly.Modules.Single(), compilation2.Assembly.Modules.Single()));

        Assert.True(comparer.Equals(class1, class2));
        Assert.Equal(comparer.GetHashCode(class1), comparer.GetHashCode(class2));

        var classSet = new HashSet<ISymbol>(comparer) { class1 };
        Assert.Contains(class2, classSet);

        var nested1 = Assert.IsAssignableFrom<INamedTypeSymbol>(class1.GetMembers("Nested").Single());
        var nested2 = Assert.IsAssignableFrom<INamedTypeSymbol>(class2.GetMembers("Nested").Single());

        Assert.True(comparer.Equals(nested1, nested2));
        Assert.Equal(comparer.GetHashCode(nested1), comparer.GetHashCode(nested2));

        var nestedSet = new HashSet<ISymbol>(comparer) { nested1 };
        Assert.Contains(nested2, nestedSet);

        var method1 = Assert.IsAssignableFrom<IMethodSymbol>(class1.GetMembers("M").Single());
        var method2 = Assert.IsAssignableFrom<IMethodSymbol>(class2.GetMembers("M").Single());

        Assert.True(comparer.Equals(method1, method2));
        Assert.Equal(comparer.GetHashCode(method1), comparer.GetHashCode(method2));

        var methodSet = new HashSet<ISymbol>(comparer) { method1 };
        Assert.Contains(method2, methodSet);

        var string1 = compilation1.GetSpecialType(SpecialType.System_String);
        var string2 = compilation2.GetSpecialType(SpecialType.System_String);

        Assert.True(comparer.Equals(string1, string2));
        Assert.Equal(comparer.GetHashCode(string1), comparer.GetHashCode(string2));

        var metadataSet = new HashSet<ISymbol>(comparer) { string1 };
        Assert.Contains(string2, metadataSet);

        var declarator1 = root1.DescendantNodes().OfType<VariableDeclaratorSyntax>().First();
        var declarator2 = root2.DescendantNodes().OfType<VariableDeclaratorSyntax>().First();

        var local1 = Assert.IsAssignableFrom<ILocalSymbol>(model1.GetDeclaredSymbol(declarator1));
        var local2 = Assert.IsAssignableFrom<ILocalSymbol>(model2.GetDeclaredSymbol(declarator2));

        Assert.True(comparer.Equals(local1, local2));
        Assert.Equal(comparer.GetHashCode(local1), comparer.GetHashCode(local2));

        var localSet = new HashSet<ISymbol>(comparer) { local1 };
        Assert.Contains(local2, localSet);
    }

    [Fact]
    public void Comparer_DistinguishesFieldTypesAcrossCompilations()
    {
        const string template = """
class Sample {{
    value: {0} = {1};
}}
""";

        var tree1 = SyntaxTree.ParseText(string.Format(template, "int", "0"));
        var tree2 = SyntaxTree.ParseText(string.Format(template, "string", "\"\""));

        var compilation1 = Compilation.Create(
            "test",
            [tree1],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var compilation2 = Compilation.Create(
            "test",
            [tree2],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var model1 = compilation1.GetSemanticModel(tree1);
        var model2 = compilation2.GetSemanticModel(tree2);

        var root1 = tree1.GetRoot();
        var root2 = tree2.GetRoot();

        var fieldSyntax1 = root1.DescendantNodes().OfType<FieldDeclarationSyntax>().Single();
        var fieldSyntax2 = root2.DescendantNodes().OfType<FieldDeclarationSyntax>().Single();

        var declarator1 = fieldSyntax1.Declaration.Declarators.Single();
        var declarator2 = fieldSyntax2.Declaration.Declarators.Single();

        var field1 = Assert.IsAssignableFrom<IFieldSymbol>(model1.GetDeclaredSymbol(declarator1));
        var field2 = Assert.IsAssignableFrom<IFieldSymbol>(model2.GetDeclaredSymbol(declarator2));

        var comparer = SymbolEqualityComparer.Default;

        Assert.False(comparer.Equals(field1, field2));
        Assert.NotEqual(comparer.GetHashCode(field1), comparer.GetHashCode(field2));

        var set = new HashSet<ISymbol>(comparer) { field1 };
        Assert.DoesNotContain(field2, set);
    }

    [Fact]
    public void Comparer_DistinguishesPropertyTypesAcrossCompilations()
    {
        const string template = """
class Sample {{
    public Value: {0} {{ get; }}
}}
""";

        var tree1 = SyntaxTree.ParseText(string.Format(template, "int"));
        var tree2 = SyntaxTree.ParseText(string.Format(template, "string"));

        var compilation1 = Compilation.Create(
            "test",
            [tree1],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var compilation2 = Compilation.Create(
            "test",
            [tree2],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var model1 = compilation1.GetSemanticModel(tree1);
        var model2 = compilation2.GetSemanticModel(tree2);

        var propertySyntax1 = tree1.GetRoot().DescendantNodes().OfType<PropertyDeclarationSyntax>().Single();
        var propertySyntax2 = tree2.GetRoot().DescendantNodes().OfType<PropertyDeclarationSyntax>().Single();

        var property1 = Assert.IsAssignableFrom<IPropertySymbol>(model1.GetDeclaredSymbol(propertySyntax1));
        var property2 = Assert.IsAssignableFrom<IPropertySymbol>(model2.GetDeclaredSymbol(propertySyntax2));

        var comparer = SymbolEqualityComparer.Default;

        Assert.False(comparer.Equals(property1, property2));
        Assert.NotEqual(comparer.GetHashCode(property1), comparer.GetHashCode(property2));

        var set = new HashSet<ISymbol>(comparer) { property1 };
        Assert.DoesNotContain(property2, set);
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
    public void Comparer_RecognizesMetadataDefinitions()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var listDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Collections.Generic.List`1"));

        var constructedFrom = Assert.IsAssignableFrom<INamedTypeSymbol>(listDefinition.ConstructedFrom);
        var originalDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(listDefinition.OriginalDefinition);

        Assert.Same(listDefinition, constructedFrom);
        Assert.Same(listDefinition, originalDefinition);

        var comparer = SymbolEqualityComparer.Default;
        Assert.True(comparer.Equals(listDefinition, constructedFrom));
        Assert.True(comparer.Equals(listDefinition, originalDefinition));
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

    [Fact]
    public void Comparer_HandlesCyclicContainingSymbols()
    {
        var comparer = SymbolEqualityComparer.Default;

        var first = new CyclicSymbol(SymbolKind.Method, "Evaluate", "Evaluate");
        first.SetContainingSymbol(first);

        var second = new CyclicSymbol(SymbolKind.Method, "Evaluate", "Evaluate");
        second.SetContainingSymbol(second);

        Assert.True(comparer.Equals(first, second));

        var set = new HashSet<ISymbol>(comparer) { first };
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

    private sealed class CyclicSymbol : ISymbol
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

        public ISymbol? ContainingSymbol { get; private set; }

        public IAssemblySymbol? ContainingAssembly => ContainingNamespace?.ContainingAssembly;

        public IModuleSymbol? ContainingModule => ContainingNamespace?.ContainingModule;

        public INamedTypeSymbol? ContainingType => null;

        public INamespaceSymbol? ContainingNamespace => null;

        public ImmutableArray<Location> Locations { get; } = ImmutableArray<Location>.Empty;

        public Accessibility DeclaredAccessibility => Accessibility.Public;

        public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences { get; } = ImmutableArray<SyntaxReference>.Empty;

        public bool IsImplicitlyDeclared => true;

        public bool IsStatic => false;

        public bool IsAlias => false;

        public ISymbol UnderlyingSymbol => this;

        public void SetContainingSymbol(ISymbol symbol)
        {
            ContainingSymbol = symbol;
        }

        public ImmutableArray<AttributeData> GetAttributes() => ImmutableArray<AttributeData>.Empty;

        public bool Equals(ISymbol? other, SymbolEqualityComparer comparer) => ReferenceEquals(this, other);

        public bool Equals(ISymbol? other) => ReferenceEquals(this, other);

        public override bool Equals(object? obj) => ReferenceEquals(this, obj);

        public override int GetHashCode() => HashCode.Combine(Kind, Name, MetadataName);

        public void Accept(SymbolVisitor visitor) => visitor.DefaultVisit(this);

        public TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.DefaultVisit(this);
    }

    private sealed class StubParameterSymbol : StubSymbol, IParameterSymbol
    {
        public StubParameterSymbol(string name, ISymbol containingSymbol, ITypeSymbol type, RefKind refKind = RefKind.None)
            : base(SymbolKind.Parameter, name, name, containingSymbol)
        {
            Type = type;
            RefKind = refKind;
        }

        public ITypeSymbol Type { get; }

        public bool IsParams => false;

        public RefKind RefKind { get; }

        public bool IsMutable => false;

        public bool HasExplicitDefaultValue => false;

        public object? ExplicitDefaultValue => null;
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
