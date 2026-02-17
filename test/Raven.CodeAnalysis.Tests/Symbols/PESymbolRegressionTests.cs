using System.Linq;
using System.Collections.Generic;

using Raven.CodeAnalysis.Semantics.Tests;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public sealed class PESymbolRegressionTests : CompilationTestBase
{
    [Fact]
    public void ConstructedMetadataMethod_AcrossCompilations_HasStableSymbolEquality()
    {
        var compilation1 = Compilation.Create("pe_method_eq_1", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);
        var compilation2 = Compilation.Create("pe_method_eq_2", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var intType1 = compilation1.GetSpecialType(SpecialType.System_Int32);
        var intType2 = compilation2.GetSpecialType(SpecialType.System_Int32);

        var listDefinition1 = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation1.GetTypeByMetadataName("System.Collections.Generic.List`1"));
        var listDefinition2 = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation2.GetTypeByMetadataName("System.Collections.Generic.List`1"));

        var listOfInt1 = Assert.IsAssignableFrom<INamedTypeSymbol>(listDefinition1.Construct(intType1));
        var listOfInt2 = Assert.IsAssignableFrom<INamedTypeSymbol>(listDefinition2.Construct(intType2));

        var add1 = Assert.Single(
            listOfInt1.GetMembers("Add")
                .OfType<IMethodSymbol>()
                .Where(method =>
                    method.MethodKind == MethodKind.Ordinary &&
                    method.Arity == 0 &&
                    method.Parameters.Length == 1 &&
                    SymbolEqualityComparer.Default.Equals(method.Parameters[0].Type, intType1)));

        var add2 = Assert.Single(
            listOfInt2.GetMembers("Add")
                .OfType<IMethodSymbol>()
                .Where(method =>
                    method.MethodKind == MethodKind.Ordinary &&
                    method.Arity == 0 &&
                    method.Parameters.Length == 1 &&
                    SymbolEqualityComparer.Default.Equals(method.Parameters[0].Type, intType2)));

        Assert.True(SymbolEqualityComparer.Default.Equals(add1, add2));
        Assert.Equal(SymbolEqualityComparer.Default.GetHashCode(add1), SymbolEqualityComparer.Default.GetHashCode(add2));
    }

    [Fact]
    public void ConstructedMetadataProperty_AcrossCompilations_HasStableSymbolEquality()
    {
        var compilation1 = Compilation.Create("pe_property_eq_1", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);
        var compilation2 = Compilation.Create("pe_property_eq_2", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var stringType1 = compilation1.GetSpecialType(SpecialType.System_String);
        var intType1 = compilation1.GetSpecialType(SpecialType.System_Int32);
        var stringType2 = compilation2.GetSpecialType(SpecialType.System_String);
        var intType2 = compilation2.GetSpecialType(SpecialType.System_Int32);

        var pairDefinition1 = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation1.GetTypeByMetadataName("System.Collections.Generic.KeyValuePair`2"));
        var pairDefinition2 = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation2.GetTypeByMetadataName("System.Collections.Generic.KeyValuePair`2"));

        var pair1 = Assert.IsAssignableFrom<INamedTypeSymbol>(pairDefinition1.Construct(stringType1, intType1));
        var pair2 = Assert.IsAssignableFrom<INamedTypeSymbol>(pairDefinition2.Construct(stringType2, intType2));

        var key1 = Assert.Single(pair1.GetMembers("Key").OfType<IPropertySymbol>());
        var key2 = Assert.Single(pair2.GetMembers("Key").OfType<IPropertySymbol>());

        Assert.True(SymbolEqualityComparer.Default.Equals(key1, key2));
        Assert.Equal(SymbolEqualityComparer.Default.GetHashCode(key1), SymbolEqualityComparer.Default.GetHashCode(key2));
    }

    [Fact]
    public void ConstructedMetadataType_AcrossCompilations_HasStableIdentity()
    {
        var compilation1 = Compilation.Create("pe_type_eq_1", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);
        var compilation2 = Compilation.Create("pe_type_eq_2", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var intType1 = compilation1.GetSpecialType(SpecialType.System_Int32);
        var intType2 = compilation2.GetSpecialType(SpecialType.System_Int32);

        var listDefinition1 = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation1.GetTypeByMetadataName("System.Collections.Generic.List`1"));
        var listDefinition2 = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation2.GetTypeByMetadataName("System.Collections.Generic.List`1"));

        var listOfInt1 = Assert.IsAssignableFrom<INamedTypeSymbol>(listDefinition1.Construct(intType1));
        var listOfInt2 = Assert.IsAssignableFrom<INamedTypeSymbol>(listDefinition2.Construct(intType2));

        Assert.True(SymbolEqualityComparer.Default.Equals(listOfInt1, listOfInt2));
        Assert.Equal(SymbolEqualityComparer.Default.GetHashCode(listOfInt1), SymbolEqualityComparer.Default.GetHashCode(listOfInt2));
        Assert.Equal(
            listOfInt1.ToFullyQualifiedMetadataName(),
            listOfInt2.ToFullyQualifiedMetadataName());
    }

    [Fact]
    public void MetadataType_AndReflectionType_ResolveToSameSymbol()
    {
        var compilation = Compilation.Create("pe_identity_single", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var fromMetadata = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Collections.Generic.List`1"));
        var fromReflection = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetType(typeof(List<>)));

        Assert.True(SymbolEqualityComparer.Default.Equals(fromMetadata, fromReflection));
        Assert.Same(fromMetadata, fromReflection);
    }

    [Fact]
    public void NestedMetadataType_AndReflectionType_ResolveToSameSymbol()
    {
        var compilation = Compilation.Create("pe_identity_nested", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var fromMetadata = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Collections.Generic.Dictionary`2+Enumerator"));
        var fromReflection = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetType(typeof(Dictionary<,>.Enumerator)));

        Assert.True(SymbolEqualityComparer.Default.Equals(fromMetadata, fromReflection));
        Assert.Same(fromMetadata, fromReflection);
    }

    [Fact]
    public void MetadataMethodTypeParameter_AcrossCompilations_HasStableSymbolEquality()
    {
        var compilation1 = Compilation.Create("pe_method_tp_1", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);
        var compilation2 = Compilation.Create("pe_method_tp_2", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var enumerable1 = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation1.GetTypeByMetadataName("System.Linq.Enumerable"));
        var enumerable2 = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation2.GetTypeByMetadataName("System.Linq.Enumerable"));

        var select1 = Assert.Single(
            enumerable1.GetMembers("Select")
                .OfType<IMethodSymbol>()
                .Where(method =>
                    method.MethodKind == MethodKind.Ordinary &&
                    method.Arity == 2 &&
                    method.Parameters.Length == 2 &&
                    method.Parameters[0].Type is INamedTypeSymbol { MetadataName: "IEnumerable`1" } &&
                    method.Parameters[1].Type is INamedTypeSymbol { MetadataName: "Func`2" }));

        var select2 = Assert.Single(
            enumerable2.GetMembers("Select")
                .OfType<IMethodSymbol>()
                .Where(method =>
                    method.MethodKind == MethodKind.Ordinary &&
                    method.Arity == 2 &&
                    method.Parameters.Length == 2 &&
                    method.Parameters[0].Type is INamedTypeSymbol { MetadataName: "IEnumerable`1" } &&
                    method.Parameters[1].Type is INamedTypeSymbol { MetadataName: "Func`2" }));

        var tp1 = Assert.IsAssignableFrom<ITypeParameterSymbol>(select1.TypeParameters[0]);
        var tp2 = Assert.IsAssignableFrom<ITypeParameterSymbol>(select2.TypeParameters[0]);

        Assert.True(SymbolEqualityComparer.Default.Equals(tp1, tp2));
        Assert.Equal(SymbolEqualityComparer.Default.GetHashCode(tp1), SymbolEqualityComparer.Default.GetHashCode(tp2));
    }

    [Fact]
    public void MetadataMethodParameter_AcrossCompilations_HasStableSymbolEquality()
    {
        var compilation1 = Compilation.Create("pe_method_param_1", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);
        var compilation2 = Compilation.Create("pe_method_param_2", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var intType1 = compilation1.GetSpecialType(SpecialType.System_Int32);
        var stringType1 = compilation1.GetSpecialType(SpecialType.System_String);
        var intType2 = compilation2.GetSpecialType(SpecialType.System_Int32);
        var stringType2 = compilation2.GetSpecialType(SpecialType.System_String);

        var dictionaryDefinition1 = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation1.GetTypeByMetadataName("System.Collections.Generic.Dictionary`2"));
        var dictionaryDefinition2 = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation2.GetTypeByMetadataName("System.Collections.Generic.Dictionary`2"));

        var dictionary1 = Assert.IsAssignableFrom<INamedTypeSymbol>(dictionaryDefinition1.Construct(intType1, stringType1));
        var dictionary2 = Assert.IsAssignableFrom<INamedTypeSymbol>(dictionaryDefinition2.Construct(intType2, stringType2));

        var tryGetValue1 = Assert.Single(
            dictionary1.GetMembers("TryGetValue")
                .OfType<IMethodSymbol>()
                .Where(method => method.Parameters.Length == 2));

        var tryGetValue2 = Assert.Single(
            dictionary2.GetMembers("TryGetValue")
                .OfType<IMethodSymbol>()
                .Where(method => method.Parameters.Length == 2));

        var outParameter1 = tryGetValue1.Parameters[1];
        var outParameter2 = tryGetValue2.Parameters[1];

        Assert.Equal(RefKind.Out, outParameter1.RefKind);
        Assert.Equal(RefKind.Out, outParameter2.RefKind);
        Assert.True(SymbolEqualityComparer.Default.Equals(outParameter1, outParameter2));
        Assert.Equal(SymbolEqualityComparer.Default.GetHashCode(outParameter1), SymbolEqualityComparer.Default.GetHashCode(outParameter2));
    }

    [Fact]
    public void MetadataTypeIdentity_AcrossCompilations_IsStable()
    {
        var compilation1 = Compilation.Create("pe_type_identity_1", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);
        var compilation2 = Compilation.Create("pe_type_identity_2", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var type1 = Assert.IsType<PENamedTypeSymbol>(compilation1.GetTypeByMetadataName("System.Collections.Generic.List`1"));
        var type2 = Assert.IsType<PENamedTypeSymbol>(compilation2.GetTypeByMetadataName("System.Collections.Generic.List`1"));

        Assert.Equal(type1.MetadataIdentity, type2.MetadataIdentity);
    }

    [Fact]
    public void MetadataTypeParameterIdentity_AcrossCompilations_IsStable()
    {
        var compilation1 = Compilation.Create("pe_type_param_identity_1", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);
        var compilation2 = Compilation.Create("pe_type_param_identity_2", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var enumerable1 = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation1.GetTypeByMetadataName("System.Linq.Enumerable"));
        var enumerable2 = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation2.GetTypeByMetadataName("System.Linq.Enumerable"));

        var select1 = Assert.Single(
            enumerable1.GetMembers("Select")
                .OfType<IMethodSymbol>()
                .Where(method =>
                    method.MethodKind == MethodKind.Ordinary &&
                    method.Arity == 2 &&
                    method.Parameters.Length == 2 &&
                    method.Parameters[0].Type is INamedTypeSymbol { MetadataName: "IEnumerable`1" } &&
                    method.Parameters[1].Type is INamedTypeSymbol { MetadataName: "Func`2" }));

        var select2 = Assert.Single(
            enumerable2.GetMembers("Select")
                .OfType<IMethodSymbol>()
                .Where(method =>
                    method.MethodKind == MethodKind.Ordinary &&
                    method.Arity == 2 &&
                    method.Parameters.Length == 2 &&
                    method.Parameters[0].Type is INamedTypeSymbol { MetadataName: "IEnumerable`1" } &&
                    method.Parameters[1].Type is INamedTypeSymbol { MetadataName: "Func`2" }));

        var tp1 = Assert.IsType<PETypeParameterSymbol>(select1.TypeParameters[0]);
        var tp2 = Assert.IsType<PETypeParameterSymbol>(select2.TypeParameters[0]);

        Assert.Equal(tp1.MetadataIdentity, tp2.MetadataIdentity);
    }

    [Fact]
    public void MetadataField_AcrossCompilations_HasStableSymbolEquality()
    {
        var compilation1 = Compilation.Create("pe_field_eq_1", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);
        var compilation2 = Compilation.Create("pe_field_eq_2", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var enum1 = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation1.GetTypeByMetadataName("System.DayOfWeek"));
        var enum2 = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation2.GetTypeByMetadataName("System.DayOfWeek"));

        var field1 = Assert.Single(enum1.GetMembers().OfType<IFieldSymbol>().Where(field => field.Name == "Monday"));
        var field2 = Assert.Single(enum2.GetMembers().OfType<IFieldSymbol>().Where(field => field.Name == "Monday"));

        Assert.True(SymbolEqualityComparer.Default.Equals(field1, field2));
        Assert.Equal(SymbolEqualityComparer.Default.GetHashCode(field1), SymbolEqualityComparer.Default.GetHashCode(field2));
    }

    [Fact]
    public void MetadataField_UsesFieldMetadataName()
    {
        var compilation = Compilation.Create("pe_field_metadata_name", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var stringType = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.String"));

        var emptyField = Assert.Single(
            stringType.GetMembers()
                .OfType<IFieldSymbol>()
                .Where(field => field.Name == "Empty"));

        Assert.Equal("Empty", emptyField.MetadataName);
    }
}
