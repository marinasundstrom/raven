using System.Collections.Generic;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Semantics.Tests;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public sealed class PESymbolRegressionTests : CompilationTestBase
{
    [Fact]
    public void MetadataEnums_ReportEnumTypeKind()
    {
        var compilation = Compilation.Create("pe_enum_kind")
            .AddReferences(TestMetadataReferences.Default);

        var attributeTargets = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.AttributeTargets"));

        Assert.Equal(TypeKind.Enum, attributeTargets.TypeKind);
        Assert.Equal(SpecialType.System_Int32, attributeTargets.EnumUnderlyingType?.SpecialType);

        var stringType = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.String"));
        Assert.Null(stringType.EnumUnderlyingType);
    }

    [Fact]
    public void MetadataTypes_ReportRefLikeType()
    {
        var compilation = Compilation.Create("pe_ref_like_type")
            .AddReferences(TestMetadataReferences.Default);

        var spanDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Span`1"));
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var spanOfInt = Assert.IsAssignableFrom<INamedTypeSymbol>(spanDefinition.Construct(intType));
        var valueTupleDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.ValueTuple`1"));

        Assert.True(spanDefinition.IsRefLikeType);
        Assert.True(spanOfInt.IsRefLikeType);
        Assert.False(valueTupleDefinition.IsRefLikeType);
    }

    [Fact]
    public void MetadataGenericType_NameLazilyStripsMetadataArity()
    {
        var compilation = Compilation.Create("pe_type_name")
            .AddReferences(TestMetadataReferences.Default);
        var listDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Collections.Generic.List`1"));

        Assert.Equal("List", listDefinition.Name);
        Assert.Equal("List", listDefinition.Name);
        Assert.Equal("List`1", listDefinition.MetadataName);
    }

    [Fact]
    public void MetadataTypeParameters_ReportWhetherTheyAllowRefLikeArguments()
    {
        var compilation = Compilation.Create("pe_ref_like_generic_parameters")
            .AddReferences(TestMetadataReferences.Default);

        var listDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Collections.Generic.List`1"));
        var actionDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Action`1"));
        var spanDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Span`1"));
        var spanOfInt = spanDefinition.Construct(compilation.GetSpecialType(SpecialType.System_Int32));

        Assert.Equal(
            TypeParameterConstraintKind.None,
            listDefinition.TypeParameters[0].ConstraintKind & TypeParameterConstraintKind.AllowByRefLike);
        Assert.Equal(
            TypeParameterConstraintKind.AllowByRefLike,
            actionDefinition.TypeParameters[0].ConstraintKind & TypeParameterConstraintKind.AllowByRefLike);
        Assert.False(spanOfInt.SatisfiesConstraints(listDefinition.TypeParameters[0]));
        Assert.True(spanOfInt.SatisfiesConstraints(actionDefinition.TypeParameters[0]));
    }

    [Fact]
    public void MetadataParameters_ReportScopedKind()
    {
        var compilation = Compilation.Create("pe_scoped_parameters")
            .AddReferences(TestMetadataReferences.DefaultWithExtensionMethods);
        var fixture = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("Raven.ExtensionMethodsFixture.ScopedParameterFixture"));

        var scopedValue = Assert.Single(
            fixture.GetMembers("Consume").OfType<IMethodSymbol>()).Parameters[0];
        var scopedRef = Assert.Single(
            fixture.GetMembers("ConsumeRef").OfType<IMethodSymbol>()).Parameters[0];
        var unscoped = Assert.Single(
            fixture.GetMembers("ConsumeUnscoped").OfType<IMethodSymbol>()).Parameters[0];

        Assert.Equal(ScopedKind.ScopedValue, scopedValue.ScopedKind);
        Assert.Equal(ScopedKind.ScopedRef, scopedRef.ScopedKind);
        Assert.Equal(ScopedKind.None, unscoped.ScopedKind);
    }

    [Fact]
    public void MetadataTypes_ReportDeclaredAccessibility()
    {
        const string metadataSource = """
namespace Lib

internal class InternalTopLevel {}

public class PublicContainer {
    public class PublicNested {}
    internal class InternalNested {}
    private class PrivateNested {}
    protected class ProtectedNested {}
    protected internal class ProtectedInternalNested {}
    private protected class PrivateProtectedNested {}
}
""";

        var metadataReference = TestMetadataFactory.CreateFileReferenceFromSource(
            metadataSource,
            assemblyName: "pe-accessibility-fixture");
        var compilation = Compilation.Create("pe_accessibility_consumer")
            .AddReferences(TestMetadataReferences.Default.Append(metadataReference).ToArray());

        AssertAccessibility("Lib.InternalTopLevel", Accessibility.Internal);
        AssertAccessibility("Lib.PublicContainer", Accessibility.Public);
        AssertAccessibility("Lib.PublicContainer+PublicNested", Accessibility.Public);
        AssertAccessibility("Lib.PublicContainer+InternalNested", Accessibility.Internal);
        AssertAccessibility("Lib.PublicContainer+PrivateNested", Accessibility.Private);
        AssertAccessibility("Lib.PublicContainer+ProtectedNested", Accessibility.ProtectedAndProtected);
        AssertAccessibility("Lib.PublicContainer+ProtectedInternalNested", Accessibility.ProtectedOrInternal);
        AssertAccessibility("Lib.PublicContainer+PrivateProtectedNested", Accessibility.ProtectedAndInternal);

        void AssertAccessibility(string metadataName, Accessibility expected)
        {
            var type = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.GetTypeByMetadataName(metadataName));
            Assert.Equal(expected, type.DeclaredAccessibility);
        }
    }

    [Fact]
    public void MetadataMembers_ReportDeclaredAccessibility()
    {
        const string metadataSource = """
namespace Lib

public class MemberContainer {
    public field PublicField: int = 0
    internal field InternalField: int = 0
    private field PrivateField: int = 0
    protected field ProtectedField: int = 0
    protected internal field ProtectedInternalField: int = 0
    private protected field PrivateProtectedField: int = 0

    public func PublicMethod() -> unit { return }
    internal func InternalMethod() -> unit { return }
    private func PrivateMethod() -> unit { return }
    protected func ProtectedMethod() -> unit { return }
    protected internal func ProtectedInternalMethod() -> unit { return }
    private protected func PrivateProtectedMethod() -> unit { return }
}
""";

        var metadataReference = TestMetadataFactory.CreateFileReferenceFromSource(
            metadataSource,
            assemblyName: "pe-member-accessibility-fixture");
        var compilation = Compilation.Create("pe_member_accessibility_consumer")
            .AddReferences(TestMetadataReferences.Default.Append(metadataReference).ToArray());
        var container = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.GetTypeByMetadataName("Lib.MemberContainer"));

        AssertFieldAccessibility("PublicField", Accessibility.Public);
        AssertFieldAccessibility("InternalField", Accessibility.Internal);
        AssertFieldAccessibility("PrivateField", Accessibility.Private);
        AssertFieldAccessibility("ProtectedField", Accessibility.ProtectedAndProtected);
        AssertFieldAccessibility("ProtectedInternalField", Accessibility.ProtectedOrInternal);
        AssertFieldAccessibility("PrivateProtectedField", Accessibility.ProtectedAndInternal);

        AssertMethodAccessibility("PublicMethod", Accessibility.Public);
        AssertMethodAccessibility("InternalMethod", Accessibility.Internal);
        AssertMethodAccessibility("PrivateMethod", Accessibility.Private);
        AssertMethodAccessibility("ProtectedMethod", Accessibility.ProtectedAndProtected);
        AssertMethodAccessibility("ProtectedInternalMethod", Accessibility.ProtectedOrInternal);
        AssertMethodAccessibility("PrivateProtectedMethod", Accessibility.ProtectedAndInternal);

        void AssertFieldAccessibility(string name, Accessibility expected)
        {
            var field = Assert.Single(container.GetMembers(name).OfType<IFieldSymbol>());
            Assert.Equal(expected, field.DeclaredAccessibility);
        }

        void AssertMethodAccessibility(string name, Accessibility expected)
        {
            var method = Assert.Single(container.GetMembers(name).OfType<IMethodSymbol>());
            Assert.Equal(expected, method.DeclaredAccessibility);
        }
    }

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
    public void ConstructedMetadataType_GetMembersByName_DoesNotForceFullDefinitionMemberLoad()
    {
        var compilation = Compilation.Create("pe_constructed_member_lookup", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var listDefinition = Assert.IsType<PENamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Collections.Generic.List`1"));
        var listOfInt = Assert.IsAssignableFrom<INamedTypeSymbol>(listDefinition.Construct(intType));

        Assert.False(IsFullyLoaded(listDefinition));

        var addMembers = listOfInt.GetMembers("Add").OfType<IMethodSymbol>().ToArray();

        Assert.NotEmpty(addMembers);
        Assert.All(addMembers, method => Assert.Equal("Add", method.Name));
        Assert.False(IsFullyLoaded(listDefinition));
    }

    [Fact]
    public void ConstructedMetadataType_GetMembersByName_LoadsExplicitInterfaceProperty()
    {
        var compilation = Compilation.Create("pe_explicit_property_lookup", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var arrayDefinition = Assert.IsType<PENamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Collections.Immutable.ImmutableArray`1"));
        var arrayOfInt = Assert.IsAssignableFrom<INamedTypeSymbol>(arrayDefinition.Construct(intType));

        Assert.False(IsFullyLoaded(arrayDefinition));

        var countProperties = arrayOfInt.GetMembers("Count").OfType<IPropertySymbol>().ToArray();

        Assert.NotEmpty(countProperties);
        Assert.All(countProperties, static count =>
        {
            Assert.Equal("Count", count.Name);
            Assert.Equal(SpecialType.System_Int32, count.Type.SpecialType);
            Assert.NotNull(count.GetMethod);
        });
        Assert.False(IsFullyLoaded(arrayDefinition));
    }

    [Fact]
    public void MetadataProperty_PreservesDefinitionTypeAndRequiredState()
    {
        var compilation = Compilation.Create("pe_property_contract")
            .AddReferences([
                .. TestMetadataReferences.Default,
                MetadataReference.CreateFromFile(typeof(RequiredPropertyFixture).Assembly.Location),
            ]);
        var fixture = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName(
                "Raven.CodeAnalysis.Tests.PESymbolRegressionTests+RequiredPropertyFixture"));
        var required = Assert.Single(fixture.GetMembers("Required").OfType<IPropertySymbol>());
        var optional = Assert.Single(fixture.GetMembers("Optional").OfType<IPropertySymbol>());

        Assert.Same(required, required.OriginalDefinition);
        Assert.Same(required.Type, required.Type);
        Assert.Equal(SpecialType.System_String, required.Type.SpecialType);
        Assert.True(required.IsRequired);
        Assert.False(optional.IsRequired);
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

    [Fact]
    public void MetadataField_AlwaysHasContainingTypeAndFieldType()
    {
        var compilation = Compilation.Create("pe_field_contract")
            .AddReferences(TestMetadataReferences.Default);
        var stringType = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.String"));
        var emptyField = Assert.Single(stringType.GetMembers("Empty").OfType<IFieldSymbol>());

        Assert.Same(stringType, emptyField.ContainingType);
        Assert.NotNull(emptyField.Type);
        Assert.Same(emptyField.Type, emptyField.Type);
    }

    [Fact]
    public void MetadataEvent_AlwaysHasContainingTypeAndEventType()
    {
        var compilation = Compilation.Create("pe_event_contract")
            .AddReferences(TestMetadataReferences.Default);
        var appDomainType = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.AppDomain"));
        var processExitEvent = Assert.Single(appDomainType.GetMembers("ProcessExit").OfType<IEventSymbol>());

        Assert.Same(appDomainType, processExitEvent.ContainingType);
        Assert.NotNull(processExitEvent.Type);
        Assert.Same(processExitEvent.Type, processExitEvent.Type);
    }

    [Fact]
    public void MetadataMethod_AlwaysHasContainingTypeAndReturnType()
    {
        var compilation = Compilation.Create("pe_method_contract")
            .AddReferences(TestMetadataReferences.Default);
        var stringType = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.String"));
        var containsMethod = Assert.Single(
            stringType.GetMembers("Contains")
                .OfType<IMethodSymbol>()
                .Where(method => method.Parameters is [{ Type.SpecialType: SpecialType.System_Char }]));

        Assert.Same(stringType, containsMethod.ContainingType);
        Assert.Equal(SpecialType.System_Boolean, containsMethod.ReturnType.SpecialType);
        Assert.Same(containsMethod.ReturnType, containsMethod.ReturnType);
    }

    public sealed class RequiredPropertyFixture
    {
        public required string Required { get; set; }

        [Obsolete]
        public string Optional { get; set; } = string.Empty;
    }

    private static bool IsFullyLoaded(PENamedTypeSymbol type)
    {
        var field = typeof(PENamedTypeSymbol).GetField("_membersLoaded", BindingFlags.Instance | BindingFlags.NonPublic);
        Assert.NotNull(field);
        return Assert.IsType<bool>(field.GetValue(type));
    }
}
