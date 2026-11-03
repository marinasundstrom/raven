using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class NullableGenericSubstitutionTests
{
    private const string Source = """
union ValueChoice { case Item(value: int) }

class NullableGenericClass<T> {
    func Echo(value: T?) -> T? => value
}

class NullableReferenceGenericClass<T> where T: class {
    func Echo(value: T?) -> T? => value
}

class NullableValueGenericClass<T> where T: struct {
    func Echo(value: T?) -> T? => value
}

class NullableGenericMethods {
    static func Echo<T>(value: T?) -> T? => value

    static func EchoReference<T>(value: T?) -> T? where T: class => value

    static func EchoValue<T>(value: T?) -> T? where T: struct => value
}
""";

    [Theory]
    [Trait("Target", "Shared")]
    [InlineData("System.Int32")]
    [InlineData("ValueChoice")]
    public void AnnotatedValueSubstitutionUsesIdentityConversions(string typeName)
    {
        var compilation = CreateCompilation();
        var valueType = compilation.GetTypeByMetadataName(typeName)!;
        var annotated = GetConstructedTypeMethod(compilation, "NullableGenericClass`1", valueType).Parameters[0].Type;
        Assert.True(compilation.ClassifyConversion(valueType, annotated).IsIdentity);
        var reverse = compilation.ClassifyConversion(annotated, valueType);
        Assert.True(reverse.IsIdentity);
        Assert.True(reverse.IsImplicit);

        var nullableValue = GetConstructedTypeMethod(compilation, "NullableValueGenericClass`1", valueType).Parameters[0].Type;
        Assert.False(compilation.ClassifyConversion(valueType, nullableValue).IsIdentity);
        Assert.False(compilation.ClassifyConversion(nullableValue, valueType).IsImplicit);
    }

    [Fact]
    public void ConstructedGenericTypes_PreserveSubstitutionAwareNullability()
    {
        var compilation = CreateCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var stringType = compilation.GetSpecialType(SpecialType.System_String);

        AssertSignature(
            GetConstructedTypeMethod(compilation, "NullableGenericClass`1", intType),
            intType,
            usesNullableValueTypeRepresentation: false);
        AssertSignature(
            GetConstructedTypeMethod(compilation, "NullableGenericClass`1", stringType),
            stringType,
            usesNullableValueTypeRepresentation: false);
        AssertSignature(
            GetConstructedTypeMethod(compilation, "NullableReferenceGenericClass`1", stringType),
            stringType,
            usesNullableValueTypeRepresentation: false);
        AssertSignature(
            GetConstructedTypeMethod(compilation, "NullableValueGenericClass`1", intType),
            intType,
            usesNullableValueTypeRepresentation: true);
    }

    [Fact]
    public void ConstructedGenericMethods_PreserveSubstitutionAwareNullability()
    {
        var compilation = CreateCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var container = Assert.IsAssignableFrom<NamedTypeSymbol>(
            compilation.GetTypeByMetadataName("NullableGenericMethods"));

        AssertSignature(GetMethod(container, "Echo").Construct(intType), intType, usesNullableValueTypeRepresentation: false);
        AssertSignature(GetMethod(container, "Echo").Construct(stringType), stringType, usesNullableValueTypeRepresentation: false);
        AssertSignature(GetMethod(container, "EchoReference").Construct(stringType), stringType, usesNullableValueTypeRepresentation: false);
        AssertSignature(GetMethod(container, "EchoValue").Construct(intType), intType, usesNullableValueTypeRepresentation: true);
    }

    [Fact]
    public void SemanticEquality_RemainsUnifiedAcrossAbiProjections()
    {
        var compilation = CreateCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var projectedUnderlying = GetMethod(
            Assert.IsAssignableFrom<NamedTypeSymbol>(compilation.GetTypeByMetadataName("NullableGenericMethods")),
            "Echo").Construct(intType).ReturnType;
        var projectedNullableValue = intType.GetNullableType();

        Assert.True(SymbolEqualityComparer.Default.Equals(projectedUnderlying, projectedNullableValue));
        Assert.NotEqual(
            projectedUnderlying.GetNullableAbiProjection(),
            projectedNullableValue.GetNullableAbiProjection());
    }

    private static Compilation CreateCompilation()
    {
        var syntaxTree = SyntaxTree.ParseText(Source);
        var compilation = Compilation.Create(
            "nullable_generic_substitution",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        return compilation;
    }

    private static MethodSymbol GetConstructedTypeMethod(
        Compilation compilation,
        string metadataName,
        TypeSymbol typeArgument)
    {
        var definition = Assert.IsAssignableFrom<NamedTypeSymbol>(compilation.GetTypeByMetadataName(metadataName));
        var constructed = Assert.IsAssignableFrom<NamedTypeSymbol>(definition.Construct(typeArgument));
        return GetMethod(constructed, "Echo");
    }

    private static MethodSymbol GetMethod(NamedTypeSymbol type, string name)
        => Assert.Single(type.GetMembers(name).OfType<MethodSymbol>());

    private static void AssertSignature(
        MethodSymbol method,
        TypeSymbol expectedUnderlyingType,
        bool usesNullableValueTypeRepresentation)
    {
        AssertType(Assert.Single(method.Parameters).Type, expectedUnderlyingType, usesNullableValueTypeRepresentation);
        AssertType(method.ReturnType, expectedUnderlyingType, usesNullableValueTypeRepresentation);
    }

    private static void AssertType(
        TypeSymbol actualType,
        TypeSymbol expectedUnderlyingType,
        bool usesNullableValueTypeRepresentation)
    {
        Assert.True(actualType.TryGetNullableUnderlyingType(out var underlyingType));
        Assert.Equal(
            usesNullableValueTypeRepresentation
                ? NullableAbiProjection.NullableValueType
                : NullableAbiProjection.AnnotatedUnderlyingType,
            actualType.GetNullableAbiProjection());
        Assert.True(SymbolEqualityComparer.Default.Equals(expectedUnderlyingType, underlyingType));
    }
}
