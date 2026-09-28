using Raven.CodeAnalysis.Symbols;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class IntersectionConversionTests : CompilationTestBase
{
    private Compilation CreateTypes(bool diagnosticsFirst = true)
    {
        var (compilation, _) = CreateCompilation("""
            interface A {}
            interface B {}
            interface C {}
            interface ChildA: A {}
            open class Base {}
            class Both: Base, A, B {}
            class OnlyA: A {}
            struct ValueBoth: A, B {}
            class Converts {
                static func implicit(value: Converts) -> Both { return Both() }
            }
            """);
        if (diagnosticsFirst)
            Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        return compilation;
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Entry_RequiresEveryConstituentWithoutBoxingOrUserConversions(bool diagnosticsFirst)
    {
        var compilation = CreateTypes(diagnosticsFirst);
        var target = compilation.CreateIntersectionTypeSymbol(Type("A"), Type("B"));
        AssertReference(compilation.ClassifyConversion(Type("Both"), target));
        Assert.True(compilation.ClassifyConversion(Type("Converts"), Type("Both")).IsUserDefined);
        foreach (var name in new[] { "OnlyA", "ValueBoth", "Converts", "Base" })
        {
            Assert.False(compilation.ClassifyConversion(Type(name), target).Exists);
            Assert.False(compilation.ClassifyConversion(Type(name), target, includeUserDefined: false).Exists);
        }
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);

        INamedTypeSymbol Type(string name) => compilation.GetTypeByMetadataName(name)!;
    }

    [Theory]
    [InlineData("A")]
    [InlineData("B")]
    [InlineData("System.Object")]
    public void Projection_IsImplicitReference(string targetName)
    {
        var compilation = CreateTypes();
        var source = compilation.CreateIntersectionTypeSymbol(
            compilation.GetTypeByMetadataName("A")!, compilation.GetTypeByMetadataName("B")!);
        AssertReference(compilation.ClassifyConversion(source, compilation.GetTypeByMetadataName(targetName)!));
    }

    [Fact]
    public void ClassAndInterfaceBounds_UseNominalInheritance()
    {
        var compilation = CreateTypes();
        var baseType = compilation.GetTypeByMetadataName("Base")!;
        var a = compilation.GetTypeByMetadataName("A")!;
        var target = compilation.CreateIntersectionTypeSymbol(baseType, a);
        AssertReference(compilation.ClassifyConversion(compilation.GetTypeByMetadataName("Both")!, target));
        AssertReference(compilation.ClassifyConversion(target, baseType));
        var child = compilation.CreateIntersectionTypeSymbol(
            compilation.GetTypeByMetadataName("ChildA")!, compilation.GetTypeByMetadataName("B")!);
        AssertReference(compilation.ClassifyConversion(child, a));
    }

    [Fact]
    public void CompoundToCompound_DropsBoundsButDoesNotInventThem()
    {
        var compilation = CreateTypes();
        var a = compilation.GetTypeByMetadataName("A")!;
        var b = compilation.GetTypeByMetadataName("B")!;
        var c = compilation.GetTypeByMetadataName("C")!;
        var ab = compilation.CreateIntersectionTypeSymbol(a, b);
        var abc = compilation.CreateIntersectionTypeSymbol(a, b, c);
        AssertReference(compilation.ClassifyConversion(abc, ab));
        Assert.False(compilation.ClassifyConversion(ab, abc).Exists);
        Assert.False(compilation.ClassifyConversion(ab, c).Exists);
        Assert.False(compilation.ClassifyConversion(a, ab).Exists);
        Assert.True(compilation.ClassifyConversion(ab, compilation.CreateIntersectionTypeSymbol(b, a)).IsIdentity);
    }

    [Fact]
    public void NullableWrappers_PreserveReferenceNullability()
    {
        var compilation = CreateTypes();
        var a = compilation.GetTypeByMetadataName("A")!;
        var b = compilation.GetTypeByMetadataName("B")!;
        var both = compilation.GetTypeByMetadataName("Both")!;
        var ab = compilation.CreateIntersectionTypeSymbol(a, b);
        AssertReference(compilation.ClassifyConversion(ab, a.GetNullableType()));
        AssertReference(compilation.ClassifyConversion(both, ab.GetNullableType()));
        AssertReference(compilation.ClassifyConversion(ab.GetNullableType(), a.GetNullableType()));
        Assert.False(compilation.ClassifyConversion(both.GetNullableType(), ab).Exists);
        Assert.False(compilation.ClassifyConversion(compilation.NullTypeSymbol, ab).Exists);
        AssertReference(compilation.ClassifyConversion(compilation.NullTypeSymbol, ab.GetNullableType()));
        Assert.False(compilation.ClassifyConversion(ab.GetNullableType(), a).IsImplicit);
        var nullableBounds = compilation.CreateIntersectionTypeSymbol(a.GetNullableType(), b.GetNullableType());
        Assert.False(compilation.ClassifyConversion(both, nullableBounds).Exists);
    }

    [Fact]
    public void Projection_UsesExistingInterfaceVariance()
    {
        var compilation = CreateTypes();
        var enumerable = compilation.GetTypeByMetadataName("System.Collections.Generic.IEnumerable`1")!;
        var strings = enumerable.Construct(compilation.GetSpecialType(SpecialType.System_String));
        var objects = enumerable.Construct(compilation.GetSpecialType(SpecialType.System_Object));
        var source = compilation.CreateIntersectionTypeSymbol(strings, compilation.GetTypeByMetadataName("A")!);
        AssertReference(compilation.ClassifyConversion(source, objects));
    }

    [Fact]
    public void ArrayConstituentsAndTypeParameterEntailment_AreDeferred()
    {
        var compilation = CreateTypes();
        var a = compilation.GetTypeByMetadataName("A")!;
        var array = compilation.CreateArrayTypeSymbol(a);
        var compoundArray = compilation.CreateIntersectionTypeSymbol(array, a);
        Assert.False(compilation.ClassifyConversion(compoundArray, array).Exists);

        var (genericCompilation, _) = CreateCompilation("""
            interface A {}
            interface B {}
            class Box<T> where T: class, A & B {}
            """);
        Assert.DoesNotContain(genericCompilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var parameter = Assert.Single(genericCompilation.GetTypeByMetadataName("Box`1")!.TypeParameters);
        var target = genericCompilation.CreateIntersectionTypeSymbol(
            genericCompilation.GetTypeByMetadataName("A")!, genericCompilation.GetTypeByMetadataName("B")!);
        Assert.False(genericCompilation.ClassifyConversion(parameter, target).Exists);
    }

    [Fact]
    public void ValueConstituents_DoNotGainNumericOrBoxingConversions()
    {
        var compilation = CreateTypes();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var numeric = compilation.CreateIntersectionTypeSymbol(intType, compilation.GetSpecialType(SpecialType.System_Int64));
        Assert.False(compilation.ClassifyConversion(intType, numeric).Exists);
        Assert.False(compilation.ClassifyConversion(numeric, compilation.GetSpecialType(SpecialType.System_Object)).Exists);
    }

    private static void AssertReference(Conversion conversion)
    {
        Assert.True(conversion.Exists);
        Assert.True(conversion.IsImplicit);
        Assert.True(conversion.IsReference);
        Assert.False(conversion.IsIdentity);
        Assert.False(conversion.IsBoxing);
        Assert.False(conversion.IsUserDefined);
    }
}
