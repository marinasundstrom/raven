using System;
using System.Linq;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Symbols;

public sealed class IntersectionTypeSymbolTests
{
    [Fact]
    public void Factory_FlattensAndDeduplicatesWithoutChangingSourceOrder()
    {
        var (compilation, a, b) = CreateTypes();
        var nested = compilation.CreateIntersectionTypeSymbol(a, b);
        var type = Assert.IsAssignableFrom<IIntersectionTypeSymbol>(
            compilation.CreateIntersectionTypeSymbol(b, nested, a));
        Assert.Equal(new ITypeSymbol[] { b, a }, type.ConstituentTypes);
        Assert.Equal(TypeKind.Intersection, type.TypeKind);
        Assert.Null(type.ContainingSymbol);
        Assert.Empty(type.MetadataName);
        Assert.Same(a, compilation.CreateIntersectionTypeSymbol(a, a));
    }

    [Fact]
    public void EqualityAndHash_AreOrderIndependent()
    {
        var (compilation, a, b) = CreateTypes();
        var left = compilation.CreateIntersectionTypeSymbol(a, b);
        var right = compilation.CreateIntersectionTypeSymbol(b, a);
        foreach (var comparer in new[] { SymbolEqualityComparer.Default, SymbolEqualityComparer.IgnoringNullability })
        {
            Assert.True(comparer.Equals(left, right));
            Assert.Equal(comparer.GetHashCode(left), comparer.GetHashCode(right));
        }

        Assert.False(SymbolEqualityComparer.Default.Equals(left, a));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Normalization_RemovesOnlyNominalSupertypes(bool bindDiagnosticsFirst)
    {
        var compilation = CreateCompilation("""
            interface A {}
            interface B: A {}
            open class Base {}
            open class Derived: Base, B {}
            """, bindDiagnosticsFirst);
        var a = compilation.GetTypeByMetadataName("A")!;
        var b = compilation.GetTypeByMetadataName("B")!;
        var derived = compilation.GetTypeByMetadataName("Derived")!;
        var baseType = compilation.GetTypeByMetadataName("Base")!;
        Assert.Same(b, compilation.CreateIntersectionTypeSymbol(a, b));
        Assert.Same(derived, compilation.CreateIntersectionTypeSymbol(baseType, a, b, derived));
        Assert.Throws<ArgumentException>(() => compilation.CreateIntersectionTypeSymbol(
            compilation.GetSpecialType(SpecialType.System_Int32), compilation.GetSpecialType(SpecialType.System_Int64)));
    }

    [Fact]
    public void Members_PreserveDistinctDeclarationsWithTheSameSignature()
    {
        var (compilation, a, b) = CreateTypes();
        var type = compilation.CreateIntersectionTypeSymbol(a, b);
        Assert.Equal(2, type.GetMembers("Shared").Length);
        Assert.Single(type.GetMembers("First"));
        Assert.Single(type.GetMembers("Second"));
        Assert.False(type.IsMemberDefined("Shared", out var ambiguous));
        Assert.Null(ambiguous);
        Assert.True(type.IsMemberDefined("First", out var first));
        Assert.Same(a, first!.ContainingType);
        Assert.Equal(2, type.AllInterfaces.Length);
    }

    [Fact]
    public void Display_PreservesGroupingInNullableAndArrayTypes()
    {
        var (compilation, a, b) = CreateTypes();
        var type = compilation.CreateIntersectionTypeSymbol(a, b);
        var format = SymbolDisplayFormat.MinimallyQualifiedFormat;
        Assert.Equal("A & B", type.ToDisplayString(format));
        Assert.Equal("(A & B)[]", compilation.CreateArrayTypeSymbol(type).ToDisplayString(format));
        Assert.Equal("(A & B)[,]", compilation.CreateArrayTypeSymbol(type, 2).ToDisplayString(format));
        var nullable = new NullableTypeSymbol(type, null, null, null, []);
        Assert.Equal("(A & B)?", nullable.ToDisplayString(format));
    }

    [Fact]
    public void BoundsMustBeExtendable_BeforeNormalization()
    {
        var compilation = CreateCompilation("""
            abstract class Animal {}
            interface Quacks {}
            interface Barks {}
            class Dog: Barks {}
            class Generic<T> {}
            """);
        var animal = compilation.GetTypeByMetadataName("Animal")!;
        var quacks = compilation.GetTypeByMetadataName("Quacks")!;
        var dog = compilation.GetTypeByMetadataName("Dog")!;
        var barks = compilation.GetTypeByMetadataName("Barks")!;
        Assert.IsAssignableFrom<IIntersectionTypeSymbol>(compilation.CreateIntersectionTypeSymbol(animal, quacks));
        Assert.Throws<ArgumentException>(() => compilation.CreateIntersectionTypeSymbol(dog, barks));
        Assert.Throws<ArgumentException>(() => compilation.CreateIntersectionTypeSymbol(dog, dog));
        Assert.Throws<ArgumentException>(() => compilation.CreateIntersectionTypeSymbol(quacks.GetNullableType(), barks));
        var parameter = Assert.Single(compilation.GetTypeByMetadataName("Generic`1")!.TypeParameters);
        Assert.Throws<ArgumentException>(() => compilation.CreateIntersectionTypeSymbol(parameter, quacks));
    }

    [Fact]
    public void Substitution_PreservesUnchangedIdentity()
    {
        var (compilation, a, b) = CreateTypes();
        var intersection = Assert.IsAssignableFrom<IIntersectionTypeSymbol>(compilation.CreateIntersectionTypeSymbol(a, b));
        Assert.Same(intersection, TypeSubstitution.SubstituteIntersection(intersection, type => type));
    }

    [Fact]
    public void Visitors_ReceiveIntersectionSymbols()
    {
        var (compilation, a, b) = CreateTypes();
        var type = compilation.CreateIntersectionTypeSymbol(a, b);
        Assert.Equal(2, new ConstituentCountVisitor().Visit(type));
    }

    [Fact]
    public void SemanticIntersection_DoesNotEnablePortableSignatureStorage()
    {
        var (compilation, a, b) = CreateTypes();
        var intersection = compilation.CreateIntersectionTypeSymbol(a, b);
        Assert.True(CallableSignature.TryType(a, false, out _));
        foreach (var type in new[] { intersection, intersection.GetNullableType(), compilation.CreateArrayTypeSymbol(intersection) })
        {
            Assert.False(CallableSignature.TryType(type, false, out _));
            Assert.False(CallableSignature.TryType(type, true, out _));
        }
    }

    [Fact]
    public void Factory_RejectsEmptyOrNullInputs()
    {
        var (compilation, a, _) = CreateTypes();
        Assert.Throws<ArgumentException>(() => compilation.CreateIntersectionTypeSymbol());
        Assert.Throws<ArgumentNullException>(() => compilation.CreateIntersectionTypeSymbol(null!));
        Assert.Throws<ArgumentNullException>(() => compilation.CreateIntersectionTypeSymbol(a, null!));
    }

    private sealed class ConstituentCountVisitor : SymbolVisitor<int>
    {
        public override int VisitIntersectionType(IIntersectionTypeSymbol symbol) => symbol.ConstituentTypes.Length;
    }

    private static (Compilation Compilation, INamedTypeSymbol A, INamedTypeSymbol B) CreateTypes()
    {
        var compilation = CreateCompilation("""
            interface A { func First() -> int; func Shared() -> int }
            interface B { func Second() -> int; func Shared() -> int }
            """);
        return (compilation, compilation.GetTypeByMetadataName("A")!, compilation.GetTypeByMetadataName("B")!);
    }

    private static Compilation CreateCompilation(string source, bool bindDiagnosticsFirst = true)
    {
        var compilation = Compilation.Create("intersection-symbols", [SyntaxTree.ParseText(source)],
            TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        if (bindDiagnosticsFirst)
            Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        return compilation;
    }
}
