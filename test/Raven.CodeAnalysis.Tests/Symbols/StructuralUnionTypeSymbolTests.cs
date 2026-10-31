using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Symbols;

public sealed class StructuralUnionTypeSymbolTests
{
    [Fact]
    public void ValueAlternative_BoxesExactlyAndExtractsExplicitly()
    {
        var c = Create();
        var value = c.GetSpecialType(SpecialType.System_Int32);
        var reference = c.GetTypeByMetadataName("First")!;
        var union = c.CreateStructuralUnionTypeSymbol(value, reference);
        Assert.True(union.IsValueType);
        Assert.False(union.IsReferenceType);
        var conversion = c.ClassifyConversion(value, union);
        Assert.True(conversion.IsImplicit && conversion.IsBoxing);
        Assert.False(conversion.IsReference);
        Assert.True(c.ClassifyConversion(reference, union).IsReference);
        var extraction = c.ClassifyConversion(union, value);
        Assert.True(extraction.IsExplicit && extraction.IsUnboxing);
        Assert.False(c.ClassifyConversion(c.GetSpecialType(SpecialType.System_Int64), union).Exists);
        var wider = c.CreateStructuralUnionTypeSymbol(value, reference, c.GetTypeByMetadataName("Second")!);
        Assert.True(c.ClassifyConversion(union, wider).IsImplicit);
        Assert.False(c.ClassifyConversion(wider, union).Exists);
    }

    [Fact]
    public void ReferenceAlternative_ExtractsExplicitlyWithoutWeakeningMembership()
    {
        var c = Create();
        var first = c.GetTypeByMetadataName("First")!;
        var second = c.GetTypeByMetadataName("Second")!;
        var a = c.GetTypeByMetadataName("A")!;
        foreach (var other in new[] { c.GetSpecialType(SpecialType.System_Int32), second })
        {
            var union = c.CreateStructuralUnionTypeSymbol(first, other);
            var extraction = c.ClassifyConversion(union, first);
            Assert.True(extraction.Exists && extraction.IsExplicit && extraction.IsReference);
            Assert.False(extraction.IsBoxing || extraction.IsUnboxing);
            Assert.False(c.ClassifyConversion(union, c.GetSpecialType(SpecialType.System_String)).Exists);
        }
        Assert.True(c.ClassifyConversion(c.CreateStructuralUnionTypeSymbol(first, a), a).IsImplicit);
    }

    [Fact]
    public void Identity_FlatteningDisplayAndVisitor_DoNotCreateNominalCarrier()
    {
        var c = Create();
        var a = c.GetTypeByMetadataName("A")!;
        var b = c.GetTypeByMetadataName("B")!;
        var u = Assert.IsAssignableFrom<IStructuralUnionTypeSymbol>(c.CreateStructuralUnionTypeSymbol(a, b));
        var reversed = c.CreateStructuralUnionTypeSymbol(b, u, a);
        Assert.True(SymbolEqualityComparer.Default.Equals(u, reversed));
        Assert.Equal(SymbolEqualityComparer.Default.GetHashCode(u), SymbolEqualityComparer.Default.GetHashCode(reversed));
        Assert.False(SymbolEqualityComparer.Default.Equals(u, c.CreateIntersectionTypeSymbol(a, b)));
        Assert.Same(a, c.CreateStructuralUnionTypeSymbol(a, a));
        Assert.Equal("A | B", u.ToDisplayString());
        Assert.Empty(u.MetadataName);
        Assert.Null(u.ContainingSymbol);
        Assert.True(u.IsValueType);
        Assert.False(u.IsReferenceType);
        Assert.Null(u.BaseType);
        Assert.Empty(u.GetMembers());
        Assert.Equal(2, u.Accept(new Count()));
        Assert.False(CallableSignature.TryType(u, false, out _));
        Assert.IsAssignableFrom<IStructuralUnionTypeSymbol>(c.CreateStructuralUnionTypeSymbol(c.GetSpecialType(SpecialType.System_Int32), a));
        Assert.Throws<System.ArgumentException>(() => c.CreateStructuralUnionTypeSymbol(c.CreateIntersectionTypeSymbol(a, b), a));
    }

    [Fact]
    public void ReferenceMembership_RequiresEveryAlternativeAndEveryBound()
    {
        var c = Create();
        var a = c.GetTypeByMetadataName("A")!;
        var b = c.GetTypeByMetadataName("B")!;
        var first = c.GetTypeByMetadataName("First")!;
        var second = c.GetTypeByMetadataName("Second")!;
        var union = c.CreateStructuralUnionTypeSymbol(first, second);
        var intersection = c.CreateIntersectionTypeSymbol(a, b);
        var broad = c.CreateStructuralUnionTypeSymbol(a, b);
        Assert.True(c.ClassifyConversion(union, intersection).IsImplicit);
        Assert.True(c.ClassifyConversion(union, intersection).IsReference);
        Assert.True(c.ClassifyConversion(intersection, broad).IsImplicit);
        Assert.True(c.ClassifyConversion(union, broad).IsImplicit);
        Assert.False(c.ClassifyConversion(broad, intersection).Exists);
        Assert.False(c.ClassifyConversion(broad, union).Exists);
        Assert.False(c.ClassifyConversion(c.GetSpecialType(SpecialType.System_Int32), broad).Exists);
    }

    [Fact]
    public void ClosedGenericBounds_PreserveNominalArgumentsAndReferenceConversions()
    {
        var c = Create();
        var enumerable = c.GetTypeByMetadataName("System.Collections.Generic.IEnumerable`1")!;
        var list = c.GetTypeByMetadataName("System.Collections.Generic.List`1")!;
        var text = c.GetSpecialType(SpecialType.System_String);
        var strings = enumerable.Construct(text);
        var objects = enumerable.Construct(c.GetSpecialType(SpecialType.System_Object));
        var listOfStrings = list.Construct(text);
        var a = c.GetTypeByMetadataName("A")!;
        var union = c.CreateStructuralUnionTypeSymbol(strings, listOfStrings);
        var intersection = c.CreateIntersectionTypeSymbol(strings, a);
        Assert.IsAssignableFrom<IStructuralUnionTypeSymbol>(union);
        var animal = c.GetTypeByMetadataName("Animal`1")!.Construct(text);
        var dog = c.GetTypeByMetadataName("Dog`1")!.Construct(text);
        Assert.IsAssignableFrom<IIntersectionTypeSymbol>(c.CreateIntersectionTypeSymbol(animal, a));
        Assert.Throws<System.ArgumentException>(() => c.CreateIntersectionTypeSymbol(dog, a));
        Assert.IsAssignableFrom<IStructuralUnionTypeSymbol>(c.CreateStructuralUnionTypeSymbol(dog, a));
        Assert.IsAssignableFrom<IIntersectionTypeSymbol>(intersection);
        Assert.True(c.ClassifyConversion(union, objects).IsReference);
        Assert.True(c.ClassifyConversion(intersection, objects).IsReference);
        Assert.Throws<System.ArgumentException>(() => c.CreateStructuralUnionTypeSymbol(enumerable, a));
        Assert.IsAssignableFrom<IStructuralUnionTypeSymbol>(c.CreateStructuralUnionTypeSymbol(
            enumerable.Construct(enumerable.TypeParameters.Cast<ITypeSymbol>().ToArray()), a));
        Assert.IsAssignableFrom<IIntersectionTypeSymbol>(c.CreateIntersectionTypeSymbol(enumerable, a));
    }

    private sealed class Count : SymbolVisitor<int>
    {
        public override int VisitStructuralUnionType(IStructuralUnionTypeSymbol symbol) => symbol.ConstituentTypes.Length;
    }
    private static Compilation Create()
    {
        var c = Compilation.Create("structural-union-symbols", [SyntaxTree.ParseText("""
            interface A {}
            interface B {}
            class First: A, B {}
            class Second: A, B {}
            open class Animal<T> {}
            class Dog<T> {}
            """)], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(c.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        return c;
    }
}
