using System.Linq;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class IntersectionMemberLookupTests : CompilationTestBase
{
    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void InheritedMembers_DeduplicateDiamondDeclarations(bool reverse, bool diagnosticsFirst)
    {
        var (compilation, _) = CreateCompilation("""
            interface Root { func Shared() -> int }
            interface A: Root { func First() -> int }
            interface B: Root { func Second() -> int }
            """);
        if (diagnosticsFirst)
            AssertNoErrors(compilation);
        var type = Intersection(compilation, reverse);
        Assert.Equal("Root", Assert.Single(Lookup(compilation, type, "Shared")).ContainingType?.Name);
        Assert.Equal("A", Assert.Single(Lookup(compilation, type, "First")).ContainingType?.Name);
        Assert.Equal("B", Assert.Single(Lookup(compilation, type, "Second")).ContainingType?.Name);
        Assert.Equal(SpecialType.System_Object, Assert.Single(Lookup(compilation, type, "GetHashCode")).ContainingType?.SpecialType);
        AssertNoErrors(compilation);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void DistinctDeclarations_RetainAmbiguityRegardlessOfOrder(bool reverse)
    {
        var (compilation, _) = CreateCompilation("""
            interface A { func Shared() -> int; val Value: int }
            interface B { func Shared() -> int; val Value: int }
            """);
        AssertNoErrors(compilation);
        var type = Intersection(compilation, reverse);
        var methods = Lookup(compilation, type, "Shared").OfType<IMethodSymbol>().ToArray();
        Assert.Equal(2, methods.Length);
        Assert.Equal(2, Lookup(compilation, type, "Value").Length);
        var result = OverloadResolver.ResolveOverload(methods, [], compilation);
        Assert.True(result.IsAmbiguous);
        Assert.Null(result.Method);
        Assert.Equal(2, result.AmbiguousCandidates.Length);
    }

    [Fact]
    public void UnrelatedInheritedDeclarations_AreNotMergedBySignature()
    {
        var (compilation, _) = CreateCompilation("""
            interface Left { func Shared() -> int }
            interface Right { func Shared() -> string }
            interface A: Left, Right {}
            interface B {}
            """);
        AssertNoErrors(compilation);
        var members = Lookup(compilation, Intersection(compilation), "Shared");
        Assert.Equal(new[] { "Left", "Right" }, members.Select(m => m.ContainingType!.Name).OrderBy(n => n));
    }

    [Fact]
    public void DerivedDeclaration_HidesItsOwnInheritedSignature()
    {
        var (compilation, _) = CreateCompilation("""
            interface Root { func Shared() -> int }
            interface A: Root { func Shared() -> string }
            interface B {}
            """);
        AssertNoErrors(compilation);
        Assert.Equal("A", Assert.Single(Lookup(compilation, Intersection(compilation), "Shared")).ContainingType?.Name);
    }

    [Fact]
    public void ClassMembers_AreInheritedWithoutLeakingExplicitImplementations()
    {
        var (compilation, _) = CreateCompilation("""
            interface Contract { func Hidden() -> int }
            open class Parent { func Inherited() -> int => 1 }
            class A: Parent, Contract { func Contract.Hidden() -> int => 2 }
            interface B {}
            """);
        AssertNoErrors(compilation);
        var type = Intersection(compilation);
        Assert.Equal("Parent", Assert.Single(Lookup(compilation, type, "Inherited")).ContainingType?.Name);
        Assert.Empty(Lookup(compilation, type, "Hidden"));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void DistinctOverloads_UseNormalArgumentResolution(bool reverse)
    {
        var (compilation, _) = CreateCompilation("""
            interface A { func Pick(value: int) -> int }
            interface B { func Pick(value: string) -> int }
            """);
        AssertNoErrors(compilation);
        var methods = Lookup(compilation, Intersection(compilation, reverse), "Pick").OfType<IMethodSymbol>();
        var argument = new BoundArgument(new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral,
            1, compilation.GetSpecialType(SpecialType.System_Int32)), RefKind.None, name: null);
        var result = OverloadResolver.ResolveOverload(methods, [argument], compilation);
        Assert.True(result.Success);
        Assert.Equal("A", result.Method?.ContainingType?.Name);
    }

    [Fact]
    public void ImportedInterfaceMembers_IncludeInheritedDeclarations()
    {
        var (compilation, _) = CreateCompilation("interface Marker {}");
        var enumerator = compilation.GetTypeByMetadataName("System.Collections.Generic.IEnumerator`1")!
            .Construct(compilation.GetSpecialType(SpecialType.System_Int32));
        var type = compilation.CreateIntersectionTypeSymbol(enumerator, compilation.GetTypeByMetadataName("Marker")!);
        var moveNext = Assert.Single(Lookup(compilation, type, "MoveNext"));
        Assert.True(SymbolEqualityComparer.Default.Equals(
            compilation.GetTypeByMetadataName("System.Collections.IEnumerator"), moveNext.ContainingType));
        Assert.Single(Lookup(compilation, type, "Dispose"));
        var current = Assert.IsAssignableFrom<IPropertySymbol>(Assert.Single(Lookup(compilation, type, "Current")));
        Assert.Equal(SpecialType.System_Int32, current.Type.SpecialType);
    }

    [Fact]
    public void ObjectMembers_AreFallbackRatherThanCompetingClassCandidates()
    {
        var (compilation, _) = CreateCompilation("""
            class A {}
            interface B { func ToString() -> string }
            """);
        AssertNoErrors(compilation);
        Assert.Equal("B", Assert.Single(Lookup(compilation, Intersection(compilation), "ToString")).ContainingType?.Name);
    }

    [Fact]
    public void Intersection_DoesNotIntroduceStaticDispatch()
    {
        var (compilation, _) = CreateCompilation("""
            interface A { static func Create() -> int }
            interface B { func Run() -> int }
            """);
        AssertNoErrors(compilation);
        var type = Intersection(compilation);
        Assert.Empty(Lookup(compilation, type, "Create"));
        Assert.Empty(new SymbolQuery("Create", type, IsStatic: true).Lookup(compilation.GlobalBinder));
        Assert.Single(Lookup(compilation, type, "Run"));
    }

    private static ITypeSymbol Intersection(Compilation compilation, bool reverse = false)
    {
        var a = compilation.GetTypeByMetadataName("A")!;
        var b = compilation.GetTypeByMetadataName("B")!;
        return reverse ? compilation.CreateIntersectionTypeSymbol(b, a) : compilation.CreateIntersectionTypeSymbol(a, b);
    }

    private static ISymbol[] Lookup(Compilation compilation, ITypeSymbol type, string name)
        => new SymbolQuery(name, type, IsStatic: false).Lookup(compilation.GlobalBinder).ToArray();

    private static void AssertNoErrors(Compilation compilation)
        => Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
}
