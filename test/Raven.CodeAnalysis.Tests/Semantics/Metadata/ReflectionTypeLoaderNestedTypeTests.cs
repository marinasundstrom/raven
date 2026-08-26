using System;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Tests;

public static class NestedArityMetadataFixtures
{
    public class Outer
    {
        public class Callback
        {
        }

        public class Callback<T>
        {
        }

        public class MarkerAttribute : System.Attribute
        {
        }

        public class MarkerAttribute<T> : System.Attribute
        {
        }
    }
}

public class ReflectionTypeLoaderNestedTypeTests : CompilationTestBase
{
    protected override MetadataReference[] GetMetadataReferences()
        => [.. TestMetadataReferences.Default, MetadataReference.CreateFromFile(typeof(ReflectionTypeLoaderNestedTypeTests).Assembly.Location)];

    [Fact]
    public void GetTypeMembers_FiltersNestedMetadataTypesByArity()
    {
        var compilation = CreateCompilation();
        var outer = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName(typeof(NestedArityMetadataFixtures.Outer).FullName!));

        Assert.Equal(0, Assert.Single(outer.GetTypeMembers("Callback", 0)).Arity);
        Assert.Equal(1, Assert.Single(outer.GetTypeMembers("Callback", 1)).Arity);
        Assert.Empty(outer.GetTypeMembers("Callback", 2));
    }

    [Fact]
    public void ResolveNestedTypeChain_PreservesContainingTypeForNonGenericInner()
    {
        var compilation = CreateCompilation();
        compilation.EnsureSetup();

        var resolver = compilation.ReflectionTypeLoader;
        var runtimeInner = typeof(ReflectionTypeLoaderNestedTypeFixtures.Outer<int>.Inner);

        var resolved = Assert.IsAssignableFrom<INamedTypeSymbol>(resolver.ResolveType(runtimeInner));
        var containing = Assert.IsAssignableFrom<INamedTypeSymbol>(resolved.ContainingType);
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);

        Assert.True(SymbolEqualityComparer.Default.Equals(intType, containing.TypeArguments[0]));
        Assert.True(SymbolEqualityComparer.Default.Equals(containing, resolved.ContainingSymbol));

        var nestedMember = Assert.Single(containing.GetMembers("Inner").OfType<INamedTypeSymbol>());
        Assert.True(SymbolEqualityComparer.Default.Equals(resolved, nestedMember));
    }

    [Fact]
    public void ResolveNestedTypeChain_BindsNestedTypeArguments()
    {
        var compilation = CreateCompilation();
        compilation.EnsureSetup();

        var resolver = compilation.ReflectionTypeLoader;
        var runtimeInner = typeof(ReflectionTypeLoaderNestedTypeFixtures.Outer<int>.InnerWith<string>);

        var resolved = Assert.IsAssignableFrom<INamedTypeSymbol>(resolver.ResolveType(runtimeInner));
        var containing = Assert.IsAssignableFrom<INamedTypeSymbol>(resolved.ContainingType);

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var stringType = compilation.GetSpecialType(SpecialType.System_String);

        Assert.Equal("InnerWith", resolved.Name);
        Assert.Equal(1, resolved.Arity);
        Assert.True(SymbolEqualityComparer.Default.Equals(stringType, resolved.TypeArguments[0]));
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, containing.TypeArguments[0]));
        Assert.True(SymbolEqualityComparer.Default.Equals(containing, resolved.ContainingSymbol));

        var nestedMember = Assert.Single(containing.GetMembers("InnerWith").OfType<INamedTypeSymbol>());
        Assert.Equal("InnerWith", nestedMember.Name);
        Assert.True(SymbolEqualityComparer.Default.Equals(containing, nestedMember.ContainingType));
    }

    internal static class ReflectionTypeLoaderNestedTypeFixtures
    {
        public class Outer<T>
        {
            public class Inner
            {
            }

            public class InnerWith<U>
            {
            }
        }
    }
}
