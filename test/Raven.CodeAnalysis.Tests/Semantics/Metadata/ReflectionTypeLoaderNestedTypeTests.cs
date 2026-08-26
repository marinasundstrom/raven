using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
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
    public void GetTypeMembers_PreservesAuthoredArityForRavenEmittedNestedGenericType()
    {
        const string librarySource = """
            namespace RavenNestedArityLibrary {
                public class Outer<TOuter> {
                    class Inner<TInner> {}
                }
            }
            """;
        var libraryReference = CreateLibraryReference(
            SyntaxTree.ParseText(librarySource),
            "RavenNestedArityLibrary");
        var compilation = CreateCompilation(
            references: [.. TestMetadataReferences.Default, libraryReference]);
        var outer = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("RavenNestedArityLibrary.Outer`1"));
        var inner = Assert.Single(outer.GetTypeMembers("Inner"));

        Assert.Equal(1, inner.Arity);
        Assert.Single(inner.TypeParameters);

        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var constructedOuter = Assert.IsAssignableFrom<INamedTypeSymbol>(outer.Construct(stringType));
        var projectedInner = Assert.Single(constructedOuter.GetTypeMembers("Inner"));

        Assert.Equal(1, projectedInner.Arity);
        Assert.Single(projectedInner.TypeParameters);
        Assert.Single(constructedOuter.GetTypeMembers("Inner", 1));
        Assert.Empty(constructedOuter.GetTypeMembers("Inner", 0));
    }

    private static MetadataReference CreateLibraryReference(SyntaxTree tree, string assemblyName)
    {
        var compilation = Compilation.Create(
            assemblyName,
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var emitResult = compilation.Emit(image);

        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));
        return MetadataReference.CreateFromImage(image.ToArray());
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
