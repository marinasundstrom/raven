using System.Collections.Immutable;

using Raven.CodeAnalysis.Metadata;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class MethodLookupIdentityTests
{
    [Fact]
    public void ProviderIdentityDeduplicatesViewsButKeepsDeclarationsDistinctWithoutLoadingSignatures()
    {
        var compilation = Compilation.Create("test", [], TestMetadataReferences.Default);
        var first = new ProviderMethod(compilation, "fixture:module:1");
        var sameDeclaration = new ProviderMethod(compilation, "fixture:module:1");
        var overload = new ProviderMethod(compilation, "fixture:module:2");

        Assert.Equal(first.GetShallowLookupIdentityKey(), sameDeclaration.GetShallowLookupIdentityKey());
        Assert.NotEqual(first.GetShallowLookupIdentityKey(), overload.GetShallowLookupIdentityKey());
        Assert.Equal(2, new[] { first, sameDeclaration, overload }.Select(m => m.GetShallowLookupIdentityKey()).Distinct().Count());
    }

    [Fact]
    public void ConstructedMethodsCombineProviderDeclarationIdentityWithTypeArguments()
    {
        var compilation = Compilation.Create("test", [], TestMetadataReferences.Default);
        var definition = new ProviderMethod(compilation, "fixture:generic:1");
        definition.SetTypeParameters([new SourceTypeParameterSymbol("T", definition, definition.ContainingType,
            compilation.SourceGlobalNamespace, [], [], 0, TypeParameterConstraintKind.None, [], VarianceKind.None)]);
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var first = new ConstructedMethodSymbol(definition, [intType]);
        var same = new ConstructedMethodSymbol(definition, [intType]);
        var other = new ConstructedMethodSymbol(definition, [stringType]);

        Assert.Equal(first.GetShallowLookupIdentityKey(), same.GetShallowLookupIdentityKey());
        Assert.NotEqual(first.GetShallowLookupIdentityKey(), other.GetShallowLookupIdentityKey());
    }

    [Fact]
    public void PeOverloadKeysRemainDistinctAndStableAfterSignatureResolution()
    {
        var compilation = Compilation.Create("test", [], TestMetadataReferences.Default);
        var type = compilation.GetTypeByMetadataName("System.String")!;
        var overloads = type.GetMembers("Substring").OfType<IMethodSymbol>().ToArray();
        Assert.True(overloads.Length > 1);
        var before = overloads.Select(m => m.GetShallowLookupIdentityKey()).ToArray();
        Assert.Equal(overloads.Length, before.Distinct().Count());

        foreach (var method in overloads)
        {
            Assert.NotEmpty(method.Parameters);
            Assert.NotNull(method.ReturnType);
        }

        Assert.Equal(before, overloads.Select(m => m.GetShallowLookupIdentityKey()).ToArray());
    }

    [Fact]
    public void SourceFallbackStillDistinguishesSameArityOverloadsByParameterType()
    {
        var compilation = Compilation.Create("test", [SyntaxTree.ParseText("""
            class Example {
                static func Map(value: int) -> int => value
                static func Map(value: string) -> string => value
            }
            """)], TestMetadataReferences.Default);
        var methods = compilation.GetTypeByMetadataName("Example")!.GetMembers("Map").OfType<IMethodSymbol>().ToArray();
        Assert.Equal(2, methods.Length);
        Assert.NotEqual(methods[0].GetShallowLookupIdentityKey(), methods[1].GetShallowLookupIdentityKey());
    }

    private sealed class ProviderMethod(Compilation compilation, string key)
        : SourceMethodSymbol("Map", compilation.GetSpecialType(SpecialType.System_Object), [],
            compilation.Assembly, null, compilation.SourceGlobalNamespace, [], []), IMethodLookupIdentity, IMethodSymbol
    {
        public string ShallowDeclarationLookupKey => key;
        ImmutableArray<IParameterSymbol> IMethodSymbol.Parameters
            => throw new InvalidOperationException("Shallow identity must not resolve the signature.");
    }
}
