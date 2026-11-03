using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "DotNet")]
public class PEReflectionTypeIdentityTests
{
    public static IEnumerable<object?[]> NonGenericTypes()
    {
        yield return [null, null, true];
        yield return [typeof(int), null, false];
        yield return [typeof(int), typeof(int), true];
        yield return [typeof(int), typeof(string), false];
        yield return [typeof(int[]), typeof(int[,]), false];
        yield return [typeof(int[]), typeof(string[]), false];
        yield return [typeof(int).MakeByRefType(), typeof(int), false];
        yield return [typeof(int).MakePointerType(), typeof(int), false];
    }

    [Theory]
    [MemberData(nameof(NonGenericTypes))]
    public void PreservesTypeShape(Type? left, Type? right, bool expected)
    {
        Assert.Equal(expected, PEReflectionTypeIdentity.AreEquivalent(left, right));
        Assert.Equal(expected, PEReflectionTypeIdentity.AreEquivalent(right, left));
    }

    [Fact]
    public void GenericDefinitionsAndArgumentsKeepTheirIdentity()
    {
        Assert.False(PEReflectionTypeIdentity.AreEquivalent(typeof(List<>), typeof(Dictionary<,>)));
        Assert.False(PEReflectionTypeIdentity.AreEquivalent(typeof(List<int>), typeof(List<string>)));
        Assert.False(PEReflectionTypeIdentity.AreEquivalent(typeof(List<>), typeof(List<int>)));
        Assert.False(PEReflectionTypeIdentity.AreEquivalent(typeof(List<int>), typeof(List<>)));
    }

    [Fact]
    public void EquivalentGenericTypesFromSeparateLoadContextsCompareWithoutRecursion()
    {
        var context = new System.Runtime.Loader.AssemblyLoadContext("identity-test", isCollectible: true);
        try
        {
            var original = typeof(IdentityFixture<>);
            var loaded = context.LoadFromAssemblyPath(original.Assembly.Location).GetType(original.FullName!)!;
            Assert.NotSame(original, loaded);
            Assert.True(PEReflectionTypeIdentity.AreEquivalent(original, loaded));
            Assert.True(PEReflectionTypeIdentity.AreEquivalent(original.MakeGenericType(typeof(int)), loaded.MakeGenericType(typeof(int))));
            Assert.False(PEReflectionTypeIdentity.AreEquivalent(original.MakeGenericType(typeof(int)), loaded.MakeGenericType(typeof(string))));
            Assert.True(PEReflectionTypeIdentity.AreEquivalent(original.GetGenericArguments()[0], loaded.GetGenericArguments()[0]));
            Assert.True(PEReflectionTypeIdentity.AreEquivalent(original.MakeArrayType(), loaded.MakeArrayType()));
        }
        finally
        {
            context.Unload();
        }
    }

    public class IdentityFixture<T> { }

}
