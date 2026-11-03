using System.Collections.Immutable;
using System.Reflection;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "Shared")]
public sealed class TypedConstantValueTests
{
    [Fact]
    public void EqualValuesSupportTypedObjectAndOperatorEquality()
    {
        var first = TypedConstant.CreatePrimitive(null, 42);
        var second = TypedConstant.CreatePrimitive(null, 42);
        Assert.IsAssignableFrom<IEquatable<TypedConstant>>(first);
        Assert.True(first.Equals(second));
        Assert.True(first.Equals((object)second));
        Assert.True(first == second);
        Assert.False(first != second);
        Assert.Equal(first.GetHashCode(), second.GetHashCode());
        Assert.Contains(second, new HashSet<TypedConstant> { first });
        Assert.False(first.Equals(null));
        Assert.False(first.Equals(42));
    }

    [Fact]
    public void KindAndValueParticipateInEquality()
    {
        var primitive = TypedConstant.CreatePrimitive(null, 42);
        Assert.True(primitive != TypedConstant.CreatePrimitive(null, 43));
        Assert.True(primitive != TypedConstant.CreateEnum(null, 42));
        Assert.True(primitive != TypedConstant.CreatePrimitive(null, 42L));
        Assert.Equal(TypedConstantKind.Primitive, primitive.Kind);
        Assert.Equal(42, primitive.Value);
        Assert.Null(primitive.Type);
        Assert.Empty(primitive.Values);
    }

    [Fact]
    public void NullFactoriesNormalizeValuesWithoutConflatingErrors()
    {
        var absent = TypedConstant.CreateNull(null);
        Assert.True(absent.IsNull);
        Assert.Null(absent.Value);
        Assert.Equal(absent, TypedConstant.CreatePrimitive(null, null));
        Assert.Equal(absent, TypedConstant.CreateEnum(null, null));
        var error = TypedConstant.CreateError(null);
        Assert.False(error.IsNull);
        Assert.Null(error.Value);
        Assert.Equal(TypedConstantKind.Error, error.Kind);
        Assert.NotEqual(absent, error);
    }

    [Fact]
    public void ArrayEqualityPreservesImmutableArrayStorageSemantics()
    {
        var element = TypedConstant.CreatePrimitive(null, 42);
        var values = ImmutableArray.Create(element);
        var first = TypedConstant.CreateArray(null, values);
        var sameStorage = TypedConstant.CreateArray(null, values);
        var differentStorage = TypedConstant.CreateArray(null, ImmutableArray.Create(element));
        Assert.Equal(first, sameStorage);
        Assert.Equal(first.GetHashCode(), sameStorage.GetHashCode());
        Assert.NotEqual(first, differentStorage);
        Assert.Equal(element, Assert.Single(first.Values));
        Assert.Equal(TypedConstantKind.Array, first.Kind);
    }

    [Fact]
    public void WithTypePreservesBoxedPayloadAndKind()
    {
        var array = TypedConstant.CreateArray(null, [TypedConstant.CreatePrimitive(null, 42)]);
        var updated = array.WithType(null);
        Assert.Equal(array, updated);
        Assert.Same(array.Value, updated.Value);
        Assert.Equal(array.Values, updated.Values);
        var primitive = TypedConstant.CreatePrimitive(null, 42);
        Assert.Same(primitive.Value, primitive.WithType(null).Value);
    }

    [Fact]
    public void DefaultAndEmptyArraysRemainDistinct()
    {
        var uninitialized = TypedConstant.CreateArray(null, default);
        var empty = TypedConstant.CreateArray(null, ImmutableArray<TypedConstant>.Empty);
        Assert.True(uninitialized.Values.IsDefault);
        Assert.False(empty.Values.IsDefault);
        Assert.NotEqual(uninitialized, empty);
        Assert.False(default(TypedConstant).IsNull);
    }

    [Fact]
    public void ConstructionRemainsInternalAndStoredPropertiesAreInitOnly()
    {
        Assert.Empty(typeof(TypedConstant).GetConstructors(BindingFlags.Public | BindingFlags.Instance));
        foreach (var name in new[] { "Type", "Value", "Kind" })
        {
            var setter = typeof(TypedConstant).GetProperty(name)!.SetMethod;
            Assert.NotNull(setter);
            Assert.Contains(typeof(System.Runtime.CompilerServices.IsExternalInit),
                setter!.ReturnParameter.GetRequiredCustomModifiers());
        }
    }

    [Fact]
    public void RecordCopyAndDeconstructionPreserveValueAndOriginal()
    {
        var original = TypedConstant.CreatePrimitive(null, 42);
        var copy = original with { Value = 43 };
        var (type, value, kind) = copy;
        Assert.Null(type);
        Assert.Equal(43, value);
        Assert.Equal(TypedConstantKind.Primitive, kind);
        Assert.Equal(42, original.Value);
        Assert.NotEqual(original, copy);
        Assert.Equal("Raven.CodeAnalysis.TypedConstant", original.ToString());
    }

}
