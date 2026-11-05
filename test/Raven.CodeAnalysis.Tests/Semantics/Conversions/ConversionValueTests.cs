namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "Shared")]
public sealed class ConversionValueTests
{
    [Fact]
    public void NoConversionIsNeitherImplicitNorExplicit()
    {
        foreach (var conversion in new[] { default(Conversion), new Conversion(), Conversion.None })
        {
            Assert.False(conversion.Exists);
            Assert.False(conversion.IsImplicit);
            Assert.False(conversion.IsExplicit);
            Assert.Equal(Conversion.None, conversion.WithAlias(true));
        }
    }

    [Fact]
    public void ExistingConversionsPreserveTheirClassification()
    {
        var implicitConversion = new Conversion(isImplicit: true);
        Assert.True(implicitConversion.Exists);
        Assert.True(implicitConversion.IsImplicit);
        Assert.False(implicitConversion.IsExplicit);
        var explicitConversion = new Conversion(isImplicit: false);
        Assert.True(explicitConversion.Exists);
        Assert.False(explicitConversion.IsImplicit);
        Assert.True(explicitConversion.IsExplicit);
        Assert.NotEqual(Conversion.None, explicitConversion);
    }

    [Fact]
    public void FlagsParticipateInValueEquality()
    {
        Conversion[] values = [Conversion.None, new(isImplicit: false), new(isImplicit: true),
            new(isIdentity: true), new(isNumeric: true), new(isReference: true),
            new(isBoxing: true), new(isUnboxing: true), new(isPointer: true),
            new(isDiscriminatedUnion: true), new(isLifted: true), new(isUserDefined: true), new(isAlias: true)];
        Assert.Equal(values.Length, new HashSet<Conversion>(values).Count);
        for (var left = 0; left < values.Length; left++)
            for (var right = 0; right < values.Length; right++)
            {
                Assert.Equal(left == right, values[left] == values[right]);
                Assert.Equal(left != right, values[left] != values[right]);
                Assert.Equal(left == right, values[left].Equals((object)values[right]));
            }
    }

    [Fact]
    public void GenericEqualityMatchesTheExistingValueContract()
    {
        var conversion = new Conversion(isImplicit: true, isNumeric: true);
        var equality = Assert.IsAssignableFrom<IEquatable<Conversion>>(conversion);
        Assert.True(equality.Equals(new Conversion(isImplicit: true, isNumeric: true)));
        Assert.False(equality.Equals(Conversion.None));
    }

    [Fact]
    public void AliasMarkingPreservesOtherFlagsAndCannotRemoveAnAlias()
    {
        var original = new Conversion(isImplicit: true, isNumeric: true, isLifted: true);
        var aliased = original.WithAlias(true);
        var expected = new Conversion(isImplicit: true, isNumeric: true, isLifted: true, isAlias: true);
        Assert.Equal(expected, aliased);
        Assert.Equal(expected.GetHashCode(), aliased.GetHashCode());
        Assert.Equal(aliased, aliased.WithAlias(false));
        Assert.Equal(original, original.WithAlias(false));
        Assert.False(original.IsAlias);
    }
}
