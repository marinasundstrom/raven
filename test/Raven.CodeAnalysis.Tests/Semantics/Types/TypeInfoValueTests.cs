namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "Shared")]
public sealed class TypeInfoValueTests
{
    [Fact]
    public void MissingTypesPreserveTheDefaultConversion()
    {
        var info = new TypeInfo(null, null);
        Assert.Null(info.Type);
        Assert.Null(info.ConvertedType);
        Assert.Equal(Conversion.None, info.Conversion);
    }

    [Fact]
    public void EqualResultsAreDistinctObjectsWithValueEquality()
    {
        var left = new TypeInfo(null, null, new Conversion(isImplicit: true, isNumeric: true));
        var right = new TypeInfo(null, null, new Conversion(isImplicit: true, isNumeric: true));
        Assert.NotSame(left, right);
        var equatable = Assert.IsAssignableFrom<IEquatable<TypeInfo>>(left);
        Assert.True(equatable.Equals(right));
        Assert.Equal(left, right);
        Assert.Equal(left.GetHashCode(), right.GetHashCode());
        Assert.Single(new HashSet<TypeInfo> { left, right });
    }

    [Fact]
    public void ConversionClassificationParticipatesInValueEquality()
    {
        var missing = new TypeInfo(null, null);
        var explicitResult = new TypeInfo(null, null, new Conversion(isImplicit: false));
        Assert.NotEqual(missing, explicitResult);
        Assert.False(missing.Equals(null));
        Assert.False(missing.Equals("TypeInfo"));
    }
}
