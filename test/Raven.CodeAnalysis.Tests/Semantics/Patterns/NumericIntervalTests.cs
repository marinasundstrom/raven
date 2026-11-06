namespace Raven.CodeAnalysis.Semantics.Tests;

[Trait("Target", "Shared")]
public sealed class NumericIntervalTests
{
    [Fact]
    public void DomainsAndConstantsAgreeAtIntegralLimits()
    {
        (SpecialType Type, object Low, object High)[] cases =
        [
            (SpecialType.System_SByte, sbyte.MinValue, sbyte.MaxValue),
            (SpecialType.System_Byte, byte.MinValue, byte.MaxValue),
            (SpecialType.System_Int16, short.MinValue, short.MaxValue),
            (SpecialType.System_UInt16, ushort.MinValue, ushort.MaxValue),
            (SpecialType.System_Int32, int.MinValue, int.MaxValue),
            (SpecialType.System_UInt32, uint.MinValue, uint.MaxValue),
            (SpecialType.System_Int64, long.MinValue, long.MaxValue),
            (SpecialType.System_UInt64, ulong.MinValue, ulong.MaxValue),
            (SpecialType.System_Char, char.MinValue, char.MaxValue)
        ];
        foreach (var (type, low, high) in cases)
        {
            Assert.True(NumericInterval.TryGetDomain(type, out var domain));
            Assert.True(NumericInterval.TryGetValue(low, out var minimum));
            Assert.True(NumericInterval.TryGetValue(high, out var maximum));
            Assert.Equal(new NumericInterval(minimum, maximum), domain);
        }
        Assert.True(NumericInterval.TryGetDomain(SpecialType.System_UInt64, out var unsigned));
        Assert.Equal((Int128)ulong.MaxValue, unsigned.High);
        Assert.False(NumericInterval.TryGetDomain(SpecialType.System_Double, out _));
        Assert.False(NumericInterval.TryGetValue(0.5, out _));
        Assert.False(NumericInterval.TryGetValue(null, out _));
    }

    [Fact]
    public void SubtractionPreservesOnlyUncoveredValues()
    {
        var domain = new NumericInterval(1, 10);
        Assert.Equal([domain], domain.Subtract(new(7, 3)));
        Assert.Equal([domain], domain.Subtract(new(20, 30)));
        Assert.Equal([new(1, 2), new NumericInterval(8, 10)], domain.Subtract(new(3, 7)));
        Assert.Empty(domain.Subtract(new(0, 11)));
        Assert.Empty(new NumericInterval(5, 4).Subtract(domain));
    }
}
