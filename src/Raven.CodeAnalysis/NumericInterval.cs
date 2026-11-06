using System;
using System.Collections.Generic;

namespace Raven.CodeAnalysis;

/// <summary>Inclusive integral bounds for pattern coverage, with room beyond either 64-bit endpoint.</summary>
internal readonly record struct NumericInterval(Int128 Low, Int128 High)
{
    public bool IsEmpty => Low > High;

    public static bool TryGetDomain(SpecialType type, out NumericInterval domain)
    {
        switch (type)
        {
            case SpecialType.System_SByte:
                domain = new(sbyte.MinValue, sbyte.MaxValue);
                return true;
            case SpecialType.System_Byte:
                domain = new(byte.MinValue, byte.MaxValue);
                return true;
            case SpecialType.System_Int16:
                domain = new(short.MinValue, short.MaxValue);
                return true;
            case SpecialType.System_UInt16:
                domain = new(ushort.MinValue, ushort.MaxValue);
                return true;
            case SpecialType.System_Int32:
                domain = new(int.MinValue, int.MaxValue);
                return true;
            case SpecialType.System_UInt32:
                domain = new(uint.MinValue, uint.MaxValue);
                return true;
            case SpecialType.System_Int64:
                domain = new(long.MinValue, long.MaxValue);
                return true;
            case SpecialType.System_UInt64:
                domain = new(ulong.MinValue, ulong.MaxValue);
                return true;
            case SpecialType.System_Char:
                domain = new(char.MinValue, char.MaxValue);
                return true;
            default:
                domain = default;
                return false;
        }
    }

    public static bool TryGetValue(object? constant, out Int128 value)
    {
        switch (constant)
        {
            case sbyte number: value = number; return true;
            case byte number: value = number; return true;
            case short number: value = number; return true;
            case ushort number: value = number; return true;
            case int number: value = number; return true;
            case uint number: value = number; return true;
            case long number: value = number; return true;
            case ulong number: value = number; return true;
            case char number: value = number; return true;
            default: value = default; return false;
        }
    }

    public IReadOnlyList<NumericInterval> Subtract(NumericInterval other)
    {
        if (IsEmpty)
            return [];
        if (other.IsEmpty || other.High < Low || other.Low > High)
            return [this];

        if (other.Low <= Low && other.High >= High)
            return [];

        var result = new List<NumericInterval>(2);
        if (other.Low > Low)
            result.Add(new(Low, other.Low - 1));
        if (other.High < High)
            result.Add(new(other.High + 1, High));
        return result;
    }

    public override string ToString() => Low == High ? $"{Low}" : $"{Low}..{High}";
}
