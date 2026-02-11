using System;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal static class ConstantValueEvaluator
{
    public static bool TryEvaluate(ExpressionSyntax expression, out object? value)
    {
        switch (expression)
        {
            case LiteralExpressionSyntax literal:
                return TryGetLiteralValue(literal, out value);
            case UnaryExpressionSyntax unary when unary.Kind == SyntaxKind.UnaryMinusExpression:
                if (TryEvaluate(unary.Expression, out var operand) && TryNegate(operand, out var negated))
                {
                    value = negated;
                    return true;
                }
                break;
            case UnaryExpressionSyntax unary when unary.Kind == SyntaxKind.UnaryPlusExpression:
                return TryEvaluate(unary.Expression, out value);
            case ParenthesizedExpressionSyntax parenthesized:
                return TryEvaluate(parenthesized.Expression, out value);
        }

        value = null;
        return false;
    }

    public static bool TryConvert(ITypeSymbol targetType, object? value, out object? converted)
    {
        targetType = UnwrapAlias(targetType);

        if (targetType.TypeKind == TypeKind.Error)
        {
            converted = null;
            return false;
        }

        if (targetType is LiteralTypeSymbol literalType)
            targetType = literalType.UnderlyingType;

        // Nullable<T> (or Raven's nullable wrapper). Allow null, otherwise convert to the underlying type.
        if (targetType is NullableTypeSymbol nullableType)
        {
            // null is always a valid value for a nullable type, regardless of whether the underlying type is a value type.
            if (value is null)
            {
                converted = null;
                return true;
            }

            // For non-null constants, validate/convert as the underlying type.
            return TryConvert(nullableType.UnderlyingType, value, out converted);
        }

        if (value is null)
        {
            if (!targetType.IsValueType)
            {
                converted = null;
                return true;
            }

            converted = null;
            return false;
        }

        if (targetType.SpecialType == SpecialType.System_Object)
        {
            converted = value;
            return true;
        }

        if (targetType.SpecialType == SpecialType.System_String)
        {
            if (value is string stringValue)
            {
                converted = stringValue;
                return true;
            }

            if (value is null)
            {
                converted = null;
                return true;
            }
        }

        switch (targetType.SpecialType)
        {
            case SpecialType.System_Boolean when value is bool boolValue:
                converted = boolValue;
                return true;
            case SpecialType.System_Char when value is char charValue:
                converted = charValue;
                return true;
            case SpecialType.System_SByte:
                if (TryConvertSignedIntegral(value, sbyte.MinValue, sbyte.MaxValue, out var signedSByte))
                {
                    converted = (sbyte)signedSByte;
                    return true;
                }
                break;
            case SpecialType.System_Byte:
                if (TryConvertUnsignedIntegral(value, byte.MaxValue, out var unsignedByte))
                {
                    converted = (byte)unsignedByte;
                    return true;
                }
                break;
            case SpecialType.System_Int16:
                if (TryConvertSignedIntegral(value, short.MinValue, short.MaxValue, out var signedInt16))
                {
                    converted = (short)signedInt16;
                    return true;
                }
                break;
            case SpecialType.System_UInt16:
                if (TryConvertUnsignedIntegral(value, ushort.MaxValue, out var unsignedInt16))
                {
                    converted = (ushort)unsignedInt16;
                    return true;
                }
                break;
            case SpecialType.System_Int32:
                if (TryConvertSignedIntegral(value, int.MinValue, int.MaxValue, out var signedInt32))
                {
                    converted = (int)signedInt32;
                    return true;
                }
                break;
            case SpecialType.System_UInt32:
                if (TryConvertUnsignedIntegral(value, uint.MaxValue, out var unsignedInt32))
                {
                    converted = (uint)unsignedInt32;
                    return true;
                }
                break;
            case SpecialType.System_Int64:
                if (TryConvertSignedIntegral(value, long.MinValue, long.MaxValue, out var signedInt64))
                {
                    converted = signedInt64;
                    return true;
                }
                break;
            case SpecialType.System_UInt64:
                if (TryConvertUnsignedIntegral(value, ulong.MaxValue, out var unsignedInt64))
                {
                    converted = unsignedInt64;
                    return true;
                }
                break;
            case SpecialType.System_Single:
                if (TryConvertToSingle(value, out var singleValue))
                {
                    converted = singleValue;
                    return true;
                }
                break;
            case SpecialType.System_Double:
                if (TryConvertToDouble(value, out var doubleValue))
                {
                    converted = doubleValue;
                    return true;
                }
                break;
            case SpecialType.System_Decimal:
                if (TryConvertToDecimal(value, out var decimalValue))
                {
                    converted = decimalValue;
                    return true;
                }
                break;
            case SpecialType.System_DateTime when value is DateTime dateTime:
                converted = dateTime;
                return true;
        }

        converted = null;
        return false;
    }

    private static bool TryGetLiteralValue(LiteralExpressionSyntax literal, out object? value)
    {
        if (literal.Kind == SyntaxKind.NullLiteralExpression)
        {
            value = null;
            return true;
        }

        value = literal.Token.Value;
        if (value is not null)
            return true;

        if (literal.Kind == SyntaxKind.StringLiteralExpression)
        {
            value = literal.Token.ValueText;
            return true;
        }

        return false;
    }

    private static bool TryNegate(object? operand, out object? value)
    {
        switch (operand)
        {
            case int i:
                value = -i;
                return true;
            case long l:
                value = -l;
                return true;
            case float f:
                value = -f;
                return true;
            case double d:
                value = -d;
                return true;
            case decimal m:
                value = -m;
                return true;
        }

        value = null;
        return false;
    }

    private static bool TryConvertSignedIntegral(object value, long min, long max, out long result)
    {
        switch (value)
        {
            case sbyte sb:
                result = sb;
                return result >= min && result <= max;
            case byte b:
                result = b;
                return result >= min && result <= max;
            case short s:
                result = s;
                return result >= min && result <= max;
            case ushort us:
                result = us;
                return result >= min && result <= max;
            case int i:
                result = i;
                return result >= min && result <= max;
            case uint ui:
                if (ui > long.MaxValue)
                    break;
                result = (long)ui;
                return result >= min && result <= max;
            case long l:
                result = l;
                return result >= min && result <= max;
            case ulong ul:
                if (ul > long.MaxValue)
                    break;
                result = (long)ul;
                return result >= min && result <= max;
        }

        result = default;
        return false;
    }

    private static bool TryConvertUnsignedIntegral(object value, ulong max, out ulong result)
    {
        switch (value)
        {
            case sbyte sb when sb >= 0:
                result = (ulong)sb;
                return result <= max;
            case byte b:
                result = b;
                return result <= max;
            case short s when s >= 0:
                result = (ulong)s;
                return result <= max;
            case ushort us:
                result = us;
                return result <= max;
            case int i when i >= 0:
                result = (ulong)i;
                return result <= max;
            case uint ui:
                result = ui;
                return result <= max;
            case long l when l >= 0:
                result = (ulong)l;
                return result <= max;
            case ulong ul:
                result = ul;
                return result <= max;
        }

        result = default;
        return false;
    }

    private static bool TryConvertToSingle(object value, out float result)
    {
        switch (value)
        {
            case float f:
                result = f;
                return true;
            case int i:
                result = i;
                return true;
            case long l:
                result = l;
                return true;
            case double d:
                result = (float)d;
                return true;
        }

        result = default;
        return false;
    }

    private static bool TryConvertToDouble(object value, out double result)
    {
        switch (value)
        {
            case double d:
                result = d;
                return true;
            case float f:
                result = f;
                return true;
            case int i:
                result = i;
                return true;
            case long l:
                result = l;
                return true;
        }

        result = default;
        return false;
    }

    private static bool TryConvertToDecimal(object value, out decimal result)
    {
        switch (value)
        {
            case decimal m:
                result = m;
                return true;
            case int i:
                result = i;
                return true;
            case long l:
                result = l;
                return true;
            case uint ui:
                result = ui;
                return true;
            case ulong ul:
                result = ul;
                return true;
            case float f:
                result = (decimal)f;
                return true;
            case double d:
                result = (decimal)d;
                return true;
        }

        result = default;
        return false;
    }

    private static ITypeSymbol UnwrapAlias(ITypeSymbol type)
    {
        while (true)
        {
            if (type.IsAlias && type.UnderlyingSymbol is ITypeSymbol alias)
            {
                type = alias;
                continue;
            }

            if (type is LiteralTypeSymbol literal)
            {
                type = literal.UnderlyingType;
                continue;
            }

            return type;
        }
    }
}
