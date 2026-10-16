using System;
using System.Numerics;

namespace Raven.CodeAnalysis;

// Evaluate resolved built-in operations only. Never execute user-defined operators.
internal static class BoundNumericConstantEvaluator
{
    internal enum Status { NotConstant, Constant, Overflow, DivisionByZero }

    internal static Status Evaluate(BoundExpression expression, Func<BoundExpression, bool> isChecked, out object? value)
    {
        value = null;
        switch (expression)
        {
            case BoundLiteralExpression literal:
                value = literal.Value;
                return Status.Constant;
            case BoundLocalAccess { Local.IsConst: true } local:
                value = local.Local.ConstantValue;
                return Status.Constant;
            case BoundFieldAccess { Field.IsConst: true } field:
                value = field.Field.GetConstantValue();
                return Status.Constant;
            case BoundParenthesizedExpression parenthesized:
                return Evaluate(parenthesized.Expression, isChecked, out value);
            case BoundConversionExpression conversion when !conversion.IsUserDefined:
                var converted = Evaluate(conversion.Expression, isChecked, out var operand);
                if (converted != Status.Constant) return converted;
                if (conversion.IsIdentity) { value = operand; return Status.Constant; }
                if (!conversion.Conversion.IsNumeric) return Status.NotConstant;
                return ConvertNumber(operand, conversion.Type.SpecialType, isChecked(conversion), out value);
            case BoundUnaryExpression unary:
                var unaryStatus = Evaluate(unary.Operand, isChecked, out var unaryValue);
                if (unaryStatus != Status.Constant) return unaryStatus;
                if (unary.Operator.OperatorKind == BoundUnaryOperatorKind.UnaryPlus) { value = unaryValue; return Status.Constant; }
                if (unary.Operator.OperatorKind == BoundUnaryOperatorKind.BitwiseNot && TryInteger(unaryValue, out var complemented))
                    return IntegerResult(~complemented, unary.Type.SpecialType, false, out value);
                if (unary.Operator.OperatorKind is not (BoundUnaryOperatorKind.UnaryMinus or BoundUnaryOperatorKind.CheckedUnaryMinus))
                    return Status.NotConstant;
                if (TryInteger(unaryValue, out var integer))
                    return IntegerResult(-integer, unary.Type.SpecialType, isChecked(unary), out value);
                if (unaryValue is double d) { value = -d; return Status.Constant; }
                if (unaryValue is float f) { value = -f; return Status.Constant; }
                if (unaryValue is decimal m) { value = -m; return Status.Constant; }
                return Status.NotConstant;
            case BoundBinaryExpression binary when binary.Operator.MethodSymbol is null:
                var kind = binary.Operator.OperatorKind & ~BinaryOperatorKind.Checked;
                if (kind is not (BinaryOperatorKind.Addition or BinaryOperatorKind.Subtraction or BinaryOperatorKind.Multiplication or
                    BinaryOperatorKind.Division or BinaryOperatorKind.Modulo or BinaryOperatorKind.BitwiseAnd or BinaryOperatorKind.BitwiseOr or BinaryOperatorKind.BitwiseXor or
                    BinaryOperatorKind.ShiftLeft or BinaryOperatorKind.ShiftRight or BinaryOperatorKind.UnsignedShiftRight))
                    return Status.NotConstant;
                var leftStatus = Evaluate(binary.Left, isChecked, out var left);
                if (leftStatus != Status.Constant) return leftStatus;
                var rightStatus = Evaluate(binary.Right, isChecked, out var right);
                if (rightStatus != Status.Constant) return rightStatus;
                if (TryInteger(left, out var a) && TryInteger(right, out var b) && IsIntegral(binary.Type.SpecialType))
                {
                    if (kind is BinaryOperatorKind.Division or BinaryOperatorKind.Modulo && b.IsZero)
                        return Status.DivisionByZero;
                    var bits = binary.Type.SpecialType is SpecialType.System_Int64 or SpecialType.System_UInt64 ? 64 : 32;
                    var shift = (int)(b & (bits - 1));
                    var result = kind switch
                    {
                        BinaryOperatorKind.Addition => a + b,
                        BinaryOperatorKind.Subtraction => a - b,
                        BinaryOperatorKind.Multiplication => a * b,
                        BinaryOperatorKind.Division => a / b,
                        BinaryOperatorKind.Modulo => a % b,
                        BinaryOperatorKind.BitwiseAnd => a & b,
                        BinaryOperatorKind.BitwiseOr => a | b,
                        BinaryOperatorKind.BitwiseXor => a ^ b,
                        BinaryOperatorKind.ShiftLeft => a << shift,
                        BinaryOperatorKind.ShiftRight => a >> shift,
                        _ => (a & ((BigInteger.One << bits) - 1)) >> shift,
                    };
                    // Shifts discard bits even in checked contexts.
                    var check = kind is BinaryOperatorKind.ShiftLeft or BinaryOperatorKind.ShiftRight or BinaryOperatorKind.UnsignedShiftRight
                        ? false : isChecked(binary);
                    return IntegerResult(result, binary.Type.SpecialType, check, out value);
                }
                if (left is null || right is null) return Status.NotConstant;
                try
                {
                    if (binary.Type.SpecialType == SpecialType.System_Decimal)
                    {
                        var aDecimal = System.Convert.ToDecimal(left);
                        var bDecimal = System.Convert.ToDecimal(right);
                        value = kind switch
                        {
                            BinaryOperatorKind.Addition => aDecimal + bDecimal,
                            BinaryOperatorKind.Subtraction => aDecimal - bDecimal,
                            BinaryOperatorKind.Multiplication => aDecimal * bDecimal,
                            BinaryOperatorKind.Division => aDecimal / bDecimal,
                            _ => aDecimal % bDecimal,
                        };
                        return Status.Constant;
                    }
                    if (binary.Type.SpecialType is SpecialType.System_Single or SpecialType.System_Double)
                    {
                        var aDouble = System.Convert.ToDouble(left);
                        var bDouble = System.Convert.ToDouble(right);
                        var result = kind switch
                        {
                            BinaryOperatorKind.Addition => aDouble + bDouble,
                            BinaryOperatorKind.Subtraction => aDouble - bDouble,
                            BinaryOperatorKind.Multiplication => aDouble * bDouble,
                            BinaryOperatorKind.Division => aDouble / bDouble,
                            _ => aDouble % bDouble,
                        };
                        value = binary.Type.SpecialType == SpecialType.System_Single ? (object)(float)result : result;
                        return Status.Constant;
                    }
                }
                catch (OverflowException) { return Status.Overflow; }
                catch (DivideByZeroException) { return Status.DivisionByZero; }
                return Status.NotConstant;
            default:
                return Status.NotConstant;
        }
    }

    private static Status ConvertNumber(object? operand, SpecialType target, bool isChecked, out object? value)
    {
        value = null;
        if (operand is null) return Status.NotConstant;
        if (IsIntegral(target))
        {
            if (TryInteger(operand, out var integer))
                return IntegerResult(integer, target, isChecked, out value);
            if (operand is decimal dec)
                return IntegerResult(new BigInteger(decimal.Truncate(dec)), target, true, out value);
            if (operand is double or float)
            {
                var number = System.Convert.ToDouble(operand);
                if (!double.IsFinite(number)) return isChecked ? Status.Overflow : Status.NotConstant;
                var result = IntegerResult(new BigInteger(number), target, true, out value);
                // Out-of-range unchecked floating conversions are backend-dependent.
                return result == Status.Overflow && !isChecked ? Status.NotConstant : result;
            }
        }
        try
        {
            switch (target)
            {
                case SpecialType.System_Single: value = System.Convert.ToSingle(operand); return Status.Constant;
                case SpecialType.System_Double: value = System.Convert.ToDouble(operand); return Status.Constant;
                case SpecialType.System_Decimal: value = System.Convert.ToDecimal(operand); return Status.Constant;
            }
        }
        catch (OverflowException) { return Status.Overflow; }
        return Status.NotConstant;
    }

    private static bool IsIntegral(SpecialType type) => type is SpecialType.System_SByte or SpecialType.System_Byte or
        SpecialType.System_Int16 or SpecialType.System_UInt16 or SpecialType.System_Int32 or SpecialType.System_UInt32 or
        SpecialType.System_Int64 or SpecialType.System_UInt64;

    private static bool TryInteger(object? value, out BigInteger number)
    {
        switch (value)
        {
            case sbyte n: number = n; return true;
            case byte n: number = n; return true;
            case short n: number = n; return true;
            case ushort n: number = n; return true;
            case int n: number = n; return true;
            case uint n: number = n; return true;
            case long n: number = n; return true;
            case ulong n: number = n; return true;
            default: number = default; return false;
        }
    }

    private static Status IntegerResult(BigInteger number, SpecialType target, bool isChecked, out object? value)
    {
        value = null;
        var (bits, signed) = target switch
        {
            SpecialType.System_SByte => (8, true),
            SpecialType.System_Byte => (8, false),
            SpecialType.System_Int16 => (16, true),
            SpecialType.System_UInt16 => (16, false),
            SpecialType.System_Int32 => (32, true),
            SpecialType.System_UInt32 => (32, false),
            SpecialType.System_Int64 => (64, true),
            SpecialType.System_UInt64 => (64, false),
            _ => (0, false),
        };
        if (bits == 0) return Status.NotConstant;
        var modulus = BigInteger.One << bits;
        var min = signed ? -(modulus >> 1) : BigInteger.Zero;
        var max = signed ? (modulus >> 1) - 1 : modulus - 1;
        if (isChecked && (number < min || number > max)) return Status.Overflow;
        number = ((number % modulus) + modulus) % modulus;
        if (signed && number > max) number -= modulus;
        value = target switch
        {
            SpecialType.System_SByte => (object)(sbyte)number,
            SpecialType.System_Byte => (byte)number,
            SpecialType.System_Int16 => (short)number,
            SpecialType.System_UInt16 => (ushort)number,
            SpecialType.System_Int32 => (int)number,
            SpecialType.System_UInt32 => (uint)number,
            SpecialType.System_Int64 => (long)number,
            _ => (ulong)number,
        };
        return Status.Constant;
    }
}
