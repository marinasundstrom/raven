using System;
using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal partial class BoundBinaryOperator
{
    public BinaryOperatorKind OperatorKind { get; }
    public ITypeSymbol LeftType { get; }
    public ITypeSymbol RightType { get; }
    public ITypeSymbol ResultType { get; }

    private BoundBinaryOperator(
        BinaryOperatorKind operatorKind,
        ITypeSymbol left,
        ITypeSymbol right,
        ITypeSymbol result)
    {
        OperatorKind = operatorKind;
        LeftType = left;
        RightType = right;
        ResultType = result;
    }

    public static bool TryLookup(Compilation compilation, SyntaxKind kind, ITypeSymbol left, ITypeSymbol right, out BoundBinaryOperator op)
    {
        if (left is LiteralTypeSymbol litLeft)
            left = litLeft.UnderlyingType;
        if (right is LiteralTypeSymbol litRight)
            right = litRight.UnderlyingType;

        // Nullable reference types are represented with a wrapper symbol in Raven.
        // For built-in operator lookup they should behave like their underlying reference type.
        if (left is NullableTypeSymbol { UnderlyingType: { IsValueType: false } leftUnderlying })
            left = leftUnderlying;

        if (right is NullableTypeSymbol { UnderlyingType: { IsValueType: false } rightUnderlying })
            right = rightUnderlying;

        if (left is ErrorTypeSymbol || right is ErrorTypeSymbol)
        {
            op = new BoundBinaryOperator(BinaryOperatorKind.None, compilation.ErrorTypeSymbol, compilation.ErrorTypeSymbol, compilation.ErrorTypeSymbol);
            return false;
        }

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var int64 = compilation.GetSpecialType(SpecialType.System_Int64);
        var nintType = compilation.GetSpecialType(SpecialType.System_IntPtr);
        var doubleType = compilation.GetSpecialType(SpecialType.System_Double);
        var decimalType = compilation.GetSpecialType(SpecialType.System_Decimal);
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var boolType = compilation.GetSpecialType(SpecialType.System_Boolean);

        if (TryCreatePointerArithmetic(kind, left, right, nintType, out op))
            return true;

        if (left.TypeKind == TypeKind.Enum && right.TypeKind == TypeKind.Enum)
        {
            if (SymbolEqualityComparer.Default.Equals(left, right))
            {
                if (kind == SyntaxKind.EqualsEqualsToken)
                {
                    op = new BoundBinaryOperator(BinaryOperatorKind.Equality, left, right, boolType);
                    return true;
                }

                if (kind == SyntaxKind.NotEqualsToken)
                {
                    op = new BoundBinaryOperator(BinaryOperatorKind.Inequality, left, right, boolType);
                    return true;
                }

                if (kind == SyntaxKind.AmpersandToken)
                {
                    op = new BoundBinaryOperator(BinaryOperatorKind.BitwiseAnd, left, right, left);
                    return true;
                }

                if (kind == SyntaxKind.BarToken)
                {
                    op = new BoundBinaryOperator(BinaryOperatorKind.BitwiseOr, left, right, left);
                    return true;
                }
            }
        }

        var candidates = new[]
        {
            // int arithmetic
            new BoundBinaryOperator(BinaryOperatorKind.Addition,        intType, intType, intType),
            new BoundBinaryOperator(BinaryOperatorKind.Subtraction,     intType, intType, intType),
            new BoundBinaryOperator(BinaryOperatorKind.Multiplication,  intType, intType, intType),
            new BoundBinaryOperator(BinaryOperatorKind.Division,        intType, intType, intType),
            new BoundBinaryOperator(BinaryOperatorKind.Modulo,          intType, intType, intType),
            new BoundBinaryOperator(BinaryOperatorKind.BitwiseAnd,      intType, intType, intType),
            new BoundBinaryOperator(BinaryOperatorKind.BitwiseOr,       intType, intType, intType),

            // bool non-short-circuit operators
            new BoundBinaryOperator(BinaryOperatorKind.BitwiseAnd,      boolType, boolType, boolType),
            new BoundBinaryOperator(BinaryOperatorKind.BitwiseOr,       boolType, boolType, boolType),

            // int (left) with long (right)
            new BoundBinaryOperator(BinaryOperatorKind.Addition,        intType, int64,  int64),
            new BoundBinaryOperator(BinaryOperatorKind.Subtraction,     intType, int64,  int64),
            new BoundBinaryOperator(BinaryOperatorKind.Multiplication,  intType, int64,  int64),
            new BoundBinaryOperator(BinaryOperatorKind.Division,        intType, int64,  int64),
            new BoundBinaryOperator(BinaryOperatorKind.Modulo,          intType, int64,  int64),
            new BoundBinaryOperator(BinaryOperatorKind.BitwiseAnd,      intType, int64,  int64),
            new BoundBinaryOperator(BinaryOperatorKind.BitwiseOr,       intType, int64,  int64),

            // long arithmetic
            new BoundBinaryOperator(BinaryOperatorKind.Addition,        int64,  int64,  int64),
            new BoundBinaryOperator(BinaryOperatorKind.Subtraction,     int64,  int64,  int64),
            new BoundBinaryOperator(BinaryOperatorKind.Multiplication,  int64,  int64,  int64),
            new BoundBinaryOperator(BinaryOperatorKind.Division,        int64,  int64,  int64),
            new BoundBinaryOperator(BinaryOperatorKind.Modulo,          int64,  int64,  int64),
            new BoundBinaryOperator(BinaryOperatorKind.BitwiseAnd,      int64,  int64,  int64),
            new BoundBinaryOperator(BinaryOperatorKind.BitwiseOr,       int64,  int64,  int64),

            // long (left) with int (right)
            new BoundBinaryOperator(BinaryOperatorKind.Addition,        int64,  intType, int64),
            new BoundBinaryOperator(BinaryOperatorKind.Subtraction,     int64,  intType, int64),
            new BoundBinaryOperator(BinaryOperatorKind.Multiplication,  int64,  intType, int64),
            new BoundBinaryOperator(BinaryOperatorKind.Division,        int64,  intType, int64),
            new BoundBinaryOperator(BinaryOperatorKind.Modulo,          int64,  intType, int64),
            new BoundBinaryOperator(BinaryOperatorKind.BitwiseAnd,      int64,  intType, int64),
            new BoundBinaryOperator(BinaryOperatorKind.BitwiseOr,       int64,  intType, int64),

            // double arithmetic
            new BoundBinaryOperator(BinaryOperatorKind.Addition,       doubleType, doubleType, doubleType),
            new BoundBinaryOperator(BinaryOperatorKind.Subtraction,    doubleType, doubleType, doubleType),
            new BoundBinaryOperator(BinaryOperatorKind.Multiplication, doubleType, doubleType, doubleType),
            new BoundBinaryOperator(BinaryOperatorKind.Division,       doubleType, doubleType, doubleType),
            new BoundBinaryOperator(BinaryOperatorKind.Modulo,         doubleType, doubleType, doubleType),

            // double comparisons
            new BoundBinaryOperator(BinaryOperatorKind.Equality,       doubleType, doubleType, boolType),
            new BoundBinaryOperator(BinaryOperatorKind.Inequality,     doubleType, doubleType, boolType),
            new BoundBinaryOperator(BinaryOperatorKind.GreaterThan,    doubleType, doubleType, boolType),
            new BoundBinaryOperator(BinaryOperatorKind.LessThan,       doubleType, doubleType, boolType),
            new BoundBinaryOperator(BinaryOperatorKind.GreaterThanOrEqual, doubleType, doubleType, boolType),
            new BoundBinaryOperator(BinaryOperatorKind.LessThanOrEqual,    doubleType, doubleType, boolType),

            // decimal arithmetic
            new BoundBinaryOperator(BinaryOperatorKind.Addition,       decimalType, decimalType, decimalType),
            new BoundBinaryOperator(BinaryOperatorKind.Subtraction,    decimalType, decimalType, decimalType),
            new BoundBinaryOperator(BinaryOperatorKind.Multiplication, decimalType, decimalType, decimalType),
            new BoundBinaryOperator(BinaryOperatorKind.Division,       decimalType, decimalType, decimalType),
            new BoundBinaryOperator(BinaryOperatorKind.Modulo,         decimalType, decimalType, decimalType),

            // decimal comparisons
            new BoundBinaryOperator(BinaryOperatorKind.Equality,       decimalType, decimalType, boolType),
            new BoundBinaryOperator(BinaryOperatorKind.Inequality,     decimalType, decimalType, boolType),
            new BoundBinaryOperator(BinaryOperatorKind.GreaterThan,    decimalType, decimalType, boolType),
            new BoundBinaryOperator(BinaryOperatorKind.LessThan,       decimalType, decimalType, boolType),
            new BoundBinaryOperator(BinaryOperatorKind.GreaterThanOrEqual, decimalType, decimalType, boolType),
            new BoundBinaryOperator(BinaryOperatorKind.LessThanOrEqual,    decimalType, decimalType, boolType),

            // string
            new BoundBinaryOperator(BinaryOperatorKind.Addition,        stringType, stringType, stringType),

            // int comparisons
            new BoundBinaryOperator(BinaryOperatorKind.Equality,        intType, intType, boolType),
            new BoundBinaryOperator(BinaryOperatorKind.Inequality,      intType, intType, boolType),

            new BoundBinaryOperator(BinaryOperatorKind.GreaterThan,     intType, intType, boolType),
            new BoundBinaryOperator(BinaryOperatorKind.LessThan,        intType, intType, boolType),
            new BoundBinaryOperator(BinaryOperatorKind.GreaterThanOrEqual, intType, intType, boolType),
            new BoundBinaryOperator(BinaryOperatorKind.LessThanOrEqual, intType, intType, boolType),

            // bool logical operators
            new BoundBinaryOperator(BinaryOperatorKind.LogicalAnd,      boolType, boolType, boolType),
            new BoundBinaryOperator(BinaryOperatorKind.LogicalOr,       boolType, boolType, boolType),
        };

        // Try regular match first
        var match = candidates.FirstOrDefault(op =>
            MatchesSyntaxKind(kind, op.OperatorKind) &&
            SymbolEqualityComparer.Default.Equals(op.LeftType, left) &&
            SymbolEqualityComparer.Default.Equals(op.RightType, right));

        if (match is not null)
        {
            op = match;
            return true;
        }

        // Try C#-style binary numeric promotion, then lookup again (covers int/long with double/decimal).
        if (TryApplyBinaryNumericPromotion(compilation, kind, left, right, out var promotedLeft, out var promotedRight))
        {
            var promotedMatch = candidates.FirstOrDefault(op =>
                MatchesSyntaxKind(kind, op.OperatorKind) &&
                SymbolEqualityComparer.Default.Equals(op.LeftType, promotedLeft) &&
                SymbolEqualityComparer.Default.Equals(op.RightType, promotedRight));

            if (promotedMatch is not null)
            {
                op = promotedMatch;
                return true;
            }
        }

        // Reference equality/inequality (C#-style: same reference type)
        if (left.IsReferenceType &&
            right.IsReferenceType &&
            SymbolEqualityComparer.Default.Equals(left, right))
        {
            if (kind == SyntaxKind.EqualsEqualsToken)
            {
                op = new BoundBinaryOperator(BinaryOperatorKind.Equality, left, right, boolType);
                return true;
            }

            if (kind == SyntaxKind.NotEqualsToken)
            {
                op = new BoundBinaryOperator(BinaryOperatorKind.Inequality, left, right, boolType);
                return true;
            }
        }

        // Null literal equality/inequality for nullable/reference operands.
        if (kind is SyntaxKind.EqualsEqualsToken or SyntaxKind.NotEqualsToken)
        {
            if (left.TypeKind == TypeKind.Null && (right.IsReferenceType || right.IsNullable))
            {
                op = new BoundBinaryOperator(
                    kind == SyntaxKind.EqualsEqualsToken ? BinaryOperatorKind.Equality : BinaryOperatorKind.Inequality,
                    left,
                    right,
                    boolType);
                return true;
            }

            if (right.TypeKind == TypeKind.Null && (left.IsReferenceType || left.IsNullable))
            {
                op = new BoundBinaryOperator(
                    kind == SyntaxKind.EqualsEqualsToken ? BinaryOperatorKind.Equality : BinaryOperatorKind.Inequality,
                    left,
                    right,
                    boolType);
                return true;
            }
        }

        // Try lifting for nullable/nullable.
        if (left.IsNullable && right.IsNullable)
        {
            var underlyingLeft = left.GetNullableUnderlyingType() ?? left;
            var underlyingRight = right.GetNullableUnderlyingType() ?? right;

            BoundBinaryOperator? lifted = candidates.FirstOrDefault(op =>
                MatchesSyntaxKind(kind, op.OperatorKind) &&
                SymbolEqualityComparer.Default.Equals(op.LeftType, underlyingLeft) &&
                SymbolEqualityComparer.Default.Equals(op.RightType, underlyingRight));

            // Try promotion on underlying types, then lookup again (e.g. int? + double?)
            if (lifted is null &&
                TryApplyBinaryNumericPromotion(compilation, kind, underlyingLeft, underlyingRight, out var pLeft, out var pRight))
            {
                lifted = candidates.FirstOrDefault(op =>
                    MatchesSyntaxKind(kind, op.OperatorKind) &&
                    SymbolEqualityComparer.Default.Equals(op.LeftType, pLeft) &&
                    SymbolEqualityComparer.Default.Equals(op.RightType, pRight));
            }

            if (lifted is not null)
            {
                var liftedKind = lifted.OperatorKind & ~(BinaryOperatorKind.Lifted | BinaryOperatorKind.Checked);
                var resultType = liftedKind is BinaryOperatorKind.Equality or BinaryOperatorKind.Inequality
                    ? boolType
                    : lifted.ResultType.MakeNullable();

                op = new BoundBinaryOperator(
                    lifted.OperatorKind | BinaryOperatorKind.Lifted,
                    left,
                    right,
                    resultType);
                return true;
            }
        }

        // Try lifting for nullable-value/non-nullable and non-nullable/nullable-value.
        // Example: int? == int, int == int?, FooStruct? != FooStruct.
        if (kind is SyntaxKind.EqualsEqualsToken or SyntaxKind.NotEqualsToken)
        {
            if (left is NullableTypeSymbol { UnderlyingType: { IsValueType: true } leftValueUnderlying } &&
                right is not NullableTypeSymbol &&
                TryLookup(compilation, kind, leftValueUnderlying, right, out var leftLiftedBase))
            {
                var liftedBaseKind = leftLiftedBase.OperatorKind & ~(BinaryOperatorKind.Lifted | BinaryOperatorKind.Checked);
                if (liftedBaseKind is BinaryOperatorKind.Equality or BinaryOperatorKind.Inequality)
                {
                    op = new BoundBinaryOperator(
                        liftedBaseKind | BinaryOperatorKind.Lifted,
                        left,
                        right,
                        boolType);
                    return true;
                }
            }

            if (right is NullableTypeSymbol { UnderlyingType: { IsValueType: true } rightValueUnderlying } &&
                left is not NullableTypeSymbol &&
                TryLookup(compilation, kind, left, rightValueUnderlying, out var rightLiftedBase))
            {
                var liftedBaseKind = rightLiftedBase.OperatorKind & ~(BinaryOperatorKind.Lifted | BinaryOperatorKind.Checked);
                if (liftedBaseKind is BinaryOperatorKind.Equality or BinaryOperatorKind.Inequality)
                {
                    op = new BoundBinaryOperator(
                        liftedBaseKind | BinaryOperatorKind.Lifted,
                        left,
                        right,
                        boolType);
                    return true;
                }
            }
        }

        op = new BoundBinaryOperator(BinaryOperatorKind.None, compilation.ErrorTypeSymbol, compilation.ErrorTypeSymbol, compilation.ErrorTypeSymbol);
        return false;
    }

    private static bool TryCreatePointerArithmetic(
        SyntaxKind kind,
        ITypeSymbol left,
        ITypeSymbol right,
        ITypeSymbol nativeIntType,
        out BoundBinaryOperator op)
    {
        var leftPointer = left as IPointerTypeSymbol;
        var rightPointer = right as IPointerTypeSymbol;

        if (kind == SyntaxKind.PlusToken)
        {
            if (leftPointer is not null && IsIntegralType(right))
            {
                op = new BoundBinaryOperator(BinaryOperatorKind.Addition, left, right, left);
                return true;
            }

            if (rightPointer is not null && IsIntegralType(left))
            {
                op = new BoundBinaryOperator(BinaryOperatorKind.Addition, left, right, right);
                return true;
            }
        }

        if (kind == SyntaxKind.MinusToken)
        {
            if (leftPointer is not null && IsIntegralType(right))
            {
                op = new BoundBinaryOperator(BinaryOperatorKind.Subtraction, left, right, left);
                return true;
            }

            if (leftPointer is not null &&
                rightPointer is not null &&
                SymbolEqualityComparer.Default.Equals(leftPointer.PointedAtType, rightPointer.PointedAtType))
            {
                op = new BoundBinaryOperator(BinaryOperatorKind.Subtraction, left, right, nativeIntType);
                return true;
            }
        }

        op = new BoundBinaryOperator(BinaryOperatorKind.None, left, right, left);
        return false;
    }

    private static bool IsIntegralType(ITypeSymbol type)
    {
        if (type is LiteralTypeSymbol literal)
            type = literal.UnderlyingType;

        return type.SpecialType switch
        {
            SpecialType.System_SByte => true,
            SpecialType.System_Byte => true,
            SpecialType.System_Int16 => true,
            SpecialType.System_UInt16 => true,
            SpecialType.System_Int32 => true,
            SpecialType.System_UInt32 => true,
            SpecialType.System_Int64 => true,
            SpecialType.System_UInt64 => true,
            SpecialType.System_Char => true,
            SpecialType.System_IntPtr => true,
            SpecialType.System_UIntPtr => true,
            _ => false,
        };
    }

    private static bool TryApplyBinaryNumericPromotion(
        Compilation compilation,
        SyntaxKind kind,
        ITypeSymbol left,
        ITypeSymbol right,
        out ITypeSymbol promotedLeft,
        out ITypeSymbol promotedRight)
    {
        promotedLeft = left;
        promotedRight = right;

        if (!IsNumericPromotionOperator(kind))
            return false;

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var int64Type = compilation.GetSpecialType(SpecialType.System_Int64);
        var doubleType = compilation.GetSpecialType(SpecialType.System_Double);
        var decimalType = compilation.GetSpecialType(SpecialType.System_Decimal);
        var byteType = compilation.GetSpecialType(SpecialType.System_Byte);
        var charType = compilation.GetSpecialType(SpecialType.System_Char);

        static bool Eq(ITypeSymbol a, ITypeSymbol b) => SymbolEqualityComparer.Default.Equals(a, b);

        static bool IsIntegralForPromotion(ITypeSymbol t, ITypeSymbol byteType, ITypeSymbol charType, ITypeSymbol intType, ITypeSymbol int64Type) =>
            SymbolEqualityComparer.Default.Equals(t, byteType) ||
            SymbolEqualityComparer.Default.Equals(t, charType) ||
            SymbolEqualityComparer.Default.Equals(t, intType) ||
            SymbolEqualityComparer.Default.Equals(t, int64Type);

        static bool IsSmallIntegral(ITypeSymbol t, ITypeSymbol byteType, ITypeSymbol charType) =>
            SymbolEqualityComparer.Default.Equals(t, byteType) ||
            SymbolEqualityComparer.Default.Equals(t, charType);

        var leftIsDecimal = Eq(left, decimalType);
        var rightIsDecimal = Eq(right, decimalType);

        var leftIsDouble = Eq(left, doubleType);
        var rightIsDouble = Eq(right, doubleType);

        // C#-style: small integral types (byte/char) are promoted to int before further numeric promotion.
        // This ensures e.g. `byte + byte` binds as `int + int` and mixed operations work naturally.
        if (IsSmallIntegral(left, byteType, charType))
            promotedLeft = intType;

        if (IsSmallIntegral(right, byteType, charType))
            promotedRight = intType;

        // If we promoted either operand, update the working types.
        left = promotedLeft;
        right = promotedRight;

        // decimal "wins" over integral, but does NOT implicitly mix with double.
        if (leftIsDecimal || rightIsDecimal)
        {
            if (leftIsDouble || rightIsDouble)
                return false;

            if (leftIsDecimal && rightIsDecimal)
                return true;

            if (leftIsDecimal && IsIntegralForPromotion(right, byteType, charType, intType, int64Type))
            {
                promotedRight = decimalType;
                return true;
            }

            if (rightIsDecimal && IsIntegralForPromotion(left, byteType, charType, intType, int64Type))
            {
                promotedLeft = decimalType;
                return true;
            }

            return false;
        }

        // double "wins" over integral.
        if (leftIsDouble || rightIsDouble)
        {
            if (leftIsDouble && rightIsDouble)
                return true;

            if (leftIsDouble && IsIntegralForPromotion(right, byteType, charType, intType, int64Type))
            {
                promotedRight = doubleType;
                return true;
            }

            if (rightIsDouble && IsIntegralForPromotion(left, byteType, charType, intType, int64Type))
            {
                promotedLeft = doubleType;
                return true;
            }

            return false;
        }

        // Pure integral promotion (e.g. int + long, byte + int, etc.)
        var leftIsLong = Eq(left, int64Type);
        var rightIsLong = Eq(right, int64Type);

        if (leftIsLong || rightIsLong)
        {
            if (leftIsLong && IsIntegralForPromotion(right, byteType, charType, intType, int64Type))
            {
                promotedRight = int64Type;
                return true;
            }

            if (rightIsLong && IsIntegralForPromotion(left, byteType, charType, intType, int64Type))
            {
                promotedLeft = int64Type;
                return true;
            }
        }

        // If we had only small integrals, we already promoted them to int above.
        if (Eq(promotedLeft, intType) && Eq(promotedRight, intType))
            return true;

        // If either side is int and the other is integral, promote the other to int.
        if (Eq(promotedLeft, intType) && IsIntegralForPromotion(promotedRight, byteType, charType, intType, int64Type))
        {
            promotedRight = intType;
            return true;
        }

        if (Eq(promotedRight, intType) && IsIntegralForPromotion(promotedLeft, byteType, charType, intType, int64Type))
        {
            promotedLeft = intType;
            return true;
        }

        return false;
    }

    private static bool IsNumericPromotionOperator(SyntaxKind kind)
    {
        return kind switch
        {
            SyntaxKind.PlusToken => true,
            SyntaxKind.MinusToken => true,
            SyntaxKind.StarToken => true,
            SyntaxKind.SlashToken => true,
            SyntaxKind.PercentToken => true,

            SyntaxKind.EqualsEqualsToken => true,
            SyntaxKind.NotEqualsToken => true,

            SyntaxKind.GreaterThanToken => true,
            SyntaxKind.LessThanToken => true,
            SyntaxKind.GreaterThanOrEqualsToken => true,
            SyntaxKind.LessThanOrEqualsToken => true,

            _ => false,
        };
    }

    private static bool MatchesSyntaxKind(SyntaxKind kind, BinaryOperatorKind operatorKind)
    {
        return kind switch
        {
            SyntaxKind.PlusToken => operatorKind == BinaryOperatorKind.Addition,
            SyntaxKind.MinusToken => operatorKind == BinaryOperatorKind.Subtraction,
            SyntaxKind.StarToken => operatorKind == BinaryOperatorKind.Multiplication,
            SyntaxKind.SlashToken => operatorKind == BinaryOperatorKind.Division,
            SyntaxKind.PercentToken => operatorKind == BinaryOperatorKind.Modulo,
            SyntaxKind.EqualsEqualsToken => operatorKind == BinaryOperatorKind.Equality,
            SyntaxKind.NotEqualsToken => operatorKind == BinaryOperatorKind.Inequality,
            SyntaxKind.GreaterThanToken => operatorKind == BinaryOperatorKind.GreaterThan,
            SyntaxKind.LessThanToken => operatorKind == BinaryOperatorKind.LessThan,
            SyntaxKind.GreaterThanOrEqualsToken => operatorKind == BinaryOperatorKind.GreaterThanOrEqual,
            SyntaxKind.LessThanOrEqualsToken => operatorKind == BinaryOperatorKind.LessThanOrEqual,
            SyntaxKind.AmpersandToken => operatorKind == BinaryOperatorKind.BitwiseAnd,
            SyntaxKind.BarToken => operatorKind == BinaryOperatorKind.BitwiseOr,
            SyntaxKind.AmpersandAmpersandToken => operatorKind == BinaryOperatorKind.LogicalAnd,
            SyntaxKind.BarBarToken => operatorKind == BinaryOperatorKind.LogicalOr,
            _ => false,
        };
    }
}

[Flags]
internal enum BinaryOperatorKind
{
    None = 0,
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Equality,
    Inequality,
    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
    Modulo,
    BitwiseAnd,
    BitwiseOr,
    LogicalAnd,
    LogicalOr,
    StringConcatenation,

    Lifted = 1 << 8,
    Checked = 1 << 9,
}
