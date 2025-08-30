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

        if (left is ErrorTypeSymbol || right is ErrorTypeSymbol)
        {
            op = new BoundBinaryOperator(BinaryOperatorKind.None, compilation.ErrorTypeSymbol, compilation.ErrorTypeSymbol, compilation.ErrorTypeSymbol);
            return false;
        }

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var int64 = compilation.GetSpecialType(SpecialType.System_Int64);
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var boolType = compilation.GetSpecialType(SpecialType.System_Boolean);

        if (left.TypeKind == TypeKind.Enum && right.TypeKind == TypeKind.Enum)
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

        var candidates = new[]
        {
            new BoundBinaryOperator(BinaryOperatorKind.Addition,  intType, intType, intType),
            new BoundBinaryOperator(BinaryOperatorKind.Subtraction, intType, intType, intType),
            new BoundBinaryOperator(BinaryOperatorKind.Multiplication, intType, intType, intType),
            new BoundBinaryOperator(BinaryOperatorKind.Division, intType, intType, intType),

            new BoundBinaryOperator(BinaryOperatorKind.Addition, intType, int64, int64),
            new BoundBinaryOperator(BinaryOperatorKind.Subtraction, intType, int64, int64),
            new BoundBinaryOperator(BinaryOperatorKind.Multiplication,intType, int64, int64),
            new BoundBinaryOperator(BinaryOperatorKind.Division, intType, int64, int64),

            new BoundBinaryOperator(BinaryOperatorKind.Addition, int64, int64, int64),
            new BoundBinaryOperator(BinaryOperatorKind.Subtraction, int64, int64, int64),
            new BoundBinaryOperator(BinaryOperatorKind.Multiplication, int64, int64, int64),
            new BoundBinaryOperator(BinaryOperatorKind.Division,int64, int64, int64),

            new BoundBinaryOperator(BinaryOperatorKind.Addition, int64, intType, int64),
            new BoundBinaryOperator(BinaryOperatorKind.Subtraction, int64, intType, int64),
            new BoundBinaryOperator(BinaryOperatorKind.Multiplication,int64, intType, int64),
            new BoundBinaryOperator(BinaryOperatorKind.Division, int64, intType, int64),

            new BoundBinaryOperator(BinaryOperatorKind.Addition, stringType, stringType, stringType),

            new BoundBinaryOperator(BinaryOperatorKind.Equality, intType, intType, boolType),
            new BoundBinaryOperator(BinaryOperatorKind.Inequality, intType, intType, boolType),

            new BoundBinaryOperator(BinaryOperatorKind.GreaterThan, intType, intType, boolType),
            new BoundBinaryOperator(BinaryOperatorKind.LessThan, intType, intType, boolType),
            new BoundBinaryOperator(BinaryOperatorKind.GreaterThanOrEqual, intType, intType, boolType),
            new BoundBinaryOperator(BinaryOperatorKind.LessThanOrEqual, intType, intType, boolType),
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

        // Try lifting
        if (left.IsNullable() && right.IsNullable())
        {
            var underlyingLeft = left.GetNullableUnderlyingType();
            var underlyingRight = right.GetNullableUnderlyingType();

            var lifted = candidates.FirstOrDefault(op =>
                MatchesSyntaxKind(kind, op.OperatorKind) &&
                SymbolEqualityComparer.Default.Equals(op.LeftType, underlyingLeft) &&
                SymbolEqualityComparer.Default.Equals(op.RightType, underlyingRight));

            if (lifted is not null)
            {
                op = new BoundBinaryOperator(
                    lifted.OperatorKind | BinaryOperatorKind.Lifted,
                    left,
                    right,
                    new NullableTypeSymbol(lifted.ResultType, null, null, null, []));
                return true;
            }
        }

        op = new BoundBinaryOperator(BinaryOperatorKind.None, compilation.ErrorTypeSymbol, compilation.ErrorTypeSymbol, compilation.ErrorTypeSymbol);
        return false;
    }

    private static bool MatchesSyntaxKind(SyntaxKind kind, BinaryOperatorKind operatorKind)
    {
        return kind switch
        {
            SyntaxKind.PlusToken => operatorKind == BinaryOperatorKind.Addition,
            SyntaxKind.MinusToken => operatorKind == BinaryOperatorKind.Subtraction,
            SyntaxKind.StarToken => operatorKind == BinaryOperatorKind.Multiplication,
            SyntaxKind.SlashToken => operatorKind == BinaryOperatorKind.Division,
            SyntaxKind.EqualsEqualsToken => operatorKind == BinaryOperatorKind.Equality,
            SyntaxKind.NotEqualsToken => operatorKind == BinaryOperatorKind.Inequality,
            SyntaxKind.GreaterThanToken => operatorKind == BinaryOperatorKind.GreaterThan,
            SyntaxKind.LessThanToken => operatorKind == BinaryOperatorKind.LessThan,
            SyntaxKind.GreaterThanOrEqualsToken => operatorKind == BinaryOperatorKind.GreaterThanOrEqual,
            SyntaxKind.LessThanOrEqualsToken => operatorKind == BinaryOperatorKind.LessThanOrEqual,
            _ => false,
        };
    }
}

public static class TypeSymbolExtension3
{
    internal static bool IsNullable(this ITypeSymbol type) =>
        type.TypeKind == TypeKind.Nullable;

    internal static ITypeSymbol GetNullableUnderlyingType(this ITypeSymbol type) =>
        ((NullableTypeSymbol)type).UnderlyingType;
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
    StringConcatenation,

    Lifted = 1 << 8,
    Checked = 1 << 9,
}