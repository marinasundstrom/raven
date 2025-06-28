using Raven.CodeAnalysis.Syntax;

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

    public static BoundBinaryOperator? Lookup(Compilation compilation, SyntaxKind kind, ITypeSymbol left, ITypeSymbol right)
    {
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var int64 = compilation.GetSpecialType(SpecialType.System_Int64);
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var boolType = compilation.GetSpecialType(SpecialType.System_Boolean);
        var objectType = compilation.GetSpecialType(SpecialType.System_Object);

        if (left.TypeKind == TypeKind.Enum && right.TypeKind == TypeKind.Enum)
        {
            if (kind == SyntaxKind.EqualsEqualsToken)
            {
                return new BoundBinaryOperator(BinaryOperatorKind.Equality, left, right, boolType);
            }

            if (kind == SyntaxKind.NotEqualsToken)
            {
                return new BoundBinaryOperator(BinaryOperatorKind.None, left, right, boolType);
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
            return match;

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
                return new BoundBinaryOperator(
                    lifted.OperatorKind | BinaryOperatorKind.Lifted,
                    left,
                    right,
                    compilation.GetSpecialType(SpecialType.System_Nullable_T).Construct(lifted.ResultType));
            }
        }

        throw new InvalidOperationException();
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
        type.OriginalDefinition?.SpecialType == SpecialType.System_Nullable_T;

    internal static ITypeSymbol GetNullableUnderlyingType(this ITypeSymbol type) =>
        ((INamedTypeSymbol)type).TypeArguments[0];
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