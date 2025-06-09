using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal partial class BoundBinaryOperator
{
    public SyntaxKind SyntaxKind { get; }
    public ITypeSymbol LeftType { get; }
    public ITypeSymbol RightType { get; }
    public ITypeSymbol ResultType { get; }

    private BoundBinaryOperator(SyntaxKind kind, ITypeSymbol left, ITypeSymbol right, ITypeSymbol result)
    {
        SyntaxKind = kind;
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
            if (kind == SyntaxKind.EqualsEqualsToken || kind == SyntaxKind.NotEqualsToken)
            {
                return new BoundBinaryOperator(kind, left, right, boolType);
            }
        }

        var candidates = new[]
        {
            new BoundBinaryOperator(SyntaxKind.PlusToken, intType, intType, intType),
            new BoundBinaryOperator(SyntaxKind.MinusToken, intType, intType, intType),
            new BoundBinaryOperator(SyntaxKind.StarToken, intType, intType, intType),
            new BoundBinaryOperator(SyntaxKind.SlashToken, intType, intType, intType),

            new BoundBinaryOperator(SyntaxKind.PlusToken, intType, int64, int64),
            new BoundBinaryOperator(SyntaxKind.MinusToken, intType, int64, int64),
            new BoundBinaryOperator(SyntaxKind.StarToken, intType, int64, int64),
            new BoundBinaryOperator(SyntaxKind.SlashToken, intType, int64, int64),

            new BoundBinaryOperator(SyntaxKind.PlusToken, int64, int64, int64),
            new BoundBinaryOperator(SyntaxKind.MinusToken, int64, int64, int64),
            new BoundBinaryOperator(SyntaxKind.StarToken, int64, int64, int64),
            new BoundBinaryOperator(SyntaxKind.SlashToken, int64, int64, int64),

            new BoundBinaryOperator(SyntaxKind.PlusToken, int64, intType, int64),
            new BoundBinaryOperator(SyntaxKind.MinusToken, int64, intType, int64),
            new BoundBinaryOperator(SyntaxKind.StarToken, int64, intType, int64),
            new BoundBinaryOperator(SyntaxKind.SlashToken, int64, intType, int64),

            new BoundBinaryOperator(SyntaxKind.PlusToken, stringType, stringType, stringType),

            new BoundBinaryOperator(SyntaxKind.GreaterThanToken, intType, intType, boolType),
            new BoundBinaryOperator(SyntaxKind.LessThanToken, intType, intType, boolType),
            new BoundBinaryOperator(SyntaxKind.GreaterOrEqualsToken, intType, intType, boolType),
            new BoundBinaryOperator(SyntaxKind.LessThanOrEqualExpression, intType, intType, boolType),
        };

        return candidates.FirstOrDefault(op =>
            op.SyntaxKind == kind &&
            SymbolEqualityComparer.Default.Equals(op.LeftType, left) &&
            SymbolEqualityComparer.Default.Equals(op.RightType, right));
    }
}
