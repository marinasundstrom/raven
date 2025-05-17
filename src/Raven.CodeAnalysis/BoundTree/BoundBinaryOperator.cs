using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

sealed class BoundBinaryOperator
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
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var boolType = compilation.GetSpecialType(SpecialType.System_Boolean);
        var objectType = compilation.GetSpecialType(SpecialType.System_Object);

        var candidates = new[]
        {
            new BoundBinaryOperator(SyntaxKind.PlusToken, intType, intType, intType),
            new BoundBinaryOperator(SyntaxKind.MinusToken, intType, intType, intType),
            new BoundBinaryOperator(SyntaxKind.StarToken, intType, intType, intType),
            new BoundBinaryOperator(SyntaxKind.SlashToken, intType, intType, intType),
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
