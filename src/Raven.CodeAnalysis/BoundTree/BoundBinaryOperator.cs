using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

sealed class BoundBinaryOperator
{
    public SyntaxKind SyntaxKind { get; }
    public ITypeSymbol LeftType { get; }
    public ITypeSymbol RightType { get; }
    public ITypeSymbol ResultType { get; }

    public static readonly BoundBinaryOperator Error =
        new BoundBinaryOperator(SyntaxKind.None, TypeSymbol.Object, TypeSymbol.Object, TypeSymbol.Object);

    private BoundBinaryOperator(SyntaxKind kind, ITypeSymbol left, ITypeSymbol right, ITypeSymbol result)
    {
        SyntaxKind = kind;
        LeftType = left;
        RightType = right;
        ResultType = result;
    }

    private static readonly BoundBinaryOperator[] Operators =
    {
        new BoundBinaryOperator(SyntaxKind.PlusToken, TypeSymbol.Int, TypeSymbol.Int, TypeSymbol.Int),
        new BoundBinaryOperator(SyntaxKind.MinusToken, TypeSymbol.Int, TypeSymbol.Int, TypeSymbol.Int),
        new BoundBinaryOperator(SyntaxKind.StarToken, TypeSymbol.Int, TypeSymbol.Int, TypeSymbol.Int),
        new BoundBinaryOperator(SyntaxKind.SlashToken, TypeSymbol.Int, TypeSymbol.Int, TypeSymbol.Int),
        new BoundBinaryOperator(SyntaxKind.PlusToken, TypeSymbol.String, TypeSymbol.String, TypeSymbol.String),
        //new BoundBinaryOperator(SyntaxKind.EqualsEqualsToken, TypeSymbol.Int, TypeSymbol.Int, TypeSymbol.Bool),
        //new BoundBinaryOperator(SyntaxKind.BangEqualsToken, TypeSymbol.Int, TypeSymbol.Int, TypeSymbol.Bool),
    };

    public static BoundBinaryOperator? Lookup(SyntaxKind kind, ITypeSymbol left, ITypeSymbol right)
    {
        return Operators.FirstOrDefault(op =>
            op.SyntaxKind == kind &&
            Equals(op.LeftType, left) &&
            Equals(op.RightType, right));
    }
}