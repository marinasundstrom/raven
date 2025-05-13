using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

class BoundBinaryOperator
{
    public SyntaxKind SyntaxKind { get; }
    public ITypeSymbol LeftType { get; }
    public ITypeSymbol RightType { get; }
    public ITypeSymbol ResultType { get; }
    public static BoundBinaryOperator Error { get; internal set; }

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
        new BoundBinaryOperator(SyntaxKind.PlusToken, TypeSymbol.String, TypeSymbol.String, TypeSymbol.String),
        //new BoundBinaryOperator(SyntaxKind.EqualsEqualsToken, TypeSymbol.Int, TypeSymbol.Int, TypeSymbol.Bool),
    };

    public static BoundBinaryOperator Lookup(SyntaxKind kind, ITypeSymbol left, ITypeSymbol right)
    {
        return Operators.FirstOrDefault(op => op.SyntaxKind == kind && op.LeftType == left && op.RightType == right);
    }
}
