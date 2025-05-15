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

    public static BoundBinaryOperator? Lookup(Compilation compilation, SyntaxKind kind, ITypeSymbol left, ITypeSymbol right)
    {
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var objectType = compilation.GetSpecialType(SpecialType.System_Object);

        var candidates = new[]
        {
            new BoundBinaryOperator(SyntaxKind.PlusToken, intType, intType, intType),
            new BoundBinaryOperator(SyntaxKind.MinusToken, intType, intType, intType),
            new BoundBinaryOperator(SyntaxKind.StarToken, intType, intType, intType),
            new BoundBinaryOperator(SyntaxKind.SlashToken, intType, intType, intType),
            new BoundBinaryOperator(SyntaxKind.PlusToken, stringType, stringType, stringType),
        };

        return candidates.FirstOrDefault(op =>
            op.SyntaxKind == kind &&
            SymbolEqualityComparer.Default.Equals(op.LeftType, left) &&
            SymbolEqualityComparer.Default.Equals(op.RightType, right));
    }
}
