using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal partial class BoundUnaryOperator
{
    public ITypeSymbol OperandType { get; }
    public ITypeSymbol ResultType { get; }
    public BoundUnaryOperatorKind OperatorKind { get; }

    private BoundUnaryOperator(
        BoundUnaryOperatorKind operatorKind,
        ITypeSymbol operandType,
        ITypeSymbol resultType)
    {
        OperatorKind = operatorKind;
        OperandType = operandType;
        ResultType = resultType;
    }

    public static bool TryLookup(Compilation compilation, SyntaxKind kind, ITypeSymbol operandType, out BoundUnaryOperator op)
    {
        operandType = operandType.UnwrapLiteralType() ?? operandType;

        if (operandType.TypeKind == TypeKind.Error)
        {
            op = new BoundUnaryOperator(
                BoundUnaryOperatorKind.UnaryPlus,
                compilation.ErrorTypeSymbol,
                compilation.ErrorTypeSymbol);
            return false;
        }

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var int64 = compilation.GetSpecialType(SpecialType.System_Int64);
        var boolType = compilation.GetSpecialType(SpecialType.System_Boolean);

        var candidates = new[]
        {
            new BoundUnaryOperator(BoundUnaryOperatorKind.UnaryPlus, intType, intType),
            new BoundUnaryOperator(BoundUnaryOperatorKind.UnaryMinus, intType, intType),
            new BoundUnaryOperator(BoundUnaryOperatorKind.BitwiseNot, intType, intType),

            new BoundUnaryOperator(BoundUnaryOperatorKind.UnaryPlus, int64, int64),
            new BoundUnaryOperator(BoundUnaryOperatorKind.UnaryMinus, int64, int64),
            new BoundUnaryOperator(BoundUnaryOperatorKind.BitwiseNot, int64, int64),

            new BoundUnaryOperator(BoundUnaryOperatorKind.LogicalNot, boolType, boolType),
        };

        var match = candidates.FirstOrDefault(op =>
            MatchesSyntaxKind(kind, op.OperatorKind) &&
            SymbolEqualityComparer.Default.Equals(op.OperandType, operandType));

        if (match is not null)
        {
            op = match;
            return true;
        }

        op = new BoundUnaryOperator(
            BoundUnaryOperatorKind.UnaryPlus,
            compilation.ErrorTypeSymbol,
            compilation.ErrorTypeSymbol);
        return false;
    }

    private static bool MatchesSyntaxKind(SyntaxKind kind, BoundUnaryOperatorKind operatorKind)
    {
        return kind switch
        {
            SyntaxKind.PlusToken => operatorKind == BoundUnaryOperatorKind.UnaryPlus,
            SyntaxKind.MinusToken => operatorKind == BoundUnaryOperatorKind.UnaryMinus,
            SyntaxKind.ExclamationToken => operatorKind == BoundUnaryOperatorKind.LogicalNot,
            _ => false,
        };
    }
}

enum BoundUnaryOperatorKind
{
    LogicalNot,
    UnaryPlus,
    UnaryMinus,
    BitwiseNot
}
