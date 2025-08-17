using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal partial class BoundUnaryOperator
{
    public ITypeSymbol ResultType { get; internal set; }
    public BoundUnaryOperatorKind OperatorKind { get; internal set; }
}

enum BoundUnaryOperatorKind
{
    LogicalNot,
    UnaryPlus,
    UnaryMinus,
    BitwiseNot
}