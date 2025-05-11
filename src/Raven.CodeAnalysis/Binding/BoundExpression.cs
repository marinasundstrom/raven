using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

abstract class BoundNode
{

}

abstract class BoundExpression : BoundNode
{
    public abstract ITypeSymbol Type { get; }
}

class BoundLiteralExpression : BoundExpression
{
    public object Value { get; }
    public override ITypeSymbol Type { get; }

    public BoundLiteralExpression(object value, ITypeSymbol type)
    {
        Value = value;
        Type = type;
    }
}

class BoundVariableExpression : BoundExpression
{
    public ILocalSymbol Variable { get; }
    public override ITypeSymbol Type => Variable.Type;

    public BoundVariableExpression(ILocalSymbol variable)
    {
        Variable = variable;
    }
}

class BoundBinaryExpression : BoundExpression
{
    public BoundExpression Left { get; }
    public BoundBinaryOperator Operator { get; }
    public BoundExpression Right { get; }

    public override ITypeSymbol Type => Operator.ResultType;

    public BoundBinaryExpression(BoundExpression left, BoundBinaryOperator op, BoundExpression right)
    {
        Left = left;
        Operator = op;
        Right = right;
    }
}

class BoundBinaryOperator
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

internal class TypeSymbol
{
    public static ITypeSymbol Int { get; internal set; }
    public static ITypeSymbol String { get; internal set; }
}

class BoundUnaryExpression : BoundExpression
{
    public BoundUnaryOperator Operator { get; }
    public BoundExpression Operand { get; }

    public override ITypeSymbol Type => Operator.ResultType;

    public BoundUnaryExpression(BoundUnaryOperator op, BoundExpression operand)
    {
        Operator = op;
        Operand = operand;
    }
}

class BoundUnaryOperator
{
    public ITypeSymbol ResultType { get; internal set; }
}

class BoundCallExpression : BoundExpression
{
    public IMethodSymbol Method { get; }
    public ImmutableArray<BoundExpression> Arguments { get; }

    public override ITypeSymbol Type => Method.ReturnType;

    public BoundCallExpression(IMethodSymbol method, ImmutableArray<BoundExpression> arguments)
    {
        Method = method;
        Arguments = arguments;
    }
}

class BoundAssignmentExpression : BoundExpression
{
    public ILocalSymbol Variable { get; }
    public BoundExpression Expression { get; }

    public override ITypeSymbol Type => Expression.Type;

    public BoundAssignmentExpression(ILocalSymbol variable, BoundExpression expression)
    {
        Variable = variable;
        Expression = expression;
    }
}