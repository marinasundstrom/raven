namespace Raven.CodeAnalysis;

internal partial class BoundLiteralExpression : BoundExpression
{
    public object Value { get; }

    public BoundLiteralExpression(BoundLiteralExpressionKind kind, object value, ITypeSymbol type)
        : base(type, null, BoundExpressionReason.None)
    {
        Kind = kind;
        Value = value;
    }

    public BoundLiteralExpressionKind Kind { get; }
}

enum BoundLiteralExpressionKind
{
    NumericLiteral,
    StringLiteral,
    CharLiteral,
    TrueLiteral,
    FalseLiteral,
    NullLiteral
}