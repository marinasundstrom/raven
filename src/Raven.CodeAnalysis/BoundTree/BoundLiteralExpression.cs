namespace Raven.CodeAnalysis;

internal partial class BoundLiteralExpression : BoundExpression
{
    public object Value { get; }

    public TypeSymbol? ConvertedType { get; }

    public BoundLiteralExpression(BoundLiteralExpressionKind kind, object value, TypeSymbol type, TypeSymbol? convertedType = null)
        : base(type, null, BoundExpressionReason.None)
    {
        Kind = kind;
        Value = value;
        ConvertedType = convertedType;
    }

    public override TypeSymbol? GetConvertedType() => ConvertedType;

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
