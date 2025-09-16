namespace Raven.CodeAnalysis;

internal partial class BoundLiteralExpression : BoundExpression
{
    public object Value { get; }

    public ITypeSymbol? ConvertedType { get; }

    public BoundLiteralExpression(BoundLiteralExpressionKind kind, object value, ITypeSymbol type, ITypeSymbol? convertedType = null)
        : base(type, null, BoundExpressionReason.None)
    {
        Kind = kind;
        Value = value;
        ConvertedType = convertedType;
    }

    public override ITypeSymbol? GetConvertedType() => ConvertedType;

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