namespace Raven.CodeAnalysis;

internal partial class BoundMemberAccessExpression : BoundExpression
{
    public BoundMemberAccessExpression(BoundExpression? receiver, Symbol member, BoundExpressionReason reason = BoundExpressionReason.None)
        : base(GetMemberType(member), member, reason)
    {
        Receiver = receiver;
        Member = member;
    }

    public BoundExpression? Receiver { get; }
    public Symbol Member { get; }

    private static TypeSymbol GetMemberType(Symbol member)
    {
        return member switch
        {
            FieldSymbol field => field.Type,
            PropertySymbol prop => prop.Type,
            MethodSymbol method => method.ReturnType,
            EventSymbol @event => @event.Type,
            _ => throw new InvalidOperationException($"Unsupported member type: {member.GetType()}")
        };
    }
}
