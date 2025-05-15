namespace Raven.CodeAnalysis;

sealed class BoundMemberAccessExpression : BoundExpression
{
    public BoundMemberAccessExpression(BoundExpression receiver, ISymbol member, CandidateReason reason = CandidateReason.None)
        : base(GetMemberType(member), member, reason)
    {
        Receiver = receiver;
        Member = member;
    }

    public BoundExpression Receiver { get; }
    public ISymbol Member { get; }

    private static ITypeSymbol GetMemberType(ISymbol member)
    {
        return member switch
        {
            IFieldSymbol field => field.Type,
            IPropertySymbol prop => prop.Type,
            IMethodSymbol method => method.ReturnType,
            _ => throw new InvalidOperationException($"Unsupported member type: {member.GetType()}")
        };
    }
}