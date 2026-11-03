namespace Raven.CodeAnalysis;

internal partial class BoundPointerMemberAccessExpression : BoundExpression
{
    public BoundPointerMemberAccessExpression(
        BoundExpression pointerReceiver,
        Symbol member,
        BoundExpressionReason reason = BoundExpressionReason.None)
        : base(GetMemberType(member), member, reason)
    {
        PointerReceiver = pointerReceiver ?? throw new ArgumentNullException(nameof(pointerReceiver));
        Member = member ?? throw new ArgumentNullException(nameof(member));
    }

    /// <summary>
    /// The pointer expression (e.g. `x` in `x->A`). This expression evaluates to an address/pointer to the containing type.
    /// </summary>
    public BoundExpression PointerReceiver { get; }

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
