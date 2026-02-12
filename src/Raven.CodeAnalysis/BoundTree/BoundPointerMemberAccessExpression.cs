namespace Raven.CodeAnalysis;

internal partial class BoundPointerMemberAccessExpression : BoundExpression
{
    public BoundPointerMemberAccessExpression(
        BoundExpression pointerReceiver,
        ISymbol member,
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

    public ISymbol Member { get; }

    private static ITypeSymbol GetMemberType(ISymbol member)
    {
        return member switch
        {
            IFieldSymbol field => field.Type,
            IPropertySymbol prop => prop.Type,
            IMethodSymbol method => method.ReturnType,
            IEventSymbol @event => @event.Type,
            _ => throw new InvalidOperationException($"Unsupported member type: {member.GetType()}")
        };
    }
}
