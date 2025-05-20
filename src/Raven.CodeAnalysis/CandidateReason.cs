namespace Raven.CodeAnalysis;

public enum CandidateReason
{
    None,
    NotFound,
    NotATypeOrNamespace,
    NotAnEvent,
    NotAWithEventsMember,
    NotAnAttributeType,
    WrongArity,
    NotCreatable,
    NotReferenceable,
    Inaccessible,
    NotAValue,
    NotInvocable,
    StaticInstanceMismatch,
    OverloadResolutionFailure,
    LateBound,
    Ambiguous,
    MemberGroup,

}
