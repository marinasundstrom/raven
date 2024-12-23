namespace Raven.CodeAnalysis;

public enum CandidateReason
{
    None,
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
    MemberGroup
}