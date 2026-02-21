namespace Raven.CodeAnalysis;

internal enum BoundTryStatementKind
{
    UserAuthored = 0,
    UsingLifetime = 1,
    AsyncDispatchGuard = 2,
    ExceptionProjection = 3,
    PropagateRewrite = 4,
}

