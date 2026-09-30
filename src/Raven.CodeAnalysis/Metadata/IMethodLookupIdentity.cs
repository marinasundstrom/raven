namespace Raven.CodeAnalysis.Metadata;

/// <summary>
/// Optional provider-owned declaration key for shallow method deduplication.
/// Keys must be provider-qualified and stable across views of the declaration.
/// This is not symbol equality or a persistent metadata identity. Implementations
/// should avoid resolving parameter types; core adds generic method arguments.
/// </summary>
internal interface IMethodLookupIdentity
{
    string ShallowDeclarationLookupKey { get; }
}
