namespace Raven.CodeAnalysis.Metadata;

// Provider-owned declaration fact used by shared namespace-member lookup.
// A provider may derive it from metadata, native declarations or in-memory data;
// consumers must not resolve attributes or require a particular symbol class.
internal interface INamespaceMemberContainer
{
    // Identifies candidate containers only. Member filtering, lookup precedence
    // and the namespace-member import option remain compiler-owned decisions.
    bool IsNamespaceMemberContainer { get; }
}
