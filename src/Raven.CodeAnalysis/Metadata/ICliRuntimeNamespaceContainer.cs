namespace Raven.CodeAnalysis.Metadata;

// Exact CLI runtime namespace-function ownership, distinct from permissive
// TopLevel-name matching used by ordinary namespace-member import.
internal interface ICliRuntimeNamespaceContainer
{
    bool IsRuntimeNamespaceContainer { get; }
}
