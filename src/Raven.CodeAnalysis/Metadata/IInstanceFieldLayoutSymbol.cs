namespace Raven.CodeAnalysis.Metadata;

// Optional physical layout fact copied during import. Only backends whose target
// contract uses ordinal field addressing consume it; ordinary .NET uses named refs.
// Scope is the containing type and its assembly's exact ResolvedAssemblyArtifact.
internal interface IInstanceFieldLayoutSymbol
{
    int InstanceStorageOrdinal { get; }
}
