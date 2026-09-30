namespace Raven.CodeAnalysis.Metadata;

// Optional type-only discovery, avoiding full member/signature materialization.
// Results are traversal candidates, not member lookup on a closed generic type:
// providers may expose nested declarations in their original definition context.
internal interface INestedTypeDiscovery
{
    IEnumerable<INamedTypeSymbol> GetNestedTypesForDiscovery();
}
