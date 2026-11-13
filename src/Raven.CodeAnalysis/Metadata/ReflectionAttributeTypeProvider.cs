using System;

namespace Raven.CodeAnalysis.Metadata;

/// <summary>Type resolution owned by the reflection metadata importer.</summary>
internal interface ReflectionAttributeTypeProvider
{
    NamedTypeSymbol? GetTypeByMetadataName(string metadataName);

    TypeSymbol? ResolveType(Type type);
}
