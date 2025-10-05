using System;
using System.Collections.Generic;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen.Metadata;

internal sealed class MetadataModuleBuilder
{
    private readonly Dictionary<ITypeSymbol, MetadataTypeDefinition> _types = new(SymbolEqualityComparer.Default);

    public MetadataTypeDefinition GetOrAddTypeDefinition(ITypeSymbol type)
    {
        if (type is null)
            throw new ArgumentNullException(nameof(type));

        if (!_types.TryGetValue(type, out var definition))
        {
            definition = new MetadataTypeDefinition(type);
            _types.Add(type, definition);

            if (type is INamedTypeSymbol named && named.ContainingType is not null)
            {
                var containingDefinition = GetOrAddTypeDefinition(named.ContainingType);
                containingDefinition.AddNestedType(definition);
            }
        }

        return definition;
    }

    public bool TryGetTypeDefinition(ITypeSymbol type, out MetadataTypeDefinition definition)
    {
        if (type is null)
            throw new ArgumentNullException(nameof(type));

        return _types.TryGetValue(type, out definition);
    }

    public IEnumerable<MetadataTypeDefinition> GetAllTypeDefinitions() => _types.Values;
}
