using System.Collections.Generic;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Metadata;

namespace Raven.CodeAnalysis.Symbols;

internal partial class PENamedTypeSymbol
{
    TypeSymbol? IExtensionReceiverResolver.GetExtensionReceiverType(PropertySymbol property)
        => property is PEPropertySymbol peProperty
            ? GetExtensionMarkerReceiverType(peProperty)
            : null;

    TypeSymbol? IExtensionReceiverResolver.GetExtensionReceiverType(MethodSymbol method)
    {
        if (method.OriginalDefinition is PEMethodSymbol peOriginal &&
            method.ContainingType is ConstructedNamedTypeSymbol constructed)
        {
            var markerReceiverType = peOriginal.ContainingType is PENamedTypeSymbol peMarkerType
                ? peMarkerType.GetExtensionMarkerReceiverType(peOriginal)
                : null;

            if (markerReceiverType is not null)
                return constructed.Substitute(markerReceiverType);
        }

        if (method is PEMethodSymbol peMethod && peMethod.ContainingType is PENamedTypeSymbol peContaining)
        {
            var markerReceiver = peContaining.GetExtensionMarkerReceiverType(peMethod);
            if (markerReceiver is not null)
            {
                if (!method.TypeParameters.IsDefaultOrEmpty)
                {
                    var map = new Dictionary<TypeParameterSymbol, TypeSymbol>(SymbolEqualityComparer.Default);
                    MapReceiverTypeParameters(markerReceiver, method.TypeParameters, map);

                    if (map.Count > 0)
                        return SymbolExtensions.SubstituteTypeParameters(markerReceiver, map);
                }

                return markerReceiver;
            }
        }
        if (method.ContainingType is PENamedTypeSymbol peType &&
            peType.GetExtensionReceiverType() is { } peReceiverType)
        {
            if (!method.TypeParameters.IsDefaultOrEmpty)
            {
                var map = new Dictionary<TypeParameterSymbol, TypeSymbol>(SymbolEqualityComparer.Default);
                MapReceiverTypeParameters(peReceiverType, method.TypeParameters, map);

                if (map.Count > 0)
                    return SymbolExtensions.SubstituteTypeParameters(peReceiverType, map);
            }

            return peReceiverType;
        }

        return null;
    }

    private static void MapReceiverTypeParameters(
        TypeSymbol receiverType,
        ImmutableArray<TypeParameterSymbol> methodParameters,
        Dictionary<TypeParameterSymbol, TypeSymbol> map)
    {
        var methodOwner = methodParameters.IsDefaultOrEmpty ? null : methodParameters[0].ContainingSymbol;

        switch (receiverType)
        {
            case TypeParameterSymbol parameter:
                if (!Equals(parameter.ContainingSymbol, methodOwner) &&
                    parameter.Ordinal < methodParameters.Length)
                {
                    map.TryAdd(parameter, methodParameters[parameter.Ordinal]);
                }
                break;
            case NullableTypeSymbol nullableType:
                MapReceiverTypeParameters(nullableType.UnderlyingType, methodParameters, map);
                break;
            case RefTypeSymbol refType:
                MapReceiverTypeParameters(refType.ElementType, methodParameters, map);
                break;
            case AddressTypeSymbol address:
                MapReceiverTypeParameters(address.ReferencedType, methodParameters, map);
                break;
            case ArrayTypeSymbol arrayType:
                MapReceiverTypeParameters(arrayType.ElementType, methodParameters, map);
                break;
            case PointerTypeSymbol pointerType:
                MapReceiverTypeParameters(pointerType.PointedAtType, methodParameters, map);
                break;
            case NamedTypeSymbol namedType:
                foreach (var arg in namedType.TypeArguments)
                    MapReceiverTypeParameters(arg, methodParameters, map);
                break;
        }
    }

}
