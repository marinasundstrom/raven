using System.Collections.Generic;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Metadata;

namespace Raven.CodeAnalysis.Symbols;

internal partial class PENamedTypeSymbol
{
    ITypeSymbol? IExtensionReceiverResolver.GetExtensionReceiverType(IPropertySymbol property)
        => property is PEPropertySymbol peProperty
            ? GetExtensionMarkerReceiverType(peProperty)
            : null;

    ITypeSymbol? IExtensionReceiverResolver.GetExtensionReceiverType(IMethodSymbol method)
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
                    var map = new Dictionary<ITypeParameterSymbol, ITypeSymbol>(SymbolEqualityComparer.Default);
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
                var map = new Dictionary<ITypeParameterSymbol, ITypeSymbol>(SymbolEqualityComparer.Default);
                MapReceiverTypeParameters(peReceiverType, method.TypeParameters, map);

                if (map.Count > 0)
                    return SymbolExtensions.SubstituteTypeParameters(peReceiverType, map);
            }

            return peReceiverType;
        }

        return null;
    }

    private static void MapReceiverTypeParameters(
        ITypeSymbol receiverType,
        ImmutableArray<ITypeParameterSymbol> methodParameters,
        Dictionary<ITypeParameterSymbol, ITypeSymbol> map)
    {
        var methodOwner = methodParameters.IsDefaultOrEmpty ? null : methodParameters[0].ContainingSymbol;

        switch (receiverType)
        {
            case ITypeParameterSymbol parameter:
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
            case IAddressTypeSymbol address:
                MapReceiverTypeParameters(address.ReferencedType, methodParameters, map);
                break;
            case IArrayTypeSymbol arrayType:
                MapReceiverTypeParameters(arrayType.ElementType, methodParameters, map);
                break;
            case IPointerTypeSymbol pointerType:
                MapReceiverTypeParameters(pointerType.PointedAtType, methodParameters, map);
                break;
            case INamedTypeSymbol namedType:
                foreach (var arg in namedType.TypeArguments)
                    MapReceiverTypeParameters(arg, methodParameters, map);
                break;
        }
    }

}
