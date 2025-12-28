using System.Collections.Immutable;
using System.IO.Pipelines;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public static partial class SymbolExtensions
{
    /// <summary>
    /// Unwraps the actual type of a property symbol or local symbol. 
    /// If the symbol is a type symbol, then return it, otherwise null.
    /// </summary>
    /// <param name="symbol"></param>
    /// <returns></returns>
    public static ITypeSymbol? UnwrapType(this ISymbol symbol)
    {
        if (symbol is IPropertySymbol propertySymbol) return propertySymbol.Type;

        if (symbol is IFieldSymbol fieldSymbol) return fieldSymbol.Type;

        if (symbol is IParameterSymbol parameterSymbol) return parameterSymbol.Type;

        if (symbol is ILocalSymbol localSymbol) return localSymbol.Type;

        return symbol as ITypeSymbol;
    }

    public static INamedTypeSymbol? GetAbsoluteBaseType(this ITypeSymbol symbol)
    {
        INamedTypeSymbol? baseType = symbol.BaseType;

        if (baseType is null) return symbol as INamedTypeSymbol;

        while (baseType?.BaseType is not null)
        {
            baseType = baseType.BaseType;
        }

        return baseType;
    }

    public static bool IsNonNullableValueType(this ITypeSymbol symbol)
    {
        if (symbol.IsValueType)
            return true;

        if (symbol is not ITypeParameterSymbol typeParameter)
            return false;

        if ((typeParameter.ConstraintKind & TypeParameterConstraintKind.ValueType) != 0)
            return true;

        foreach (var constraint in typeParameter.ConstraintTypes)
        {
            if (constraint.IsValueType)
                return true;
        }

        return false;
    }

    public static IEnumerable<ISymbol> ResolveMembers(this ITypeSymbol symbol, string name)
    {
        var seenSignatures = new HashSet<string>();

        for (ITypeSymbol? current = symbol; current is not null; current = current.BaseType)
        {
            foreach (var member in current.GetMembers())
            {
                if (member.Name != name)
                    continue;

                // Vi skapar en unik signatursträng: return type + param types
                var signature = GetSignatureKey(member);

                // Har vi redan sett denna metodsignatur? Hoppa över base-definitioner.
                if (seenSignatures.Add(signature))
                    yield return member;
            }
        }
    }

    private static string GetSignatureKey(ISymbol symbol)
    {
        if (symbol is IMethodSymbol method)
        {
            var paramTypes = string.Join(",", method.Parameters.Select(p => p.Type.ToDisplayString()));
            return $"{method.Name}({paramTypes})";
        }

        return symbol.Name; // fallback för andra symboltyper
    }

    public static ITypeSymbol? UnwrapLiteralType(this ITypeSymbol? type)
        => type is LiteralTypeSymbol literal ? literal.UnderlyingType : type;

    public static bool IsExtensionProperty(this IPropertySymbol property)
    {
        return property switch
        {
            SourcePropertySymbol sourceProperty => sourceProperty.IsDeclaredInExtension,
            _ when property.GetMethod?.IsExtensionMethod == true => true,
            _ when property.SetMethod?.IsExtensionMethod == true => true,
            _ => false
        };
    }

    public static ITypeSymbol? GetExtensionReceiverType(this IPropertySymbol property)
    {
        if (property.GetMethod?.GetExtensionReceiverType() is { } getterReceiver)
            return getterReceiver;

        if (property.SetMethod?.GetExtensionReceiverType() is { } setterReceiver)
            return setterReceiver;

        if (property is SourcePropertySymbol sourceProperty && sourceProperty.IsDeclaredInExtension)
            return sourceProperty.ExtensionReceiverType;

        if (property.ContainingType?.GetExtensionReceiverType() is { } containerReceiver)
            return containerReceiver;

        return property switch
        {
            PEPropertySymbol peProperty => peProperty.ContainingType is PENamedTypeSymbol peType
                ? peType.GetExtensionMarkerReceiverType(peProperty)
                : null,
            _ => null
        };
    }

    public static ITypeSymbol? GetExtensionReceiverType(this IMethodSymbol method)
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
                        return SubstituteTypeParameters(markerReceiver, map);
                }

                return markerReceiver;
            }
        }

        if (method.MethodKind is MethodKind.Conversion or MethodKind.UserDefinedOperator &&
            !method.Parameters.IsDefaultOrEmpty)
        {
            return method.Parameters[0].Type;
        }

        if (method.IsExtensionMethod && !method.Parameters.IsDefaultOrEmpty)
            return method.Parameters[0].Type;

        if (method.ContainingType is PENamedTypeSymbol peType &&
            peType.GetExtensionReceiverType() is { } peReceiverType)
        {
            if (!method.TypeParameters.IsDefaultOrEmpty)
            {
                var map = new Dictionary<ITypeParameterSymbol, ITypeSymbol>(SymbolEqualityComparer.Default);
                MapReceiverTypeParameters(peReceiverType, method.TypeParameters, map);

                if (map.Count > 0)
                    return SubstituteTypeParameters(peReceiverType, map);
            }

            return peReceiverType;
        }

        if (method.ContainingType is SourceNamedTypeSymbol sourceType &&
            sourceType.IsExtensionDeclaration &&
            sourceType.ExtensionReceiverType is { } extensionReceiverType)
        {
            var containerParameters = sourceType.TypeParameters;
            var methodParameters = method.TypeParameters;

            if (!containerParameters.IsDefaultOrEmpty &&
                methodParameters.Length >= containerParameters.Length)
            {
                var map = new Dictionary<ITypeParameterSymbol, ITypeSymbol>(SymbolEqualityComparer.Default);
                for (int i = 0; i < containerParameters.Length; i++)
                    map[containerParameters[i]] = methodParameters[i];

                return SubstituteTypeParameters(extensionReceiverType, map);
            }

            return extensionReceiverType;
        }

        return method.ContainingType?.GetExtensionReceiverType();
    }

    public static bool HasStaticExtensionMembers(this INamedTypeSymbol type)
    {
        if (type.GetExtensionReceiverType() is not null)
            return true;

        return type switch
        {
            PENamedTypeSymbol peType => peType.HasExtensionMarkerMembers(),
            ConstructedNamedTypeSymbol constructed when constructed.OriginalDefinition is PENamedTypeSymbol peType
                => peType.HasExtensionMarkerMembers(),
            _ => false
        };
    }

    public static ITypeSymbol? GetExtensionReceiverType(this INamedTypeSymbol type)
    {
        return type switch
        {
            SourceNamedTypeSymbol sourceType when sourceType.IsExtensionDeclaration => sourceType.ExtensionReceiverType,
            ConstructedNamedTypeSymbol constructed when constructed.OriginalDefinition is SourceNamedTypeSymbol sourceType
                => sourceType.ExtensionReceiverType is { } receiverType ? constructed.Substitute(receiverType) : null,
            PENamedTypeSymbol peType => peType.GetExtensionReceiverType(),
            ConstructedNamedTypeSymbol constructed when constructed.OriginalDefinition is PENamedTypeSymbol peType
                => peType.GetExtensionReceiverType() is { } receiverType ? constructed.Substitute(receiverType) : null,
            _ => null
        };
    }

    private static ITypeSymbol SubstituteTypeParameters(
        ITypeSymbol type,
        Dictionary<ITypeParameterSymbol, ITypeSymbol> map)
    {
        if (type is ITypeParameterSymbol parameter &&
            map.TryGetValue(parameter, out var replacement))
        {
            return replacement;
        }

        if (type is NullableTypeSymbol nullableTypeSymbol)
        {
            var underlyingType = SubstituteTypeParameters(nullableTypeSymbol.UnderlyingType, map);

            if (!SymbolEqualityComparer.Default.Equals(underlyingType, nullableTypeSymbol.UnderlyingType))
                return new NullableTypeSymbol(underlyingType, nullableTypeSymbol.ContainingSymbol, nullableTypeSymbol.ContainingType, nullableTypeSymbol.ContainingNamespace, [.. nullableTypeSymbol.Locations]);

            return type;
        }

        if (type is ByRefTypeSymbol byRef)
        {
            var substitutedElement = SubstituteTypeParameters(byRef.ElementType, map);

            if (!SymbolEqualityComparer.Default.Equals(substitutedElement, byRef.ElementType))
                return new ByRefTypeSymbol(substitutedElement);

            return type;
        }

        if (type is IAddressTypeSymbol address)
        {
            var substitutedElement = SubstituteTypeParameters(address.ReferencedType, map);

            if (!SymbolEqualityComparer.Default.Equals(substitutedElement, address.ReferencedType))
                return new AddressTypeSymbol(substitutedElement);

            return type;
        }

        if (type is IArrayTypeSymbol arrayType)
        {
            var substitutedElement = SubstituteTypeParameters(arrayType.ElementType, map);

            if (!SymbolEqualityComparer.Default.Equals(substitutedElement, arrayType.ElementType))
                return new ArrayTypeSymbol(arrayType.BaseType, substitutedElement, arrayType.ContainingSymbol, arrayType.ContainingType, arrayType.ContainingNamespace, [], arrayType.Rank);

            return type;
        }

        if (type is IPointerTypeSymbol pointerType)
        {
            var substitutedElement = SubstituteTypeParameters(pointerType.PointedAtType, map);

            if (!SymbolEqualityComparer.Default.Equals(substitutedElement, pointerType.PointedAtType))
                return new PointerTypeSymbol(substitutedElement);

            return type;
        }

        if (type is INamedTypeSymbol namedType)
        {
            if (namedType.TypeArguments.IsDefaultOrEmpty)
                return type;

            var substitutedArguments = new ITypeSymbol[namedType.TypeArguments.Length];
            var changed = false;

            for (int i = 0; i < substitutedArguments.Length; i++)
            {
                var original = namedType.TypeArguments[i];
                var substituted = SubstituteTypeParameters(original, map);
                substitutedArguments[i] = substituted;
                changed |= !SymbolEqualityComparer.Default.Equals(original, substituted);
            }

            if (!changed)
                return type;

            return namedType.Construct([.. substitutedArguments]);
        }

        return type;
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
            case ByRefTypeSymbol byRef:
                MapReceiverTypeParameters(byRef.ElementType, methodParameters, map);
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

    public static ITypeSymbol? GetExtensionReceiverType(this ISymbol symbol)
    {
        return symbol switch
        {
            IMethodSymbol method => method.GetExtensionReceiverType(),
            IPropertySymbol property => property.GetExtensionReceiverType(),
            INamedTypeSymbol type => type.GetExtensionReceiverType(),
            _ => null
        };
    }
}
