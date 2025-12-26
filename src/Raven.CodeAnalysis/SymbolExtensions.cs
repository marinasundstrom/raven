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
        if (property is SourcePropertySymbol sourceProperty && sourceProperty.IsDeclaredInExtension)
            return sourceProperty.ExtensionReceiverType;

        if (property.GetMethod?.GetExtensionReceiverType() is { } getterReceiver)
            return getterReceiver;

        if (property.SetMethod?.GetExtensionReceiverType() is { } setterReceiver)
            return setterReceiver;

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
            var receiverType = peOriginal.ContainingType is PENamedTypeSymbol peType
                ? peType.GetExtensionMarkerReceiverType(peOriginal)
                : null;

            if (receiverType is not null)
                return constructed.Substitute(receiverType);
        }

        if (method is PEMethodSymbol peMethod && peMethod.ContainingType is PENamedTypeSymbol peContaining)
        {
            var markerReceiver = peContaining.GetExtensionMarkerReceiverType(peMethod);
            if (markerReceiver is not null)
                return markerReceiver;
        }

        if (method.IsExtensionMethod && !method.Parameters.IsDefaultOrEmpty)
            return method.Parameters[0].Type;

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
