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
        return property switch
        {
            SourcePropertySymbol sourceProperty when sourceProperty.IsDeclaredInExtension => sourceProperty.ExtensionReceiverType,
            _ => null
        };
    }
}
