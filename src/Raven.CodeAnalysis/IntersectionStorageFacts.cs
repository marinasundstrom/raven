using System.Collections.Generic;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal static class IntersectionStorageFacts
{
    internal static bool ContainsIntersection(ITypeSymbol? type)
    {
        HashSet<ITypeSymbol>? visited = null;
        return Visit(type);

        bool Visit(ITypeSymbol? current)
        {
            switch (current)
            {
                case IIntersectionTypeSymbol:
                    return true;
                case NullableTypeSymbol nullable:
                    return Visit(nullable.UnderlyingType);
                case IArrayTypeSymbol array:
                    return Visit(array.ElementType);
                case RefTypeSymbol reference:
                    return Visit(reference.ElementType);
                case IAddressTypeSymbol address:
                    return Visit(address.ReferencedType);
                case IPointerTypeSymbol pointer:
                    return Visit(pointer.PointedAtType);
                case ITupleTypeSymbol tuple:
                    foreach (var element in tuple.TupleElements)
                    {
                        if (Visit(element.Type))
                            return true;
                    }
                    return false;
                case INamedTypeSymbol named:
                    if (named.TypeArguments.IsDefaultOrEmpty && named.ContainingType is null &&
                        named.TypeKind != TypeKind.Delegate)
                        return false;

                    if (!(visited ??= new(ReferenceEqualityComparer.Instance)).Add(named))
                        return false;

                    if (Visit(named.ContainingType))
                        return true;
                    foreach (var argument in named.TypeArguments)
                    {
                        if (Visit(argument))
                            return true;
                    }

                    if (named.TypeKind == TypeKind.Delegate && named.GetDelegateInvokeMethod() is { } invoke)
                    {
                        if (Visit(invoke.ReturnType))
                            return true;
                        foreach (var parameter in invoke.Parameters)
                        {
                            if (Visit(parameter.Type))
                                return true;
                        }
                    }
                    return false;
                default:
                    // Nominal members, base types, and type-parameter constraints
                    // are not part of the stored type's structural shape.
                    return false;
            }
        }
    }
}
