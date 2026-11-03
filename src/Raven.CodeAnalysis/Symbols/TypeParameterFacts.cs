using System.Collections.Generic;

namespace Raven.CodeAnalysis.Symbols;

internal static class TypeParameterFacts
{
    internal static bool IsKnownReferenceType(TypeParameterSymbol parameter)
    {
        var pending = new Stack<TypeParameterSymbol>();
        var visited = new HashSet<TypeParameterSymbol>(SymbolEqualityComparer.Default);
        pending.Push(parameter);
        while (pending.TryPop(out var current))
        {
            if (!visited.Add(current))
                continue;
            if ((current.ConstraintKind & TypeParameterConstraintKind.ReferenceType) != 0)
                return true;
            foreach (var constraint in current.ConstraintTypes)
            {
                var type = constraint.GetNonNullableType();
                if (type is TypeParameterSymbol next)
                    pending.Push(next);
                else if (type is NamedTypeSymbol { TypeKind: TypeKind.Class } &&
                         type.SpecialType is not (SpecialType.System_Object or SpecialType.System_ValueType or SpecialType.System_Enum))
                    return true;
            }
        }
        return false;
    }
}
