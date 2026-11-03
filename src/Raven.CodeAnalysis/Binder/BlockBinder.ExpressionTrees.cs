using System;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

partial class BlockBinder
{
    private static bool TryGetExpressionTreeDelegateType(TypeSymbol type, out NamedTypeSymbol delegateType)
    {
        delegateType = null!;

        static TypeSymbol Unalias(TypeSymbol symbol)
        {
            while (symbol.IsAlias && symbol.UnderlyingSymbol is TypeSymbol underlying)
                symbol = underlying;

            return symbol;
        }

        type = Unalias(type);

        if (type is NullableTypeSymbol nullable)
            type = nullable.UnderlyingType;

        if (type is not NamedTypeSymbol named)
            return false;

        var definition = (named.OriginalDefinition as NamedTypeSymbol) ?? named;
        if (definition.Arity != 1)
            return false;

        var isExpressionType =
            string.Equals(definition.Name, "Expression", StringComparison.Ordinal) ||
            string.Equals(definition.MetadataName, "Expression`1", StringComparison.Ordinal);
        if (!isExpressionType)
            return false;

        if (named.TypeArguments.Length != 1)
            return false;

        var candidate = Unalias(named.TypeArguments[0]);
        if (candidate is not NamedTypeSymbol candidateDelegate)
            return false;

        if (candidateDelegate.TypeKind != TypeKind.Delegate &&
            candidateDelegate.GetDelegateInvokeMethod() is null)
            return false;

        delegateType = candidateDelegate;
        return true;
    }
}
