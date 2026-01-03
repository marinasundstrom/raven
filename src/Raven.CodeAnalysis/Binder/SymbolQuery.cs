using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Microsoft.CodeAnalysis;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal readonly record struct SymbolQuery(
    string Name,
    ITypeSymbol? ContainingType = null,
    int? Arity = null,
    bool? IsStatic = null)
{
    public IEnumerable<ISymbol> Lookup(Binder binder)
    {
        IEnumerable<ISymbol> symbols;
        var containingType = NormalizeContainingType(ContainingType);
        if (containingType is ITypeParameterSymbol typeParameter)
        {
            symbols = LookupTypeParameterMembers(binder, typeParameter);
        }
        else if (containingType is not null)
        {
            symbols = IsStatic == true
                ? containingType.GetMembers(Name)
                : containingType.ResolveMembers(Name);
        }
        else
        {
            symbols = binder.LookupSymbols(Name);
        }

        var isStatic = IsStatic;
        var arity = Arity;

        if (isStatic.HasValue)
        {
            var requireStatic = isStatic.Value;
            symbols = symbols.Where(symbol =>
            {
                if (symbol is ITypeSymbol)
                    return requireStatic;

                return symbol.IsStatic == requireStatic;
            });
        }

        if (arity.HasValue)
            symbols = symbols.Where(s => s is IMethodSymbol m && SupportsArgumentCount(m.Parameters, arity.Value));

        return symbols;
    }

    public IEnumerable<IMethodSymbol> LookupMethods(Binder binder) =>
        Lookup(binder).OfType<IMethodSymbol>();

    private IEnumerable<ISymbol> LookupTypeParameterMembers(Binder binder, ITypeParameterSymbol typeParameter)
    {
        binder.EnsureTypeParameterConstraintTypesResolved(ImmutableArray.Create(typeParameter));

        var constraintTypes = GetConstraintLookupTypes(binder, typeParameter);
        if (constraintTypes.IsDefaultOrEmpty)
            return Enumerable.Empty<ISymbol>();

        var seenSignatures = new HashSet<string>();
        var results = new List<ISymbol>();

        foreach (var constraint in constraintTypes)
        {
            foreach (var member in constraint.ResolveMembers(Name))
            {
                var signature = GetSignatureKey(member);
                if (seenSignatures.Add(signature))
                    results.Add(member);
            }
        }

        return results;
    }

    private static ImmutableArray<ITypeSymbol> GetConstraintLookupTypes(Binder binder, ITypeParameterSymbol typeParameter)
    {
        var builder = ImmutableArray.CreateBuilder<ITypeSymbol>();
        var seen = new HashSet<ITypeSymbol>(SymbolEqualityComparer.Default);

        foreach (var constraint in typeParameter.ConstraintTypes)
        {
            if (constraint is IErrorTypeSymbol)
                continue;

            if (seen.Add(constraint))
                builder.Add(constraint);
        }

        if ((typeParameter.ConstraintKind & TypeParameterConstraintKind.ValueType) != 0)
        {
            var valueType = binder.Compilation.GetSpecialType(SpecialType.System_ValueType);
            if (seen.Add(valueType))
                builder.Add(valueType);
        }

        if ((typeParameter.ConstraintKind & TypeParameterConstraintKind.ReferenceType) != 0)
        {
            var objectType = binder.Compilation.GetSpecialType(SpecialType.System_Object);
            if (seen.Add(objectType))
                builder.Add(objectType);
        }

        return builder.ToImmutable();
    }

    private static ITypeSymbol? NormalizeContainingType(ITypeSymbol? type)
    {
        while (type is IAliasSymbol alias && alias.UnderlyingSymbol is ITypeSymbol aliasType)
            type = aliasType;

        if (type is LiteralTypeSymbol literal)
            return literal.UnderlyingType;

        return type;
    }

    private static string GetSignatureKey(ISymbol symbol)
    {
        if (symbol is IMethodSymbol method)
        {
            var paramTypes = string.Join(",", method.Parameters.Select(p => p.Type.ToDisplayString()));
            return $"{method.Name}({paramTypes})";
        }

        return symbol.Name;
    }

    private static bool SupportsArgumentCount(ImmutableArray<IParameterSymbol> parameters, int argumentCount)
    {
        if (argumentCount > parameters.Length)
            return false;

        var required = parameters.Length;
        while (required > 0 && parameters[required - 1].HasExplicitDefaultValue)
            required--;

        return argumentCount >= required;
    }
}
