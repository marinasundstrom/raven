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

        var constraintTypes = GetConstraintLookupTypes(binder, typeParameter, includeObjectMembers: !preferStaticMembers);
        if (constraintTypes.IsDefaultOrEmpty)
            return Enumerable.Empty<ISymbol>();

        var seenSignatures = new HashSet<string>();
        var results = new List<ISymbol>();

        var preferStaticMembers = IsStatic == true;

        foreach (var constraint in constraintTypes)
        {
            var members = preferStaticMembers
                ? GetStaticAbstractConstraintMembers(constraint)
                : constraint.ResolveMembers(Name);

            foreach (var member in members)
            {
                var signature = GetSignatureKey(member);
                if (seenSignatures.Add(signature))
                    results.Add(member);
            }
        }

        return results;
    }

    private IEnumerable<ISymbol> GetStaticAbstractConstraintMembers(ITypeSymbol constraint)
    {
        if (constraint.TypeKind != TypeKind.Interface)
            return Enumerable.Empty<ISymbol>();

        return constraint
            .GetMembers(Name)
            .Where(IsStaticAbstractMember);
    }

    private static bool IsStaticAbstractMember(ISymbol member)
    {
        if (!member.IsStatic)
            return false;

        return member switch
        {
            IMethodSymbol method => method.IsAbstract,
            IPropertySymbol property => property.GetMethod?.IsAbstract == true || property.SetMethod?.IsAbstract == true,
            _ => false
        };
    }

    private static ImmutableArray<ITypeSymbol> GetConstraintLookupTypes(
        Binder binder,
        ITypeParameterSymbol typeParameter,
        bool includeObjectMembers)
    {
        var builder = ImmutableArray.CreateBuilder<ITypeSymbol>();
        var seen = new HashSet<ITypeSymbol>(SymbolEqualityComparer.Default);
        var hasValueTypeConstraint = (typeParameter.ConstraintKind & TypeParameterConstraintKind.ValueType) != 0;
        var hasReferenceTypeConstraint = (typeParameter.ConstraintKind & TypeParameterConstraintKind.ReferenceType) != 0;

        foreach (var constraint in typeParameter.ConstraintTypes)
        {
            if (constraint is IErrorTypeSymbol)
                continue;

            if (seen.Add(constraint))
                builder.Add(constraint);
        }

        if (hasValueTypeConstraint)
        {
            var valueType = binder.Compilation.GetSpecialType(SpecialType.System_ValueType);
            if (seen.Add(valueType))
                builder.Add(valueType);
        }

        if (hasReferenceTypeConstraint)
        {
            var objectType = binder.Compilation.GetSpecialType(SpecialType.System_Object);
            if (seen.Add(objectType))
                builder.Add(objectType);
        }

        if (includeObjectMembers && !hasValueTypeConstraint && !hasReferenceTypeConstraint)
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
