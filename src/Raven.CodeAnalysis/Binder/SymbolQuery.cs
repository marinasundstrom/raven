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
                : ResolveInstanceMembersIncludingInterfaces(containingType, Name);
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

    private static IEnumerable<ISymbol> ResolveInstanceMembersIncludingInterfaces(ITypeSymbol type, string name)
    {
        // NOTE: `ResolveMembers` currently seems to walk BaseType but not Interfaces.
        // This breaks patterns like IEnumerator<T>.MoveNext(), where MoveNext is declared
        // on the non-generic IEnumerator base interface.
        //
        // Important: when we add interface members we must avoid duplicates/hidden members
        // (e.g. List<T>.Add vs ICollection<T>.Add, IEnumerable<T>.GetEnumerator vs IEnumerable.GetEnumerator).
        // We therefore de-duplicate by signature (name + parameter types), preferring members found earlier.

        if (type is not INamedTypeSymbol named)
            return type.GetMembers(name);

        var seenKeys = new HashSet<string>();
        var results = new List<ISymbol>();

        void AddFrom(INamedTypeSymbol t)
        {
            foreach (var m in t.GetMembers(name))
            {
                // Ignore return type for the purpose of de-duplication; this matches .NET interface hiding patterns.
                var key = GetSignatureKey(m);
                if (seenKeys.Add(key))
                    results.Add(m);
            }
        }

        // 1) Declared + base types (class/struct inheritance). These should win over interface members.
        for (INamedTypeSymbol? t = named; t is not null; t = t.BaseType)
            AddFrom(t);

        // 2) Interfaces (flattened). For interfaces, AllInterfaces includes inherited interfaces.
        // For classes, AllInterfaces includes interfaces from base types too.
        foreach (var iface in named.AllInterfaces)
            AddFrom(iface);

        return results;
    }

    private IEnumerable<ISymbol> LookupTypeParameterMembers(Binder binder, ITypeParameterSymbol typeParameter)
    {
        binder.EnsureTypeParameterConstraintTypesResolved(ImmutableArray.Create(typeParameter));

        var preferStaticMembers = IsStatic == true;
        var constraintTypes = GetConstraintLookupTypes(binder, typeParameter, includeObjectMembers: !preferStaticMembers);
        if (constraintTypes.IsDefaultOrEmpty)
            return Enumerable.Empty<ISymbol>();

        var seenSignatures = new HashSet<string>();
        var results = new List<ISymbol>();

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

        if (type is NullableTypeSymbol nullable)
            return nullable.UnderlyingType;

        if (type is ITypeUnionSymbol union)
        {
            var nonNullMembers = union.Types.Where(member => member.TypeKind != TypeKind.Null).ToArray();
            if (nonNullMembers.Length == 1)
                return nonNullMembers[0];
        }

        return type;
    }

    private static string GetSignatureKey(ISymbol symbol)
    {
        // We intentionally do NOT include return type: interface members like
        // IEnumerable.GetEnumerator() and IEnumerable<T>.GetEnumerator() differ only by return type
        // and are meant to be treated as a single callable member (the more-derived one wins).

        return symbol switch
        {
            IMethodSymbol method => $"M:{method.Name}({string.Join(",", method.Parameters.Select(p => p.Type.ToDisplayString()))})",
            IPropertySymbol property => $"P:{property.Name}",
            IFieldSymbol field => $"F:{field.Name}",
            IEventSymbol evt => $"E:{evt.Name}",
            _ => $"O:{symbol.Name}"
        };
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
