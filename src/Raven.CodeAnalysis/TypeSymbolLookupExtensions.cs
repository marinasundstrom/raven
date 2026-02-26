namespace Raven.CodeAnalysis;

public static class TypeSymbolLookupExtensions
{
    public static IMethodSymbol? GetMethodRecursive(this ITypeSymbol type, string name, ITypeSymbol returnType, ITypeSymbol[] paramTypes)
    {
        var currentType = type;
        while (currentType != null)
        {
            var method = currentType
                .GetMembers(name)
                .OfType<IMethodSymbol>()
                .FirstOrDefault(MatchesSignature);

            if (method is not null)
                return method;

            currentType = currentType.BaseType;
        }

        return null;

        bool MatchesSignature(IMethodSymbol m)
        {
            // Match by return type.
            if (!SymbolEqualityComparer.Default.Equals(m.ReturnType, returnType))
                return false;

            // Match by parameter list.
            if (m.Parameters.Length != paramTypes.Length)
                return false;

            for (var i = 0; i < paramTypes.Length; i++)
            {
                if (!SymbolEqualityComparer.Default.Equals(m.Parameters[i].Type, paramTypes[i]))
                    return false;
            }

            return true;
        }
    }

    [Flags]
    public enum MemberLookupFlags
    {
        None = 0,

        // What kinds of members to consider.
        Methods = 1 << 0,
        Properties = 1 << 1,
        Fields = 1 << 2,
        Types = 1 << 3,
        Events = 1 << 4,

        // Search behavior.
        IncludeBaseTypes = 1 << 16,

        // Convenience.
        AnyMember = Methods | Properties | Fields | Types | Events,

        Default = AnyMember | IncludeBaseTypes,
    }

    public static ISymbol? GetFirstMemberRecursive(
        this ITypeSymbol type,
        string name,
        MemberLookupFlags flags = MemberLookupFlags.Default,
        Func<ISymbol, bool>? predicate = null)
    {
        if ((flags & MemberLookupFlags.AnyMember) == 0)
            flags |= MemberLookupFlags.AnyMember;

        var includeBaseTypes = (flags & MemberLookupFlags.IncludeBaseTypes) != 0;

        for (var currentType = type; currentType != null; currentType = includeBaseTypes ? currentType.BaseType : null)
        {
            foreach (var member in currentType.GetMembers(name))
            {
                if (!IsAllowedKind(member, flags))
                    continue;

                if (predicate is not null && !predicate(member))
                    continue;

                return member;
            }

            if (!includeBaseTypes)
                break;
        }

        return null;

        static bool IsAllowedKind(ISymbol member, MemberLookupFlags flags)
        {
            return member switch
            {
                IMethodSymbol => (flags & MemberLookupFlags.Methods) != 0,
                IPropertySymbol => (flags & MemberLookupFlags.Properties) != 0,
                IFieldSymbol => (flags & MemberLookupFlags.Fields) != 0,
                ITypeSymbol => (flags & MemberLookupFlags.Types) != 0,
                IEventSymbol => (flags & MemberLookupFlags.Events) != 0,
                _ => false,
            };
        }
    }
}
