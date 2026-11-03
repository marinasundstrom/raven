using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal static class AccessibilityUtilities
{
    public static string GetDisplayText(Accessibility accessibility)
    {
        return accessibility switch
        {
            Accessibility.ProtectedAndProtected => "protected",
            Accessibility.ProtectedOrInternal => "protected internal",
            Accessibility.ProtectedAndInternal => "private protected",
            _ => accessibility.ToString().ToLowerInvariant(),
        };
    }

    public static Accessibility DetermineAccessibility(
        IEnumerable<SyntaxToken> modifiers,
        Accessibility defaultAccessibility)
    {
        var hasPublic = false;
        var hasPrivate = false;
        var hasProtected = false;
        var hasInternal = false;

        foreach (var modifier in modifiers)
        {
            switch (modifier.Kind)
            {
                case SyntaxKind.PublicKeyword:
                    hasPublic = true;
                    break;
                case SyntaxKind.PrivateKeyword:
                    hasPrivate = true;
                    break;
                case SyntaxKind.ProtectedKeyword:
                    hasProtected = true;
                    break;
                case SyntaxKind.InternalKeyword:
                    hasInternal = true;
                    break;
            }
        }

        if (hasPublic)
            return Accessibility.Public;

        if (hasProtected && hasInternal && !hasPrivate)
            return Accessibility.ProtectedOrInternal;

        if (hasProtected && hasPrivate && !hasInternal)
            return Accessibility.ProtectedAndInternal;

        if (hasProtected && hasInternal && hasPrivate)
            return Accessibility.ProtectedAndInternal;

        if (hasProtected)
            return Accessibility.ProtectedAndProtected;

        if (hasInternal)
            return Accessibility.Internal;

        if (hasPrivate)
            return Accessibility.Private;

        return defaultAccessibility;
    }

    public static bool HasFileScopeModifier(IEnumerable<SyntaxToken> modifiers)
        => modifiers.Any(static modifier => modifier.Kind == SyntaxKind.FileprivateKeyword);

    public static Accessibility GetDefaultTypeAccessibility(Symbol? containingSymbol)
    {
        if (containingSymbol is null)
            return Accessibility.Internal;

        if (containingSymbol is NamespaceSymbol)
            return Accessibility.Internal;

        if (containingSymbol is NamedTypeSymbol)
            return Accessibility.Public;

        return Accessibility.Internal;
    }

    public static bool IsAccessible(Symbol symbol, Symbol? within, string? withinFilePath = null)
    {
        if (symbol is null)
            return true;

        if (symbol is SymbolBase sourceSymbol &&
            sourceSymbol.IsFileScoped &&
            !sourceSymbol.IsAccessibleFromFile(withinFilePath))
        {
            return false;
        }

        return symbol.DeclaredAccessibility switch
        {
            Accessibility.Public => true,
            Accessibility.NotApplicable => true,
            Accessibility.Private => IsAccessibleFromPrivateScope(symbol, within),
            Accessibility.Internal => IsAccessibleFromInternalScope(symbol, within),
            Accessibility.ProtectedAndProtected => IsAccessibleFromProtectedScope(symbol, within),
            Accessibility.ProtectedOrInternal =>
                IsAccessibleFromProtectedScope(symbol, within) ||
                IsAccessibleFromInternalScope(symbol, within),
            Accessibility.ProtectedAndInternal =>
                IsAccessibleFromProtectedScope(symbol, within) &&
                IsAccessibleFromInternalScope(symbol, within),
            _ => true,
        };
    }

    private static bool IsAccessibleFromPrivateScope(Symbol symbol, Symbol? within)
    {
        for (var current = within; current is not null; current = current.ContainingSymbol)
        {
            if (SymbolEqualityComparer.Default.Equals(current, symbol))
                return true;

            if (symbol.ContainingType is not null &&
                SymbolEqualityComparer.Default.Equals(current, symbol.ContainingType))
                return true;

            if (symbol.ContainingSymbol is not null &&
                SymbolEqualityComparer.Default.Equals(current, symbol.ContainingSymbol))
                return true;
        }

        return false;
    }

    private static bool IsAccessibleFromInternalScope(Symbol symbol, Symbol? within)
    {
        var symbolAssembly = symbol.ContainingAssembly;
        if (symbolAssembly is null)
            return true;

        var withinAssembly = GetContainingAssembly(within);
        if (withinAssembly is null)
            return true;

        return SymbolEqualityComparer.Default.Equals(symbolAssembly, withinAssembly)
            || withinAssembly is not null && FriendAssemblyAccess.IsGranted(symbolAssembly, withinAssembly);
    }

    private static AssemblySymbol? GetContainingAssembly(Symbol? symbol)
    {
        if (symbol is null)
            return null;

        if (symbol is AssemblySymbol assembly)
            return assembly;

        if (symbol.ContainingAssembly is not null)
            return symbol.ContainingAssembly;

        return GetContainingAssembly(symbol.ContainingSymbol);
    }

    private static bool IsAccessibleFromProtectedScope(Symbol symbol, Symbol? within)
    {
        var protectedContainer = GetProtectedContainer(symbol);
        if (protectedContainer is null)
            return false;

        if (within is null)
            return false;

        foreach (var containingType in GetContainingTypes(within))
        {
            if (IsTypeOrDerivedFrom(containingType, protectedContainer))
                return true;
        }

        return false;
    }

    private static NamedTypeSymbol? GetProtectedContainer(Symbol symbol)
    {
        if (symbol is NamedTypeSymbol namedType)
            return namedType.ContainingType;

        return symbol.ContainingType;
    }

    private static IEnumerable<NamedTypeSymbol> GetContainingTypes(Symbol symbol)
    {
        for (var current = symbol; current is not null; current = current.ContainingSymbol)
        {
            if (current is NamedTypeSymbol type)
                yield return type;
        }
    }

    private static bool IsTypeOrDerivedFrom(NamedTypeSymbol derived, NamedTypeSymbol potentialBase)
    {
        if (potentialBase.TypeKind == TypeKind.Interface &&
            derived.AllInterfaces.Any(i => SymbolEqualityComparer.Default.Equals(i, potentialBase)))
            return true;

        for (var current = derived; current is not null; current = current.BaseType)
        {
            if (SymbolEqualityComparer.Default.Equals(current, potentialBase))
                return true;
        }

        return false;
    }

    public static bool IsTypeLessAccessibleThan(TypeSymbol type, Accessibility requiredAccessibility)
        => GetLessAccessibleConstituentTypes(type, requiredAccessibility).Count != 0;

    public static IReadOnlyList<TypeSymbol> GetLessAccessibleConstituentTypes(
        TypeSymbol type,
        Accessibility requiredAccessibility)
    {
        if (type is null)
            return [];

        if (type.TypeKind == TypeKind.Error)
            return [];

        var visiting = new HashSet<TypeSymbol>(TypeSymbolReferenceComparer.Instance);
        var reported = new HashSet<TypeSymbol>(TypeSymbolReferenceComparer.Instance);
        var inaccessibleTypes = new List<TypeSymbol>();
        AddLessAccessibleConstituentTypes(
            type,
            NormalizeAccessibility(requiredAccessibility),
            visiting,
            reported,
            inaccessibleTypes);
        return inaccessibleTypes;
    }

    private static void AddLessAccessibleConstituentTypes(
        TypeSymbol type,
        Accessibility requiredAccessibility,
        HashSet<TypeSymbol> visiting,
        HashSet<TypeSymbol> reported,
        List<TypeSymbol> inaccessibleTypes)
    {
        if (type.TypeKind == TypeKind.Error || !visiting.Add(type))
            return;

        try
        {
            if (type is FunctionReturnTypeSymbol projection)
            {
                AddLessAccessibleConstituentTypes(projection.FunctionType, requiredAccessibility, visiting, reported, inaccessibleTypes);
                return;
            }
            // Accessibility of a native function signature follows its component types.
            if (type is StructuralFunctionTypeSymbol function)
            {
                foreach (var component in function.ParameterTypes.Append(function.ReturnType))
                    AddLessAccessibleConstituentTypes(component, requiredAccessibility, visiting, reported, inaccessibleTypes);
                return;
            }
            if (!IsAtLeastAsAccessibleAs(NormalizeAccessibility(type.DeclaredAccessibility), requiredAccessibility))
            {
                if (reported.Add(type))
                    inaccessibleTypes.Add(type);
            }

            if (type.ContainingType is { } containingType)
            {
                AddLessAccessibleConstituentTypes(
                    containingType,
                    requiredAccessibility,
                    visiting,
                    reported,
                    inaccessibleTypes);
            }

            TypeSymbol? wrappedType = type switch
            {
                ArrayTypeSymbol arrayType => arrayType.ElementType,
                PointerTypeSymbol pointerType => pointerType.PointedAtType,
                AddressTypeSymbol addressType => addressType.ReferencedType,
                NullableTypeSymbol nullableType => nullableType.UnderlyingType,
                RefTypeSymbol refType => refType.ElementType,
                _ => null,
            };

            if (wrappedType is not null)
            {
                AddLessAccessibleConstituentTypes(
                    wrappedType,
                    requiredAccessibility,
                    visiting,
                    reported,
                    inaccessibleTypes);
            }

            if (type is NamedTypeSymbol namedType && !namedType.TypeArguments.IsDefaultOrEmpty)
            {
                foreach (var typeArgument in namedType.TypeArguments)
                {
                    AddLessAccessibleConstituentTypes(
                        typeArgument,
                        requiredAccessibility,
                        visiting,
                        reported,
                        inaccessibleTypes);
                }
            }
        }
        finally
        {
            visiting.Remove(type);
        }
    }

    public static Accessibility GetEffectiveAccessibility(Accessibility accessibility, NamedTypeSymbol? containingType)
    {
        var effectiveAccessibility = NormalizeAccessibility(accessibility);

        for (var current = containingType; current is not null; current = current.ContainingType)
        {
            var currentAccessibility = NormalizeAccessibility(current.DeclaredAccessibility);
            effectiveAccessibility = Intersect(effectiveAccessibility, currentAccessibility);
        }

        return effectiveAccessibility;
    }

    private sealed class TypeSymbolReferenceComparer : IEqualityComparer<TypeSymbol>
    {
        public static TypeSymbolReferenceComparer Instance { get; } = new();

        public bool Equals(TypeSymbol? x, TypeSymbol? y) => ReferenceEquals(x, y);

        public int GetHashCode(TypeSymbol obj) => RuntimeHelpers.GetHashCode(obj);
    }

    private static Accessibility Intersect(Accessibility first, Accessibility second)
    {
        first = NormalizeAccessibility(first);
        second = NormalizeAccessibility(second);

        if (first == second || second == Accessibility.Public)
            return first;

        if (first == Accessibility.Public)
            return second;

        if (first == Accessibility.Private || second == Accessibility.Private)
            return Accessibility.Private;

        if (first == Accessibility.ProtectedAndInternal || second == Accessibility.ProtectedAndInternal)
            return Accessibility.ProtectedAndInternal;

        if (first == Accessibility.ProtectedOrInternal)
            return second;

        if (second == Accessibility.ProtectedOrInternal)
            return first;

        // The only remaining unequal pair is protected and internal. Their
        // intersection is private protected.
        return Accessibility.ProtectedAndInternal;
    }

    private static Accessibility NormalizeAccessibility(Accessibility accessibility)
    {
        if (accessibility == Accessibility.NotApplicable)
            return Accessibility.Public;

        return accessibility;
    }

    private static bool IsAtLeastAsAccessibleAs(Accessibility candidate, Accessibility required)
    {
        return required switch
        {
            Accessibility.Private => true,
            Accessibility.ProtectedAndInternal => candidate != Accessibility.Private,
            Accessibility.ProtectedAndProtected => candidate is Accessibility.Public
                or Accessibility.ProtectedAndProtected
                or Accessibility.ProtectedOrInternal,
            Accessibility.Internal => candidate is Accessibility.Public
                or Accessibility.Internal
                or Accessibility.ProtectedOrInternal,
            Accessibility.ProtectedOrInternal => candidate is Accessibility.Public
                or Accessibility.ProtectedOrInternal,
            _ => candidate == Accessibility.Public,
        };
    }
}
