using System.Collections.Generic;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal static class AccessibilityUtilities
{
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

    public static Accessibility GetDefaultTypeAccessibility(ISymbol? containingSymbol)
    {
        if (containingSymbol is null)
            return Accessibility.Internal;

        if (containingSymbol is INamespaceSymbol)
            return Accessibility.Internal;

        if (containingSymbol is INamedTypeSymbol containingType)
        {
            if (containingType.TypeKind == TypeKind.Interface)
                return Accessibility.Public;

            return Accessibility.Private;
        }

        return Accessibility.Internal;
    }

    public static Accessibility GetDefaultMemberAccessibility(INamedTypeSymbol containingType)
    {
        if (containingType.TypeKind == TypeKind.Interface)
            return Accessibility.Public;

        return Accessibility.Public;
    }

    public static bool IsAccessible(ISymbol symbol, ISymbol? within)
    {
        if (symbol is null)
            return true;

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

    private static bool IsAccessibleFromPrivateScope(ISymbol symbol, ISymbol? within)
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

    private static bool IsAccessibleFromInternalScope(ISymbol symbol, ISymbol? within)
    {
        var symbolAssembly = symbol.ContainingAssembly;
        if (symbolAssembly is null)
            return true;

        var withinAssembly = GetContainingAssembly(within);
        if (withinAssembly is null)
            return true;

        return SymbolEqualityComparer.Default.Equals(symbolAssembly, withinAssembly);
    }

    private static IAssemblySymbol? GetContainingAssembly(ISymbol? symbol)
    {
        if (symbol is null)
            return null;

        if (symbol is IAssemblySymbol assembly)
            return assembly;

        if (symbol.ContainingAssembly is not null)
            return symbol.ContainingAssembly;

        return GetContainingAssembly(symbol.ContainingSymbol);
    }

    private static bool IsAccessibleFromProtectedScope(ISymbol symbol, ISymbol? within)
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

    private static INamedTypeSymbol? GetProtectedContainer(ISymbol symbol)
    {
        if (symbol is INamedTypeSymbol namedType)
            return namedType.ContainingType;

        return symbol.ContainingType;
    }

    private static IEnumerable<INamedTypeSymbol> GetContainingTypes(ISymbol symbol)
    {
        for (var current = symbol; current is not null; current = current.ContainingSymbol)
        {
            if (current is INamedTypeSymbol type)
                yield return type;
        }
    }

    private static bool IsTypeOrDerivedFrom(INamedTypeSymbol derived, INamedTypeSymbol potentialBase)
    {
        for (var current = derived; current is not null; current = current.BaseType)
        {
            if (SymbolEqualityComparer.Default.Equals(current, potentialBase))
                return true;
        }

        return false;
    }
}
