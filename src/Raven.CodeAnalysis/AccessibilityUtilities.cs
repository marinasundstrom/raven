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
}
