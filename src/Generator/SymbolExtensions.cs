namespace Generator;

using System.Linq;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

public static class SymbolExtensions
{
    public static bool IsPartial(this IPropertySymbol propertySymbol)
    {
        return propertySymbol.DeclaringSyntaxReferences.Any(syntaxRef =>
            syntaxRef.GetSyntax() is PropertyDeclarationSyntax propertySyntax &&
            propertySyntax.Modifiers.Any(modifier => modifier.IsKind(SyntaxKind.PartialKeyword)));
    }

    public static bool InheritsFromSyntaxNode(this ITypeSymbol classSymbol)
    {
        // Traverse the inheritance hierarchy to check if the type derives from SyntaxNode
        var baseType = classSymbol.BaseType;
        while (baseType != null)
        {
            if (baseType.Name == "SyntaxNode" && baseType.ContainingNamespace.ToDisplayString() == "Raven.CodeAnalysis.Syntax")
            {
                return true;
            }
            baseType = baseType.BaseType;
        }

        return false;
    }

    public static bool InheritsFromInternalSyntaxNode(this ITypeSymbol classSymbol)
    {
        // Traverse the inheritance hierarchy to check if the type derives from SyntaxNode
        var baseType = classSymbol.BaseType;
        while (baseType != null)
        {
            if (baseType.Name == "SyntaxNode" && baseType.ContainingNamespace.ToDisplayString() == "Raven.CodeAnalysis.Syntax.InternalSyntax")
            {
                return true;
            }
            baseType = baseType.BaseType;
        }

        return false;
    }

    public static bool InheritsFromBoundNode(this ITypeSymbol classSymbol)
    {
        // Traverse the inheritance hierarchy to check if the type derives from SyntaxNode
        var baseType = classSymbol.BaseType;
        while (baseType != null)
        {
            if (baseType.Name == "BoundNode" && baseType.ContainingNamespace.ToDisplayString() == "Raven.CodeAnalysis")
            {
                return true;
            }
            baseType = baseType.BaseType;
        }

        return false;
    }

    public static bool InheritsFromSymbol(this ITypeSymbol classSymbol)
    {
        // Traverse the inheritance hierarchy to check if the type derives from SyntaxNode
        var baseType = classSymbol.BaseType;
        while (baseType != null)
        {
            if (baseType.Name == "Symbol" && baseType.ContainingNamespace.ToDisplayString() == "Raven.CodeAnalysis.Symbols")
            {
                return true;
            }
            baseType = baseType.BaseType;
        }

        return false;
    }

    public static bool IsImplementingISymbol(this ITypeSymbol? classSymbol)
    {
        if (classSymbol is null)
            return false;

        foreach (var iface in classSymbol.AllInterfaces)
        {
            if (iface.Name == "ISymbol" &&
                SymbolEqualityComparer.Default.Equals(iface.ContainingNamespace,
                    iface.ContainingNamespace.ContainingCompilation?.GlobalNamespace
                        .GetNamespaceMembers().FirstOrDefault(ns => ns.Name == "Raven")?
                        .GetNamespaceMembers().FirstOrDefault(ns => ns.Name == "CodeAnalysis")))
            {
                return true;
            }
        }

        return false;
    }

    public static bool IsISymbol(this ITypeSymbol? classSymbol)
    {
        return classSymbol.Name == "ISymbol";
    }

    public static ITypeSymbol? GetElementType(this ITypeSymbol type)
    {
        return type switch
        {
            IArrayTypeSymbol array => array.ElementType,
            INamedTypeSymbol named when named.IsGenericType =>
                named.TypeArguments.Length == 1 ? named.TypeArguments[0] : null,
            _ => null
        };
    }

    public static bool IsArrayType(this ITypeSymbol typeSymbol)
    {
        return typeSymbol is IArrayTypeSymbol;
    }
}