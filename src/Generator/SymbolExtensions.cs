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
}