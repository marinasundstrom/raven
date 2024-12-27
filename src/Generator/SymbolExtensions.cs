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
}