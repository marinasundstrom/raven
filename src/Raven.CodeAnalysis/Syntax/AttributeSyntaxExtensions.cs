using System;

namespace Raven.CodeAnalysis.Syntax;

public static class AttributeSyntaxExtensions
{
    public static bool IsMacroAttribute(this AttributeSyntax attribute)
    {
        if (attribute is null)
            throw new ArgumentNullException(nameof(attribute));

        return attribute.HashToken.Kind == SyntaxKind.HashToken;
    }

    public static bool TryGetMacroName(this AttributeSyntax attribute, out string macroName)
    {
        if (attribute is null)
            throw new ArgumentNullException(nameof(attribute));

        if (!attribute.IsMacroAttribute())
        {
            macroName = string.Empty;
            return false;
        }

        macroName = GetNormalizedName(attribute.Name);
        return true;
    }

    private static string GetNormalizedName(TypeSyntax name)
        => name switch
        {
            IdentifierNameSyntax identifier => identifier.Identifier.ValueText,
            GenericNameSyntax generic => generic.Identifier.ValueText,
            QualifiedNameSyntax qualified => $"{GetNormalizedName(qualified.Left)}.{GetNormalizedName(qualified.Right)}",
            AliasQualifiedNameSyntax aliasQualified => $"{aliasQualified.Alias.Identifier.ValueText}::{GetNormalizedName(aliasQualified.Name)}",
            _ => name.ToString()
        };
}
