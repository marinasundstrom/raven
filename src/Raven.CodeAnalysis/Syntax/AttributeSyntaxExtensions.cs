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

    internal static int GetMacroArity(this TypeSyntax name)
        => name.TryGetMacroTypeArgumentList(out var typeArgumentList)
            ? typeArgumentList.Arguments.Count
            : 0;

    internal static bool TryGetMacroTypeArgumentList(
        this TypeSyntax name,
        out TypeArgumentListSyntax typeArgumentList)
    {
        switch (name)
        {
            case GenericNameSyntax generic:
                typeArgumentList = generic.TypeArgumentList;
                return true;
            case QualifiedNameSyntax qualified:
                return qualified.Right.TryGetMacroTypeArgumentList(out typeArgumentList);
            case AliasQualifiedNameSyntax aliasQualified:
                return aliasQualified.Name.TryGetMacroTypeArgumentList(out typeArgumentList);
            default:
                typeArgumentList = null!;
                return false;
        }
    }

    public static bool TryGetMacroName(this InvocableMacroExpressionSyntax macroExpression, out string macroName)
    {
        if (macroExpression is null)
            throw new ArgumentNullException(nameof(macroExpression));

        macroName = GetNormalizedName(macroExpression.Name);
        return !string.IsNullOrWhiteSpace(macroName);
    }

    public static bool TryGetMacroName(this InvocableMacroMemberDeclarationSyntax macroInvocation, out string macroName)
    {
        if (macroInvocation is null)
            throw new ArgumentNullException(nameof(macroInvocation));

        macroName = GetNormalizedName(macroInvocation.Name);
        return !string.IsNullOrWhiteSpace(macroName);
    }
}
