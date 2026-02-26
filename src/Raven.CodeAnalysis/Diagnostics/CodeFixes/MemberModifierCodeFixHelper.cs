using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Diagnostics;

internal static class MemberModifierCodeFixHelper
{
    public static TextChange? CreateMakePrivateChange(MemberDeclarationSyntax declaration)
    {
        var modifiers = declaration.Modifiers;
        if (modifiers.Any(t => t.Kind == SyntaxKind.PrivateKeyword))
            return null;

        if (TryGetAccessibilityModifierRange(modifiers, out var accessibilitySpan))
            return new TextChange(accessibilitySpan, "private");

        var insertPosition = GetModifierInsertPosition(declaration);
        return insertPosition is null
            ? null
            : new TextChange(new TextSpan(insertPosition.Value, 0), "private ");
    }

    public static TextChange? CreateMakeStaticChange(MemberDeclarationSyntax declaration)
    {
        var modifiers = declaration.Modifiers;
        if (modifiers.Any(t => t.Kind == SyntaxKind.StaticKeyword))
            return null;

        var insertPosition = GetStaticInsertPosition(declaration);
        return insertPosition is null
            ? null
            : new TextChange(new TextSpan(insertPosition.Value, 0), "static ");
    }

    private static int? GetStaticInsertPosition(MemberDeclarationSyntax declaration)
    {
        var modifiers = declaration.Modifiers;
        if (modifiers.Count == 0)
            return GetModifierInsertPosition(declaration);

        var insertionIndex = 0;
        while (insertionIndex < modifiers.Count && IsAccessibilityModifier(modifiers[insertionIndex].Kind))
            insertionIndex++;

        if (insertionIndex < modifiers.Count)
            return modifiers[insertionIndex].SpanStart;

        return modifiers[modifiers.Count - 1].Span.End;
    }

    private static bool TryGetAccessibilityModifierRange(SyntaxTokenList modifiers, out TextSpan span)
    {
        span = default;

        var firstIndex = -1;
        for (var i = 0; i < modifiers.Count; i++)
        {
            if (!IsAccessibilityModifier(modifiers[i].Kind))
                continue;

            firstIndex = i;
            break;
        }

        if (firstIndex < 0)
            return false;

        var lastIndex = firstIndex;
        if (firstIndex + 1 < modifiers.Count && IsAccessibilityModifier(modifiers[firstIndex + 1].Kind))
            lastIndex = firstIndex + 1;

        var start = modifiers[firstIndex].SpanStart;
        var end = modifiers[lastIndex].Span.End;
        span = new TextSpan(start, end - start);
        return true;
    }

    private static int? GetModifierInsertPosition(MemberDeclarationSyntax declaration)
    {
        if (declaration.Modifiers.Count > 0)
            return declaration.Modifiers[0].SpanStart;

        return declaration switch
        {
            MethodDeclarationSyntax method => method.FuncKeyword.SpanStart,
            PropertyDeclarationSyntax property => (property.BindingKeyword.Kind == SyntaxKind.None
                    ? property.Identifier
                    : property.BindingKeyword).SpanStart,
            FieldDeclarationSyntax field => field.FieldKeyword.SpanStart,
            EventDeclarationSyntax eventDecl => eventDecl.EventKeyword.SpanStart,
            IndexerDeclarationSyntax indexer => indexer.BindingKeyword.SpanStart,
            _ => null
        };
    }

    private static bool IsAccessibilityModifier(SyntaxKind kind)
    {
        return kind is SyntaxKind.PublicKeyword
            or SyntaxKind.PrivateKeyword
            or SyntaxKind.ProtectedKeyword
            or SyntaxKind.InternalKeyword;
    }
}
