using System;
using System.Globalization;

namespace Raven.CodeAnalysis.Syntax;

public static partial class SyntaxFacts
{
    public static bool CanBeIdentifier(SyntaxKind kind)
    {
        return kind == SyntaxKind.IdentifierToken ||
               (IsKeywordKind(kind) && !IsReservedWordKind(kind));
    }

    public static bool IsIdentifierStartCharacter(char ch)
    {
        if (ch == '_' || ch == '$')
        {
            return true;
        }

        if (ch <= 0x7F)
        {
            return (ch >= 'A' && ch <= 'Z') ||
                   (ch >= 'a' && ch <= 'z');
        }

        var category = CharUnicodeInfo.GetUnicodeCategory(ch);

        return category == UnicodeCategory.UppercaseLetter ||
               category == UnicodeCategory.LowercaseLetter ||
               category == UnicodeCategory.TitlecaseLetter ||
               category == UnicodeCategory.ModifierLetter ||
               category == UnicodeCategory.OtherLetter ||
               category == UnicodeCategory.LetterNumber;
    }

    public static bool IsIdentifierPartCharacter(char ch)
    {
        if (IsIdentifierStartCharacter(ch))
        {
            return true;
        }

        if (ch <= 0x7F)
        {
            return ch >= '0' && ch <= '9';
        }

        var category = CharUnicodeInfo.GetUnicodeCategory(ch);

        return category == UnicodeCategory.DecimalDigitNumber ||
               category == UnicodeCategory.ConnectorPunctuation ||
               category == UnicodeCategory.NonSpacingMark ||
               category == UnicodeCategory.SpacingCombiningMark ||
               category == UnicodeCategory.Format;
    }
}
