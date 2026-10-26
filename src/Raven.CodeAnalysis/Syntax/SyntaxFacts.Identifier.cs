using System;
using System.Globalization;

namespace Raven.CodeAnalysis.Syntax;

public static partial class SyntaxFacts
{
    internal static bool RequiresBacktickEscaping(string identifier)
    {
        if (identifier.Length == 0)
            return false;

        if (identifier == "_" || !IsIdentifierStartCharacter(identifier[0]))
            return true;

        for (var i = 1; i < identifier.Length; i++)
            if (!IsIdentifierPartCharacter(identifier[i]))
                return true;

        return false;
    }

    internal static string EscapeIdentifier(string identifier)
        => RequiresBacktickEscaping(identifier) ? "`" + identifier + "`"
            : TryParseKeyword(identifier, out _) ? "@" + identifier : identifier;

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
