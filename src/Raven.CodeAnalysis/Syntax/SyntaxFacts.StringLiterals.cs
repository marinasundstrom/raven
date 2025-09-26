using System;
using System.Text;

namespace Raven.CodeAnalysis.Syntax;

public static partial class SyntaxFacts
{
    public static string DecodeStringLiteralContent(ReadOnlySpan<char> text, out bool hadInvalidEscape)
    {
        if (text.IsEmpty)
        {
            hadInvalidEscape = false;
            return string.Empty;
        }

        var builder = new StringBuilder(text.Length);
        hadInvalidEscape = false;

        for (int i = 0; i < text.Length; i++)
        {
            var ch = text[i];
            if (ch != '\\')
            {
                builder.Append(ch);
                continue;
            }

            if (i + 1 >= text.Length)
            {
                hadInvalidEscape = true;
                builder.Append('?');
                break;
            }

            var escape = text[++i];
            switch (escape)
            {
                case '\"':
                    builder.Append('\"');
                    break;
                case '\\':
                    builder.Append('\\');
                    break;
                case '\0':
                    builder.Append('\0');
                    break;
                case 'a':
                    builder.Append('\a');
                    break;
                case 'b':
                    builder.Append('\b');
                    break;
                case 'f':
                    builder.Append('\f');
                    break;
                case 'n':
                    builder.Append('\n');
                    break;
                case 'r':
                    builder.Append('\r');
                    break;
                case 't':
                    builder.Append('\t');
                    break;
                case 'v':
                    builder.Append('\v');
                    break;
                case '$':
                    builder.Append('$');
                    break;
                case 'u':
                {
                    var remaining = text[(i + 1)..];
                    if (!TryDecodeUnicodeEscape(remaining, digits: 4, allowBraces: true, out var rune, out var consumed))
                    {
                        hadInvalidEscape = true;
                        builder.Append('?');
                        i += consumed;
                    }
                    else
                    {
                        builder.Append(rune.ToString());
                        i += consumed;
                    }
                    break;
                }
                case 'U':
                {
                    var remaining = text[(i + 1)..];
                    if (!TryDecodeUnicodeEscape(remaining, digits: 8, allowBraces: false, out var rune, out var consumed))
                    {
                        hadInvalidEscape = true;
                        builder.Append('?');
                        i += consumed;
                    }
                    else
                    {
                        builder.Append(rune.ToString());
                        i += consumed;
                    }
                    break;
                }
                default:
                    hadInvalidEscape = true;
                    builder.Append('?');
                    break;
            }
        }

        return builder.ToString();
    }

    public static bool TryDecodeUnicodeEscape(
        ReadOnlySpan<char> text,
        int digits,
        bool allowBraces,
        out Rune rune,
        out int charsConsumed)
    {
        rune = default;
        charsConsumed = 0;

        if (allowBraces && !text.IsEmpty && text[0] == '{')
        {
            var value = 0;
            var digitCount = 0;
            for (int i = 1; i < text.Length; i++)
            {
                var c = text[i];
                if (c == '}')
                {
                    charsConsumed = i + 1;
                    if (digitCount == 0)
                    {
                        return false;
                    }

                    return Rune.TryCreate(value, out rune);
                }

                if (!IsHexDigit(c))
                {
                    charsConsumed = i + 1;
                    return false;
                }

                value = (value << 4) + GetHexValue(c);
                if (value > 0x10FFFF)
                {
                    charsConsumed = i + 1;
                    return false;
                }

                digitCount++;
                if (digitCount > 8)
                {
                    charsConsumed = i + 1;
                    return false;
                }
            }

            charsConsumed = text.Length;
            return false;
        }

        if (text.Length < digits)
        {
            charsConsumed = text.Length;
            return false;
        }

        var scalar = 0;
        for (int i = 0; i < digits; i++)
        {
            var c = text[i];
            if (!IsHexDigit(c))
            {
                charsConsumed = i + 1;
                return false;
            }

            scalar = (scalar << 4) + GetHexValue(c);
        }

        charsConsumed = digits;
        return Rune.TryCreate(scalar, out rune);
    }

    public static bool IsHexDigit(char ch)
    {
        return (ch >= '0' && ch <= '9') ||
               (ch >= 'a' && ch <= 'f') ||
               (ch >= 'A' && ch <= 'F');
    }

    public static int GetHexValue(char ch)
    {
        if (ch >= '0' && ch <= '9')
            return ch - '0';
        if (ch >= 'a' && ch <= 'f')
            return ch - 'a' + 10;
        if (ch >= 'A' && ch <= 'F')
            return ch - 'A' + 10;

        throw new ArgumentOutOfRangeException(nameof(ch));
    }
}
