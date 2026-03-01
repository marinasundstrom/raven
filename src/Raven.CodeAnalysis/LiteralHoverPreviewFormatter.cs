using System.Globalization;
using System.Text;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal static class LiteralHoverPreviewFormatter
{
    private static readonly SymbolDisplayFormat PlainTypeFormat = SymbolDisplayFormat.RavenSignatureFormat
        .WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameOnly)
        .WithKindOptions(SymbolDisplayKindOptions.None);

    public static bool TryCreatePreview(
        SemanticModel semanticModel,
        SyntaxToken token,
        out string preview,
        out TextSpan span)
    {
        preview = string.Empty;
        span = default;

        if (token.Parent is not LiteralExpressionSyntax literalExpression)
            return false;

        var type = semanticModel.GetTypeInfo(literalExpression).Type?.UnwrapLiteralType();
        if (type is null)
            return false;

        if (!TryFormatLiteralValue(token, out var valueDisplay))
            return false;

        preview = $"{type.ToDisplayString(PlainTypeFormat)} = {valueDisplay}";
        span = literalExpression.Span;
        return true;
    }

    private static bool TryFormatLiteralValue(SyntaxToken token, out string valueDisplay)
    {
        valueDisplay = string.Empty;
        var value = token.Value;

        if (value is EncodedStringLiteralValue encoded)
        {
            var encoding = encoded.Encoding == EncodedStringLiteralEncoding.Utf8 ? "utf8" : "ascii";
            valueDisplay = $"{QuoteAndEscape(encoded.Text)} ({encoding})";
            return true;
        }

        if (value is string plainString &&
            token.IsKind(SyntaxKind.StringLiteralToken) &&
            TryGetEncodedStringMarker(token.Text, out var marker))
        {
            valueDisplay = $"{QuoteAndEscape(plainString)} ({marker})";
            return true;
        }

        switch (value)
        {
            case null when token.IsKind(SyntaxKind.NullKeyword):
                valueDisplay = "null";
                return true;
            case string text:
                valueDisplay = QuoteAndEscape(text);
                return true;
            case char ch:
                valueDisplay = QuoteAndEscape(ch.ToString(), quote: '\'');
                return true;
            case bool flag:
                valueDisplay = flag ? "true" : "false";
                return true;
            case float f:
                valueDisplay = f.ToString("R", CultureInfo.InvariantCulture);
                return true;
            case double d:
                valueDisplay = d.ToString("R", CultureInfo.InvariantCulture);
                return true;
            case decimal m:
                valueDisplay = m.ToString(CultureInfo.InvariantCulture);
                return true;
            case sbyte or byte or short or ushort or int or uint or long or ulong:
                valueDisplay = Convert.ToString(value, CultureInfo.InvariantCulture) ?? value.ToString() ?? string.Empty;
                return true;
            default:
                if (value is not null)
                {
                    valueDisplay = value.ToString() ?? string.Empty;
                    return true;
                }

                return false;
        }
    }

    private static bool TryGetEncodedStringMarker(string tokenText, out string marker)
    {
        marker = string.Empty;

        if (string.IsNullOrEmpty(tokenText))
            return false;

        if (tokenText.EndsWith("u8", StringComparison.Ordinal))
        {
            marker = "utf8";
            return true;
        }

        if (tokenText.EndsWith("ascii", StringComparison.Ordinal))
        {
            marker = "ascii";
            return true;
        }

        return false;
    }

    private static string QuoteAndEscape(string value, char quote = '"')
    {
        var builder = new StringBuilder(value.Length + 2);
        builder.Append(quote);

        foreach (var ch in value)
        {
            builder.Append(ch switch
            {
                '\\' => "\\\\",
                '"' when quote == '"' => "\\\"",
                '\'' when quote == '\'' => "\\'",
                '\n' => "\\n",
                '\r' => "\\r",
                '\t' => "\\t",
                '\0' => "\\0",
                _ when char.IsControl(ch) => $"\\u{(int)ch:X4}",
                _ => ch.ToString()
            });
        }

        builder.Append(quote);
        return builder.ToString();
    }
}
