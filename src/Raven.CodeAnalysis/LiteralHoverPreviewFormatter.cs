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

        if (token.Parent?.AncestorsAndSelf().OfType<DefaultExpressionSyntax>().FirstOrDefault() is { } defaultExpression)
        {
            var defaultType = semanticModel.GetTypeInfo(defaultExpression).Type ??
                              defaultExpression.AncestorsAndSelf()
                                  .OfType<ParameterSyntax>()
                                  .Select(parameter => semanticModel.GetDeclaredSymbol(parameter))
                                  .OfType<IParameterSymbol>()
                                  .Select(parameter => parameter.Type)
                                  .Concat(defaultExpression.AncestorsAndSelf()
                                      .OfType<ParameterSyntax>()
                                      .Select(parameter => parameter.TypeAnnotation?.Type)
                                      .OfType<TypeSyntax>()
                                      .Select(type => semanticModel.GetTypeInfo(type).Type))
                                  .OfType<ITypeSymbol>()
                                  .FirstOrDefault();

            var fallbackTypeSyntax = defaultExpression.Type ??
                                     defaultExpression.AncestorsAndSelf()
                                         .OfType<ParameterSyntax>()
                                         .Select(parameter => parameter.TypeAnnotation?.Type)
                                         .FirstOrDefault(type => type is not null);

            string? typeDisplay = null;
            if (defaultType is not null && defaultType.TypeKind != TypeKind.Error)
                typeDisplay = defaultType.ToDisplayString(PlainTypeFormat);
            else if (fallbackTypeSyntax is not null)
                typeDisplay = fallbackTypeSyntax.ToString();

            if (string.IsNullOrWhiteSpace(typeDisplay))
                return false;

            preview = FormatDefaultPreview(typeDisplay, defaultType, fallbackTypeSyntax);
            span = defaultExpression.Span;
            return true;
        }

        if (token.Parent is not LiteralExpressionSyntax literalExpression)
            return false;

        var type = semanticModel.GetTypeInfo(literalExpression).Type?.UnwrapLiteralType();
        if (type is null)
            return false;

        if (!TryFormatLiteralValue(token, out var valueDisplay))
            return false;

        preview = FormatExpressionPreview(token.Text, type.ToDisplayString(PlainTypeFormat), valueDisplay);
        span = literalExpression.Span;
        return true;
    }

    private static string FormatExpressionPreview(
        string expressionDisplay,
        string typeDisplay,
        string valueDisplay,
        bool alwaysShowValue = false)
    {
        if (!alwaysShowValue && string.Equals(expressionDisplay, valueDisplay, StringComparison.Ordinal))
            return $"{expressionDisplay}: {typeDisplay}";

        return $"{expressionDisplay}: {typeDisplay} = {valueDisplay}";
    }

    private static string FormatDefaultPreview(string typeDisplay, ITypeSymbol? type, TypeSyntax? fallbackTypeSyntax)
    {
        var expressionDisplay = $"default({typeDisplay})";
        if (!TryFormatKnownDefaultValue(type, out var valueDisplay) &&
            !TryFormatKnownDefaultValue(fallbackTypeSyntax, out valueDisplay))
        {
            return expressionDisplay;
        }

        return $"{expressionDisplay} = {valueDisplay}";
    }

    private static bool TryFormatKnownDefaultValue(ITypeSymbol? type, out string valueDisplay)
    {
        valueDisplay = string.Empty;

        if (type is null || type.TypeKind == TypeKind.Error)
            return false;

        if (type.TypeKind is TypeKind.Nullable or TypeKind.Null ||
            type.IsReferenceType)
        {
            valueDisplay = "null";
            return true;
        }

        switch (type.SpecialType)
        {
            case SpecialType.System_Boolean:
                valueDisplay = "false";
                return true;
            case SpecialType.System_Char:
                valueDisplay = "'\\0'";
                return true;
            case SpecialType.System_SByte:
            case SpecialType.System_Byte:
            case SpecialType.System_Int16:
            case SpecialType.System_UInt16:
            case SpecialType.System_Int32:
            case SpecialType.System_UInt32:
            case SpecialType.System_Int64:
            case SpecialType.System_UInt64:
            case SpecialType.System_IntPtr:
            case SpecialType.System_UIntPtr:
            case SpecialType.System_Decimal:
                valueDisplay = "0";
                return true;
            case SpecialType.System_Single:
            case SpecialType.System_Double:
                valueDisplay = "0.0";
                return true;
            default:
                if (type.TypeKind == TypeKind.Unit)
                {
                    valueDisplay = "()";
                    return true;
                }

                return false;
        }
    }

    private static bool TryFormatKnownDefaultValue(TypeSyntax? typeSyntax, out string valueDisplay)
    {
        valueDisplay = string.Empty;

        switch (typeSyntax)
        {
            case NullableTypeSyntax:
                valueDisplay = "null";
                return true;
            case UnitTypeSyntax:
                valueDisplay = "()";
                return true;
            case PredefinedTypeSyntax predefined:
                return TryFormatKnownDefaultValue(predefined.Keyword.Kind, out valueDisplay);
            default:
                return false;
        }
    }

    private static bool TryFormatKnownDefaultValue(SyntaxKind kind, out string valueDisplay)
    {
        switch (kind)
        {
            case SyntaxKind.BoolKeyword:
                valueDisplay = "false";
                return true;
            case SyntaxKind.CharKeyword:
                valueDisplay = "'\\0'";
                return true;
            case SyntaxKind.FloatKeyword:
            case SyntaxKind.DoubleKeyword:
                valueDisplay = "0.0";
                return true;
            case SyntaxKind.SByteKeyword:
            case SyntaxKind.ByteKeyword:
            case SyntaxKind.ShortKeyword:
            case SyntaxKind.UShortKeyword:
            case SyntaxKind.IntKeyword:
            case SyntaxKind.UIntKeyword:
            case SyntaxKind.LongKeyword:
            case SyntaxKind.ULongKeyword:
            case SyntaxKind.DecimalKeyword:
                valueDisplay = "0";
                return true;
            case SyntaxKind.StringKeyword:
            case SyntaxKind.ObjectKeyword:
                valueDisplay = "null";
                return true;
            default:
                valueDisplay = string.Empty;
                return false;
        }
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
