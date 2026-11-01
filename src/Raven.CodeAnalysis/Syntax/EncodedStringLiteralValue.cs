namespace Raven.CodeAnalysis.Syntax;

internal enum EncodedStringLiteralEncoding
{
    Utf8,
    Ascii
}

internal sealed record EncodedStringLiteralValue(
    string Text,
    EncodedStringLiteralEncoding Encoding,
    bool ContainsInterpolation);
