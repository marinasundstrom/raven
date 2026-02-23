namespace Raven.CodeAnalysis.Syntax;

internal enum EncodedStringLiteralEncoding
{
    Utf8,
    Ascii
}

internal sealed class EncodedStringLiteralValue
{
    public EncodedStringLiteralValue(string text, EncodedStringLiteralEncoding encoding, bool containsInterpolation)
    {
        Text = text;
        Encoding = encoding;
        ContainsInterpolation = containsInterpolation;
    }

    public string Text { get; }

    public EncodedStringLiteralEncoding Encoding { get; }

    public bool ContainsInterpolation { get; }
}
