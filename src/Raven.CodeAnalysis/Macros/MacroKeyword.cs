using System;

namespace Raven.CodeAnalysis.Macros;

public sealed class MacroKeyword
{
    public MacroKeyword(
        string text,
        int rawKind,
        MacroKeywordClassification classification = MacroKeywordClassification.Keyword)
    {
        if (string.IsNullOrWhiteSpace(text))
            throw new ArgumentException("A macro keyword must have non-whitespace text.", nameof(text));

        Text = text;
        RawKind = rawKind;
        Classification = classification;
    }

    public string Text { get; }

    public int RawKind { get; }

    public MacroKeywordClassification Classification { get; }
}

public enum MacroKeywordClassification
{
    Keyword = 0,
    ReservedWord = 1,
}
