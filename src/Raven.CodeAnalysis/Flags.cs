namespace Raven.CodeAnalysis;

public static class SyntaxParserFlags
{
    public static bool PrintParseSequence { get; set; } = false;

    public static bool PrintTimestamp { get; set; } = true;
}

public static class LexerFlags
{
    public static bool PrintDebug { get; set; } = false;
}
