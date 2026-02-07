using System;

namespace Raven.CodeAnalysis;

public static class SyntaxParserFlags
{
    public static bool PrintParseSequence { get; set; } = false;

    public static bool PrintTimestamp { get; set; } = true;

    private static int _parseSequenceThrottleMilliseconds;
    public static int ParseSequenceThrottleMilliseconds
    {
        get => _parseSequenceThrottleMilliseconds;
        set => _parseSequenceThrottleMilliseconds = Math.Max(0, value);
    }
}

public static class LexerFlags
{
    public static bool PrintDebug { get; set; } = false;
}

public static class CodeGenFlags
{
    public static bool PrintDebug { get; set; } = false;

    public static bool PrintEmittedBoundNodes { get; set; } = false;
}
