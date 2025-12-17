using System;
using System.Collections.Generic;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;
using LspRange = OmniSharp.Extensions.LanguageServer.Protocol.Models.Range;

namespace Raven.LanguageServer;

internal static class PositionHelper
{
    public static int ToOffset(SourceText text, Position position)
    {
        var content = text.ToString();
        var lineStarts = TextUtils.ComputeLineStarts(content);

        var clampedLine = Math.Clamp(position.Line, 0, Math.Max(lineStarts.Count - 1, 0));
        var lineStart = clampedLine < lineStarts.Count ? lineStarts[clampedLine] : content.Length;
        var nextLineStart = clampedLine + 1 < lineStarts.Count ? lineStarts[clampedLine + 1] : content.Length;
        var available = Math.Max(0, nextLineStart - lineStart);
        var character = Math.Clamp(position.Character, 0, available);

        return Math.Min(lineStart + character, content.Length);
    }

    public static LspRange ToRange(SourceText text, TextSpan span)
    {
        var content = text.ToString();
        var lineStarts = TextUtils.ComputeLineStarts(content);
        var (startLine, startChar) = GetLinePosition(lineStarts, span.Start);
        var (endLine, endChar) = GetLinePosition(lineStarts, span.End);

        return new LspRange(new Position(startLine, startChar), new Position(endLine, endChar));
    }

    private static (int line, int character) GetLinePosition(List<int> lineStarts, int position)
    {
        var (line, column) = TextUtils.GetLineAndColumn(lineStarts, position);
        return (Math.Max(0, line - 1), Math.Max(0, column - 1));
    }
}
