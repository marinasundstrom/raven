namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using System;
using System.Collections.Generic;
using System.IO.Pipelines;
using System.Text;

using Raven.CodeAnalysis.Text;

using static Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxFactory;

using SyntaxFacts = Raven.CodeAnalysis.Syntax.SyntaxFacts;

internal partial class ExpressionSyntaxParser : SyntaxParser
{
    private InterpolatedStringExpressionSyntax ParseInterpolatedStringExpression(SyntaxToken token)
    {
        var contents = new List<InterpolatedStringContentSyntax>();
        var raw = token.Text;
        var inner = raw.Length >= 2 ? raw.Substring(1, raw.Length - 2) : string.Empty;

        int segmentStart = 0;
        for (int i = 0; i < inner.Length;)
        {
            if (IsInterpolationStart(inner, i))
            {
                if (i > segmentStart)
                {
                    var segmentRaw = inner.Substring(segmentStart, i - segmentStart);
                    AddTextSegment(segmentRaw);
                }

                var dollarToken = new SyntaxToken(SyntaxKind.DollarToken, "$");

                if (inner[i + 1] == '{')
                {
                    var openBraceToken = new SyntaxToken(SyntaxKind.OpenBraceToken, "{");

                    i += 2; // skip ${

                    var closeIndex = ScanInterpolationBody(inner, i, out var exprText);
                    var exprSyntax = ParseExpressionFromText(exprText);

                    // If we found a closing '}', produce it; otherwise synthesize missing.
                    SyntaxToken closeBraceToken;
                    if (closeIndex < inner.Length && closeIndex >= 0 && inner[closeIndex] == '}')
                    {
                        closeBraceToken = new SyntaxToken(SyntaxKind.CloseBraceToken, "}");
                        i = closeIndex + 1; // continue after '}'
                    }
                    else
                    {
                        closeBraceToken = SyntaxToken.Missing(SyntaxKind.CloseBraceToken);
                        i = inner.Length; // stop scanning
                    }

                    contents.Add(Interpolation(dollarToken, openBraceToken, exprSyntax, closeBraceToken));
                }
                else
                {
                    var openBraceToken = SyntaxToken.Missing(SyntaxKind.OpenBraceToken);

                    i++; // skip $
                    int start = i;
                    while (i < inner.Length && SyntaxFacts.IsIdentifierPartCharacter(inner[i]))
                    {
                        i++;
                    }

                    var exprText = inner.Substring(start, i - start);
                    var exprSyntax = ParseExpressionFromText(exprText);
                    var closeBraceToken = SyntaxToken.Missing(SyntaxKind.CloseBraceToken);
                    contents.Add(Interpolation(dollarToken, openBraceToken, exprSyntax, closeBraceToken));
                }

                segmentStart = i;
            }
            else
            {
                i++;
            }
        }

        if (segmentStart < inner.Length)
        {
            var segmentRaw = inner.Substring(segmentStart);
            AddTextSegment(segmentRaw);
        }

        var startToken = new SyntaxToken(
            SyntaxKind.StringStartToken,
            "\"",
            token.LeadingTrivia,
            SyntaxTriviaList.Empty);

        var endToken = new SyntaxToken(
            SyntaxKind.StringEndToken,
            "\"",
            SyntaxTriviaList.Empty,
            token.TrailingTrivia);

        return InterpolatedStringExpression(startToken, List(contents), endToken);

        void AddTextSegment(string segmentRaw)
        {
            if (segmentRaw.Length == 0)
            {
                return;
            }

            var decoded = SyntaxFacts.DecodeStringLiteralContent(segmentRaw.AsSpan(), out _);
            var interned = string.Intern(decoded);
            contents.Add(InterpolatedStringText(new SyntaxToken(
                SyntaxKind.StringLiteralToken,
                interned,
                interned,
                segmentRaw.Length)));
        }
    }

    private static bool ContainsInterpolation(string text)
    {
        for (int i = 0; i < text.Length - 1; i++)
        {
            if (IsInterpolationStart(text, i))
            {
                return true;
            }
        }

        return false;
    }

    private static bool IsInterpolationStart(string text, int index)
    {
        if (index + 1 >= text.Length)
        {
            return false;
        }

        if (text[index] != '$' || IsEscaped(text, index))
        {
            return false;
        }

        var next = text[index + 1];
        return next == '{' || SyntaxFacts.IsIdentifierStartCharacter(next);
    }

    private static bool IsEscaped(string text, int index)
    {
        int backslashCount = 0;
        for (int i = index - 1; i >= 0 && text[i] == '\\'; i--)
        {
            backslashCount++;
        }

        return (backslashCount & 1) == 1;
    }

    // Scans the body of an interpolation that starts immediately after `${`.
    // Returns the index of the matching `}` in `text`, or -1 if none is found.
    // `expressionText` is the raw substring between `${` and the matching `}` (or to end-of-input if unterminated).
    private static int ScanInterpolationBody(ReadOnlySpan<char> text, int startIndex, out string expressionText)
    {
        int i = startIndex;
        int braceDepth = 0;   // nested { } inside the interpolation expression
        int parenDepth = 0;   // ( )
        int bracketDepth = 0; // [ ]

        while (i < text.Length)
        {
            char c = text[i];

            // Skip string/char literals so braces/dollars inside them do not affect scanning.
            if (c == '"')
            {
                i = ScanNormalStringLiteral(text, i);
                continue;
            }

            if (c == '\'')
            {
                i = ScanCharacterLiteral(text, i);
                continue;
            }

            // Skip comments.
            if (c == '/' && i + 1 < text.Length)
            {
                char n = text[i + 1];
                if (n == '/')
                {
                    i = ScanSingleLineComment(text, i);
                    continue;
                }
                if (n == '*')
                {
                    i = ScanMultiLineComment(text, i);
                    continue;
                }
            }

            // Track nesting constructs.
            switch (c)
            {
                case '{':
                    braceDepth++;
                    i++;
                    continue;

                case '}':
                    // Only treat as the end of the interpolation when we are not inside any nested construct.
                    if (braceDepth == 0 && parenDepth == 0 && bracketDepth == 0)
                    {
                        expressionText = text.Slice(startIndex, i - startIndex).ToString();
                        return i;
                    }

                    if (braceDepth > 0)
                        braceDepth--;

                    i++;
                    continue;

                case '(':
                    parenDepth++;
                    i++;
                    continue;

                case ')':
                    if (parenDepth > 0)
                        parenDepth--;
                    i++;
                    continue;

                case '[':
                    bracketDepth++;
                    i++;
                    continue;

                case ']':
                    if (bracketDepth > 0)
                        bracketDepth--;
                    i++;
                    continue;
            }

            i++;
        }

        // Unterminated interpolation; return what we have.
        expressionText = text.Slice(startIndex).ToString();
        return -1;

        static int ScanNormalStringLiteral(ReadOnlySpan<char> s, int start)
        {
            // Supports both normal strings ("...") and raw triple-quote strings ("""...""")
            // inside interpolation expressions.
            if (start + 2 < s.Length && s[start] == '"' && s[start + 1] == '"' && s[start + 2] == '"')
            {
                // Raw triple-quote: scan until next """ or EOF.
                int j = start + 3;
                while (j + 2 < s.Length)
                {
                    if (s[j] == '"' && s[j + 1] == '"' && s[j + 2] == '"')
                        return j + 3;
                    j++;
                }
                return s.Length;
            }

            // Normal string with escapes.
            int i = start + 1;
            while (i < s.Length)
            {
                char c = s[i];
                if (c == '\\')
                {
                    // Skip escaped char if present.
                    i += (i + 1 < s.Length) ? 2 : 1;
                    continue;
                }

                if (c == '"')
                    return i + 1;

                i++;
            }

            return s.Length;
        }

        static int ScanCharacterLiteral(ReadOnlySpan<char> s, int start)
        {
            // Character literal: 'a' or '\n' etc. Scan to closing quote or EOF.
            int i = start + 1;
            while (i < s.Length)
            {
                char c = s[i];
                if (c == '\\')
                {
                    i += (i + 1 < s.Length) ? 2 : 1;
                    continue;
                }

                if (c == '\'')
                    return i + 1;

                i++;
            }

            return s.Length;
        }

        static int ScanSingleLineComment(ReadOnlySpan<char> s, int start)
        {
            // // ... until newline or EOF
            int i = start + 2;
            while (i < s.Length)
            {
                char c = s[i];
                if (c == '\n' || c == '\r' || c == '\u0085' || c == '\u2028' || c == '\u2029')
                    break;
                i++;
            }
            return i;
        }

        static int ScanMultiLineComment(ReadOnlySpan<char> s, int start)
        {
            // /* ... */ or EOF
            int i = start + 2;
            while (i + 1 < s.Length)
            {
                if (s[i] == '*' && s[i + 1] == '/')
                    return i + 2;
                i++;
            }
            return s.Length;
        }
    }

    private InterpolatedStringExpressionSyntax ParseInterpolatedMultiLineStringExpression(SyntaxToken token, string inner)
    {
        var contents = new List<InterpolatedStringContentSyntax>();

        int segmentStart = 0;
        for (int i = 0; i < inner.Length;)
        {
            if (IsInterpolationStart(inner, i))
            {
                if (i > segmentStart)
                {
                    var segmentRaw = inner.Substring(segmentStart, i - segmentStart);
                    AddTextSegment(segmentRaw);
                }

                var dollarToken = new SyntaxToken(SyntaxKind.DollarToken, "$");

                if (inner[i + 1] == '{')
                {
                    var openBraceToken = new SyntaxToken(SyntaxKind.OpenBraceToken, "{");

                    i += 2; // skip ${

                    var closeIndex = ScanInterpolationBody(inner, i, out var exprText);
                    var exprSyntax = ParseExpressionFromText(exprText);

                    // If we found a closing '}', produce it; otherwise synthesize missing.
                    SyntaxToken closeBraceToken;
                    if (closeIndex < inner.Length && closeIndex >= 0 && inner[closeIndex] == '}')
                    {
                        closeBraceToken = new SyntaxToken(SyntaxKind.CloseBraceToken, "}");
                        i = closeIndex + 1; // continue after '}'
                    }
                    else
                    {
                        closeBraceToken = SyntaxToken.Missing(SyntaxKind.CloseBraceToken);
                        i = inner.Length; // stop scanning
                    }

                    contents.Add(Interpolation(dollarToken, openBraceToken, exprSyntax, closeBraceToken));
                }
                else
                {
                    var openBraceToken = SyntaxToken.Missing(SyntaxKind.OpenBraceToken);

                    i++; // skip $
                    int start = i;
                    while (i < inner.Length && SyntaxFacts.IsIdentifierPartCharacter(inner[i]))
                    {
                        i++;
                    }

                    var exprText = inner.Substring(start, i - start);
                    var exprSyntax = ParseExpressionFromText(exprText);
                    var closeBraceToken = SyntaxToken.Missing(SyntaxKind.CloseBraceToken);
                    contents.Add(Interpolation(dollarToken, openBraceToken, exprSyntax, closeBraceToken));
                }

                segmentStart = i;
            }
            else
            {
                i++;
            }
        }

        if (segmentStart < inner.Length)
        {
            var segmentRaw = inner.Substring(segmentStart);
            AddTextSegment(segmentRaw);
        }

        var startToken = new SyntaxToken(
            SyntaxKind.StringStartToken,
            "\"\"\"",
            token.LeadingTrivia,
            SyntaxTriviaList.Empty);

        var endToken = new SyntaxToken(
            SyntaxKind.StringEndToken,
            "\"\"\"",
            SyntaxTriviaList.Empty,
            token.TrailingTrivia);

        return InterpolatedStringExpression(startToken, List(contents), endToken);

        void AddTextSegment(string segmentRaw)
        {
            if (segmentRaw.Length == 0)
            {
                return;
            }

            // Multiline strings are raw: do not decode escape sequences.
            var interned = string.Intern(segmentRaw);
            contents.Add(InterpolatedStringText(new SyntaxToken(
                SyntaxKind.StringLiteralToken,
                interned,
                interned,
                segmentRaw.Length)));
        }
    }
}
