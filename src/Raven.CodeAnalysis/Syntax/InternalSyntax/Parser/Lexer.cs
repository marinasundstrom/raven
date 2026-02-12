using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.Text;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

internal class Lexer : ILexer
{
    private readonly SeekableTextSource _textSource;
    private readonly StringBuilder _stringBuilder = new StringBuilder();
    private readonly List<Token> _lookaheadTokens = new List<Token>();
    private int _currentPosition = 0;
    private int _tokenStartPosition = 0;
    public Action<DiagnosticInfo>? DiagnosticSink { get; set; }

    public Lexer(TextReader textReader, int startPosition = 0)
    {
        _textSource = new SeekableTextSource(textReader, startPosition);
        _currentPosition = startPosition;
        _tokenStartPosition = startPosition;
    }

    public void ResetToPosition(int position)
    {
        _lookaheadTokens.Clear();
        _currentPosition = position;
        _tokenStartPosition = position;
        _textSource.ResetPosition(position);

        PrintDebug($"Lexer reset ot position {position}");
    }

    [Conditional("DEBUG")]
    private static void PrintDebug(string text)
    {
        if (LexerFlags.PrintDebug)
        {
            Console.WriteLine(text);
        }
    }

    private void ReportDiagnostic(DiagnosticInfo diagnostic)
    {
        DiagnosticSink?.Invoke(diagnostic);
    }

    public void CreateCheckpoint()
    {
        var position = _textSource.PushPosition();

        PrintDebug($"Lexer pushed checkpoint for position {position}");
    }

    public void RewindToCheckpoint()
    {
        var position = _textSource.PopAndRestorePosition();
        _lookaheadTokens.Clear();
        _currentPosition = position;
        _tokenStartPosition = position;

        PrintDebug($"Lexer rewind to position {position}");
    }

    public void ClearCheckpoint()
    {
        var position = _textSource.PopPosition();

        PrintDebug($"Lexer cleared checkpoint at position {position}");
    }

    /// <summary>
    /// NewLineToken for CR and LF
    /// </summary>
    /// <value></value>
    public bool UseUnifiedNewLineToken { get; set; } = true;

    /// <summary>
    /// Treat the sequence CR and LF as one symbol.
    /// </summary>
    /// <value></value>
    public bool MergeCarriageReturnAndLineFeed { get; set; } = true;

    public SyntaxKind LineFeedTokenKind => UseUnifiedNewLineToken ? SyntaxKind.NewLineToken : SyntaxKind.LineFeedToken;
    public SyntaxKind CarriageReturnLineFeedTokenKind => UseUnifiedNewLineToken ? SyntaxKind.NewLineToken : SyntaxKind.CarriageReturnLineFeedToken;

    public Token ReadToken()
    {
        Token token;

        if (_lookaheadTokens.Count > 0)
        {
            // Remove the token from the lookahead list
            token = _lookaheadTokens[0]; // Using index from end for clarity
            _lookaheadTokens.RemoveAt(0);
        }
        else
        {
            // Fallback to reading a new token
            token = ReadTokenCore();
        }

        PrintDebug($"Lexer read token: {token.Kind} {token.Text}{(token.Value is not null ? $" ({token.Value})" : "")}, at position {_tokenStartPosition} (Width: {token.Length})");

        return token;
    }

    public void ReadAndDiscardTokens(int count)
    {
        for (int i = 0; i < count; i++)
        {
            ReadToken();
        }
    }

    public IEnumerable<Token> ReadTokens(int count)
    {
        var tokens = new Token[count];
        for (int i = 0; i < count; i++)
        {
            tokens[i] = ReadToken();
        }
        return tokens;
    }

    public Token PeekToken(int index = 0)
    {
        // Ensure the lookahead tokens list is populated up to the requested index
        while (_lookaheadTokens.Count <= index)
        {
            var token = ReadTokenCore();
            _lookaheadTokens.Add(token);
        }
        return _lookaheadTokens[index];
    }

    private Token ReadTokenCore()
    {
        _tokenStartPosition = _currentPosition;

        char ch = '\0', ch2 = '\0';

        while (ReadChar(out ch))
        {
            if (_stringBuilder.Length > 0) _stringBuilder.Clear();

            if (ch == '@')
            {
                _stringBuilder.Append(ch);

                if (!PeekChar(out ch2) || !SyntaxFacts.IsIdentifierStartCharacter(ch2))
                {
                    return new Token(SyntaxKind.IdentifierToken, GetStringBuilderValue());
                }

                ReadChar(out ch2);
                _stringBuilder.Append(ch2);

                while (PeekChar(out ch2) && SyntaxFacts.IsIdentifierPartCharacter(ch2))
                {
                    ReadChar();
                    _stringBuilder.Append(ch2);
                }

                var text2 = GetStringBuilderValue();
                var valueText = string.Intern(text2.Substring(1));
                return new Token(SyntaxKind.IdentifierToken, text2, valueText);
            }

            if (SyntaxFacts.IsIdentifierStartCharacter(ch) ||
                char.IsDigit(ch) || (ch == '.' && PeekChar(out ch2) && char.IsDigit(ch2)))
            {

                _stringBuilder.Append(ch);

                if (SyntaxFacts.IsIdentifierStartCharacter(ch))
                {
                    SyntaxKind syntaxKind = SyntaxKind.IdentifierToken;

                    while (PeekChar(out ch) && SyntaxFacts.IsIdentifierPartCharacter(ch))
                    {
                        ReadChar();
                        _stringBuilder.Append(ch);
                    }

                    if (!SyntaxFacts.TryParseKeyword(GetStringBuilderValue(), out syntaxKind))
                    {
                        syntaxKind = SyntaxKind.IdentifierToken;
                    }

                    if (GetStringBuilderValue() == "_")
                    {
                        syntaxKind = SyntaxKind.UnderscoreToken;
                    }

                    switch (syntaxKind)
                    {
                        case SyntaxKind.TrueKeyword:
                            return new Token(SyntaxKind.TrueKeyword, GetStringBuilderValue(), true, _stringBuilder.Length);

                        case SyntaxKind.FalseKeyword:
                            return new Token(SyntaxKind.FalseKeyword, GetStringBuilderValue(), false, _stringBuilder.Length);
                    }

                    return new Token(syntaxKind, GetStringBuilderValue());
                }
                else if (char.IsDigit(ch) || ch == '.')
                    return ParseNumber(ref ch);
            }
            else
            {
                string chStr = string.Intern(ch.ToString());

                switch (ch)
                {
                    case '+':
                        if (PeekChar(out ch2) && ch2 == '=')
                        {
                            ReadChar();
                            return new Token(SyntaxKind.PlusEqualsToken, "+=");
                        }
                        else if (PeekChar(out ch2) && ch2 == '+')
                        {
                            ReadChar();
                            return new Token(SyntaxKind.PlusPlusToken, "++");
                        }
                        return new Token(SyntaxKind.PlusToken, chStr);

                    case '-':
                        if (PeekChar(out ch2) && ch2 == '>')
                        {
                            ReadChar();
                            return new Token(SyntaxKind.ArrowToken, "->");
                        }
                        else if (PeekChar(out ch2) && (char.IsDigit(ch2) || ch2 == '.'))
                        {
                            return ParseNumber(ref ch, true);
                        }
                        else if (PeekChar(out ch2) && ch2 == '=')
                        {
                            ReadChar();
                            return new Token(SyntaxKind.MinusEqualsToken, "-=");
                        }
                        else if (PeekChar(out ch2) && ch2 == '-')
                        {
                            ReadChar();
                            return new Token(SyntaxKind.MinusMinusToken, "--");
                        }
                        return new Token(SyntaxKind.MinusToken, chStr);

                    case '/':
                        // Comments are lexed as a single token (raw text) to avoid tokenizing inside them.
                        // This prevents characters like '\'' and '"' from affecting trivia parsing.
                        if (PeekChar(out ch2))
                        {
                            // Compound operator
                            if (ch2 == '=')
                            {
                                ReadChar();
                                return new Token(SyntaxKind.SlashEqualsToken, "/=");
                            }

                            // Single-line comment: //...
                            if (ch2 == '/')
                            {
                                _stringBuilder.Append('/');
                                ReadChar(); // consume second '/'
                                _stringBuilder.Append('/');

                                // If the comment begins with "///" classify it as a documentation comment.
                                // IMPORTANT: this must be decided before scanning the rest of the line,
                                // otherwise PeekChar will be sitting on the newline.
                                var isDocComment = PeekChar(out var nextAfterSlashes) && nextAfterSlashes == '/';

                                while (PeekChar(out var c))
                                {
                                    if (IsEndOfLine(c))
                                        break; // do NOT consume newline

                                    ReadChar();
                                    _stringBuilder.Append(c);
                                }

                                var kind = isDocComment ? SyntaxKind.DocumentationCommentTrivia : SyntaxKind.SingleLineCommentTrivia;
                                return new Token(kind, GetStringBuilderValue());
                            }

                            // Multi-line comment: /* ... */
                            if (ch2 == '*')
                            {
                                _stringBuilder.Append('/');
                                ReadChar(); // consume '*'
                                _stringBuilder.Append('*');

                                while (PeekChar(out var c))
                                {
                                    ReadChar();
                                    _stringBuilder.Append(c);

                                    if (c == '*' && PeekChar(out var next) && next == '/')
                                    {
                                        ReadChar();
                                        _stringBuilder.Append('/');
                                        return new Token(SyntaxKind.MultiLineCommentTrivia, GetStringBuilderValue());
                                    }
                                }

                                // Unterminated comment; return what we have. (Keep newline/EOF behavior consistent.)
                                // If you have a dedicated diagnostic for this, you can add it here.
                                return new Token(SyntaxKind.MultiLineCommentTrivia, GetStringBuilderValue());
                            }
                        }

                        return new Token(SyntaxKind.SlashToken, chStr);

                    case '*':
                        if (PeekChar(out ch2) && ch2 == '=')
                        {
                            ReadChar();
                            return new Token(SyntaxKind.StarEqualsToken, "*=");
                        }
                        return new Token(SyntaxKind.StarToken, chStr);

                    case '%':
                        return new Token(SyntaxKind.PercentToken, chStr);

                    case '.':
                        if (PeekChar(out ch2) && ch2 == '.')
                        {
                            ReadChar();
                            return new Token(SyntaxKind.DotDotToken, "..");
                        }
                        return new Token(SyntaxKind.DotToken, chStr);

                    case ',':
                        return new Token(SyntaxKind.CommaToken, chStr);

                    case ':':
                        return new Token(SyntaxKind.ColonToken, chStr);

                    case ';':
                        return new Token(SyntaxKind.SemicolonToken, chStr);

                    case '(':
                        return new Token(SyntaxKind.OpenParenToken, chStr);

                    case ')':
                        return new Token(SyntaxKind.CloseParenToken, chStr);

                    case '{':
                        return new Token(SyntaxKind.OpenBraceToken, chStr);

                    case '}':
                        return new Token(SyntaxKind.CloseBraceToken, chStr);

                    case '[':
                        return new Token(SyntaxKind.OpenBracketToken, chStr);

                    case ']':
                        return new Token(SyntaxKind.CloseBracketToken, chStr);

                    case '=':
                        if (PeekChar(out ch2) && ch2 == '=')
                        {
                            ReadChar();
                            return new Token(SyntaxKind.EqualsEqualsToken, "==");
                        }
                        else if (PeekChar(out ch2) && ch2 == '>')
                        {
                            ReadChar();
                            return new Token(SyntaxKind.FatArrowToken, "=>");
                        }
                        return new Token(SyntaxKind.EqualsToken, chStr);

                    case '|':
                        if (PeekChar(out ch2) && ch2 == '|')
                        {
                            ReadChar();
                            return new Token(SyntaxKind.BarBarToken, "||");
                        }
                        else if (PeekChar(out ch2) && ch2 == '=')
                        {
                            ReadChar();
                            return new Token(SyntaxKind.BarEqualsToken, "|=");
                        }
                        else if (PeekChar(out ch2) && ch2 == '>')
                        {
                            ReadChar();
                            return new Token(SyntaxKind.PipeToken, "|>");
                        }
                        return new Token(SyntaxKind.BarToken, chStr);

                    case '!':
                        if (PeekChar(out ch2) && ch2 == '=')
                        {
                            ReadChar();
                            return new Token(SyntaxKind.NotEqualsToken, "!=");
                        }
                        return new Token(SyntaxKind.ExclamationToken, chStr);

                    case '?':
                        if (PeekChar(out ch2) && ch2 == '?')
                        {
                            ReadChar();
                            if (PeekChar(out var ch3) && ch3 == '=')
                            {
                                ReadChar();
                                return new Token(SyntaxKind.QuestionQuestionEqualsToken, "??=");
                            }
                            return new Token(SyntaxKind.QuestionQuestionToken, "??");
                        }
                        return new Token(SyntaxKind.QuestionToken, chStr);

                    case '&':
                        if (PeekChar(out ch2) && ch2 == '&')
                        {
                            ReadChar();
                            return new Token(SyntaxKind.AmpersandAmpersandToken, "&&");
                        }
                        else if (PeekChar(out ch2) && ch2 == '=')
                        {
                            ReadChar();
                            return new Token(SyntaxKind.AmpersandEqualsToken, "&=");
                        }
                        return new Token(SyntaxKind.AmpersandToken, chStr);

                    case '<':
                        if (PeekChar(out ch2) && ch2 == '=')
                        {
                            ReadChar();
                            return new Token(SyntaxKind.LessThanOrEqualsToken, "<=");
                        }
                        else if (PeekChar(out ch2) && ch2 == '<')
                        {
                            ReadChar();
                            return new Token(SyntaxKind.LessThanLessThanToken, "<<");
                        }
                        return new Token(SyntaxKind.LessThanToken, chStr);

                    case '>':
                        if (PeekChar(out ch2) && ch2 == '=')
                        {
                            ReadChar();
                            return new Token(SyntaxKind.GreaterThanOrEqualsToken, ">=");
                        }
                        else if (PeekChar(out ch2) && ch2 == '>')
                        {
                            ReadChar();
                            return new Token(SyntaxKind.GreaterThanGreaterThanToken, ">>");
                        }
                        return new Token(SyntaxKind.GreaterThanToken, chStr);

                    case '^':
                        if (PeekChar(out ch2) && ch2 == '=')
                        {
                            ReadChar();
                            return new Token(SyntaxKind.CaretEqualsToken, "^=");
                        }
                        return new Token(SyntaxKind.CaretToken, chStr);


                    case ' ':
                        int length = 1;

                        while (ReadWhile(' '))
                        {
                            length++;
                        }

                        return new Token(SyntaxKind.Whitespace, string.Intern(new string(' ', length)));


                    case '\'':
                        {
                            _stringBuilder.Append(ch); // opening quote

                            if (!ReadChar(out ch))
                            {
                                ReportDiagnostic(DiagnosticInfo.Create(
                                    CompilerDiagnostics.UnterminatedCharacterLiteral,
                                    GetTokenStartPositionSpan()));
                                return new Token(SyntaxKind.CharacterLiteralToken, GetStringBuilderValue());
                            }

                            char character;

                            if (ch == '\\')
                            {
                                _stringBuilder.Append(ch); // backslash

                                if (!ReadChar(out ch))
                                {
                                    ReportDiagnostic(DiagnosticInfo.Create(
                                        CompilerDiagnostics.UnterminatedCharacterLiteral,
                                        GetTokenStartPositionSpan()));
                                    return new Token(SyntaxKind.CharacterLiteralToken, GetStringBuilderValue());
                                }

                                _stringBuilder.Append(ch); // escaped char

                                switch (ch)
                                {
                                    case '\'':
                                        character = '\'';
                                        break;
                                    case '\"':
                                        character = '\"';
                                        break;
                                    case '\\':
                                        character = '\\';
                                        break;
                                    case '0':
                                        character = '\0';
                                        break;
                                    case 'a':
                                        character = '\a';
                                        break;
                                    case 'b':
                                        character = '\b';
                                        break;
                                    case 'f':
                                        character = '\f';
                                        break;
                                    case 'n':
                                        character = '\n';
                                        break;
                                    case 'r':
                                        character = '\r';
                                        break;
                                    case 't':
                                        character = '\t';
                                        break;
                                    case 'v':
                                        character = '\v';
                                        break;
                                    default:
                                        ReportDiagnostic(DiagnosticInfo.Create(
                                            CompilerDiagnostics.InvalidEscapeSequence,
                                            GetTokenStartPositionSpan()));
                                        character = '?';
                                        break;
                                }
                            }
                            else
                            {
                                _stringBuilder.Append(ch);
                                character = ch;
                            }

                            if (!ReadChar(out ch) || ch != '\'')
                            {
                                ReportDiagnostic(DiagnosticInfo.Create(
                                    CompilerDiagnostics.UnterminatedCharacterLiteral,
                                    GetTokenStartPositionSpan()));
                                return new Token(SyntaxKind.CharacterLiteralToken, GetStringBuilderValue());
                            }

                            _stringBuilder.Append(ch); // closing quote

                            return new Token(
                                SyntaxKind.CharacterLiteralToken,
                                GetStringBuilderValue(),
                                character,
                                _stringBuilder.Length);
                        }

                    case '"':
                        _stringBuilder.Append(ch); // opening quote

                        // Triple-quote raw multi-line string: """ ... """
                        // Closing delimiter must be the first non-whitespace on its line.
                        // Value is indentation-trimmed based on the closing delimiter line.
                        if (TryConsumeTripleQuoteStart())
                        {
                            return ParseTripleQuotedStringLiteral();
                        }

                        // Normal (single-line) string literal
                        return ParseSingleLineStringLiteral();

                    /*

                    case '`':
                        return new Token(SyntaxKind.BackquoteToken, chStr);

                    */

                    case '\t':
                        return new Token(SyntaxKind.TabToken, string.Intern("\t"));

                    case '\n':
                        return new Token(LineFeedTokenKind, string.Intern("\n"));

                    case '\u0085':
                        return new Token(LineFeedTokenKind, string.Intern("\u0085"));

                    case '\u2028':
                        return new Token(LineFeedTokenKind, string.Intern("\u2028"));

                    case '\u2029':
                        return new Token(LineFeedTokenKind, string.Intern("\u2029"));

                    case '\r':
                        if (MergeCarriageReturnAndLineFeed && PeekChar(out ch2) && ch2 == '\n')
                        {
                            ReadChar();
                            return new Token(CarriageReturnLineFeedTokenKind, string.Intern("\r\n"));
                        }
                        return new Token(SyntaxKind.CarriageReturnToken, string.Intern("\r"));

                    case '\0':
                        return new Token(SyntaxKind.EndOfFileToken, string.Empty);
                }
            }

            // Unknown token
            /*ReportDiagnostic(DiagnosticInfo.Create(
                     CompilerDiagnostics.UnknownCharacter,
                     new TextSpan(_tokenStartPosition, 1)
                 ));*/

            return new Token(SyntaxKind.None, ch.ToString());
        }

        return new Token(SyntaxKind.EndOfFileToken, string.Empty);
    }

    private bool TryConsumeTripleQuoteStart()
    {
        // Called after consuming/appending the first '"'.
        // Look ahead for two more quotes without committing speculative reads.
        if (!PeekChar(out var next) || next != '"')
            return false;

        var checkpoint = _currentPosition;
        _textSource.PushPosition();

        if (!ReadChar(out var q2) || q2 != '"')
        {
            _textSource.PopAndRestorePosition();
            _currentPosition = checkpoint;
            return false;
        }

        if (!ReadChar(out var q3) || q3 != '"')
        {
            _textSource.PopAndRestorePosition();
            _currentPosition = checkpoint;
            return false;
        }

        // Restore, then consume for real.
        _textSource.PopAndRestorePosition();
        _currentPosition = checkpoint;

        ReadChar();
        _stringBuilder.Append('"');
        ReadChar();
        _stringBuilder.Append('"');

        return true;
    }

    private Token ParseSingleLineStringLiteral()
    {
        // We enter after consuming/appending the opening quote.
        // Interpolation-aware lexing: quotes inside ${...} or $ident do not terminate the string.
        while (true)
        {
            if (!PeekChar(out var ch2))
            {
                ReportDiagnostic(
                    DiagnosticInfo.Create(
                        CompilerDiagnostics.NewlineInConstant,
                        GetTokenStartPositionSpan()));
                break;
            }

            ReadChar();

            // Interpolation: $ (not escaped), followed by { or identifier start
            if (ch2 == '$' && !IsDollarEscaped(_stringBuilder) && PeekChar(out var next) && (next == '{' || SyntaxFacts.IsIdentifierStartCharacter(next)))
            {
                _stringBuilder.Append('$');
                if (next == '{')
                {
                    ReadChar(); // consume '{'
                    _stringBuilder.Append('{');
                    ScanInterpolationBodyIntoBuilder();
                }
                else
                {
                    // $identifier form
                    ReadChar(); // consume first identifier char
                    _stringBuilder.Append(next);
                    while (PeekChar(out var idch) && SyntaxFacts.IsIdentifierPartCharacter(idch))
                    {
                        ReadChar();
                        _stringBuilder.Append(idch);
                    }
                }
                continue;
            }

            if (ch2 == '"')
            {
                _stringBuilder.Append(ch2);
                break;
            }

            if (IsEndOfLine(ch2))
            {
                ReportDiagnostic(
                    DiagnosticInfo.Create(
                        CompilerDiagnostics.NewlineInConstant,
                        GetTokenStartPositionSpan()));
                break;
            }

            if (ch2 == '\\')
            {
                // Normal escape handling
                _stringBuilder.Append(ch2);

                if (!PeekChar(out var escaped2))
                {
                    ReportDiagnostic(
                        DiagnosticInfo.Create(
                            CompilerDiagnostics.NewlineInConstant,
                            GetTokenStartPositionSpan()));
                    break;
                }

                ReadChar();

                if (IsEndOfLine(escaped2))
                {
                    ReportDiagnostic(
                        DiagnosticInfo.Create(
                            CompilerDiagnostics.NewlineInConstant,
                            GetTokenStartPositionSpan()));
                    break;
                }

                _stringBuilder.Append(escaped2);
                continue;
            }

            _stringBuilder.Append(ch2);
        }

        var rawText = GetStringBuilderValue();
        ReadOnlySpan<char> innerText = rawText.Length >= 2
            ? rawText.AsSpan(1, rawText.Length - 2)
            : ReadOnlySpan<char>.Empty;

        var decoded = SyntaxFacts.DecodeStringLiteralContent(innerText, out var hadInvalidEscape);

        if (hadInvalidEscape)
        {
            ReportDiagnostic(
                DiagnosticInfo.Create(
                    CompilerDiagnostics.InvalidEscapeSequence,
                    GetTokenStartPositionSpan()));
        }

        // Value is the decoded inner content (no quotes).
        return new Token(
            SyntaxKind.StringLiteralToken,
            rawText,
            string.Intern(decoded));

        // --- Local helpers for interpolation-aware lexing ---

        // Returns true if the $ is immediately preceded by an odd number of backslashes in the builder.
        static bool IsDollarEscaped(StringBuilder sb)
        {
            int count = 0;
            for (int i = sb.Length - 1; i >= 0 && sb[i] == '\\'; i--)
                count++;
            return (count & 1) == 1;
        }

        // Scan the body of an interpolation (after `${` has been appended, and the `{` consumed).
        void ScanInterpolationBodyIntoBuilder()
        {
            int braceDepth = 1;
            int parenDepth = 0;
            int bracketDepth = 0;
            while (true)
            {
                if (!PeekChar(out var c))
                {
                    // Unterminated interpolation: just stop.
                    break;
                }
                // Do not allow newlines inside interpolation in single-line strings.
                if (IsEndOfLine(c))
                {
                    // Emit newline-in-constant diagnostic and stop.
                    ReportDiagnostic(
                        DiagnosticInfo.Create(
                            CompilerDiagnostics.NewlineInConstant,
                            GetTokenStartPositionSpan()));
                    break;
                }
                // Handle string literals inside interpolation
                if (c == '"')
                {
                    ReadChar();
                    _stringBuilder.Append('"');
                    // Check for raw triple-quote
                    if (PeekChar(out var q2) && q2 == '"' && _textSource.PeekChar(1, out var q3) && q3 == '"')
                    {
                        ReadChar(); _stringBuilder.Append('"');
                        ReadChar(); _stringBuilder.Append('"');
                        ScanRawTripleQuotedStringLiteralIntoBuilder();
                    }
                    else
                    {
                        ScanNormalStringLiteralIntoBuilder();
                    }
                    continue;
                }
                // Handle character literals inside interpolation
                if (c == '\'')
                {
                    ReadChar();
                    _stringBuilder.Append('\'');
                    ScanCharacterLiteralIntoBuilder();
                    continue;
                }
                // Handle comments
                if (c == '/' && PeekChar(out var next))
                {
                    if (_textSource.PeekChar(1, out var n2))
                    {
                        if (next == '/')
                        {
                            ReadChar(); _stringBuilder.Append('/');
                            ReadChar(); _stringBuilder.Append('/');
                            ScanSingleLineCommentIntoBuilder();
                            continue;
                        }
                        else if (next == '*')
                        {
                            ReadChar(); _stringBuilder.Append('/');
                            ReadChar(); _stringBuilder.Append('*');
                            ScanMultiLineCommentIntoBuilder();
                            continue;
                        }
                    }
                }
                // Handle nesting
                if (c == '{')
                {
                    ReadChar();
                    _stringBuilder.Append('{');
                    braceDepth++;
                    continue;
                }
                if (c == '}')
                {
                    if (braceDepth == 1 && parenDepth == 0 && bracketDepth == 0)
                    {
                        ReadChar();
                        _stringBuilder.Append('}');
                        // End of interpolation
                        break;
                    }
                    else
                    {
                        ReadChar();
                        _stringBuilder.Append('}');
                        if (braceDepth > 1) braceDepth--;
                        continue;
                    }
                }
                if (c == '(')
                {
                    ReadChar();
                    _stringBuilder.Append('(');
                    parenDepth++;
                    continue;
                }
                if (c == ')')
                {
                    ReadChar();
                    _stringBuilder.Append(')');
                    if (parenDepth > 0) parenDepth--;
                    continue;
                }
                if (c == '[')
                {
                    ReadChar();
                    _stringBuilder.Append('[');
                    bracketDepth++;
                    continue;
                }
                if (c == ']')
                {
                    ReadChar();
                    _stringBuilder.Append(']');
                    if (bracketDepth > 0) bracketDepth--;
                    continue;
                }
                // Otherwise, consume and append
                ReadChar();
                _stringBuilder.Append(c);
            }
        }

        // Scan a normal string literal (entered after opening " is already appended)
        void ScanNormalStringLiteralIntoBuilder()
        {
            while (PeekChar(out var c))
            {
                ReadChar();
                _stringBuilder.Append(c);
                if (c == '\\')
                {
                    if (PeekChar(out var next))
                    {
                        ReadChar();
                        _stringBuilder.Append(next);
                        continue;
                    }
                    else
                    {
                        break;
                    }
                }
                if (c == '"')
                {
                    break;
                }
                // Do not allow newlines in string literal
                if (IsEndOfLine(c))
                {
                    break;
                }
            }
        }

        // Scan a raw triple-quoted string literal (entered after opening """ is already appended)
        void ScanRawTripleQuotedStringLiteralIntoBuilder()
        {
            while (true)
            {
                if (!PeekChar(out var c))
                {
                    break;
                }
                // End at next """
                if (c == '"' && _textSource.PeekChar(1, out var q2) && q2 == '"' && _textSource.PeekChar(2, out var q3) && q3 == '"')
                {
                    ReadChar(); _stringBuilder.Append('"');
                    ReadChar(); _stringBuilder.Append('"');
                    ReadChar(); _stringBuilder.Append('"');
                    break;
                }
                // Newline or EOF ends triple-quoted string inside interpolation
                if (IsEndOfLine(c))
                {
                    break;
                }
                ReadChar();
                _stringBuilder.Append(c);
            }
        }

        // Scan a character literal (entered after opening ' is already appended)
        void ScanCharacterLiteralIntoBuilder()
        {
            while (PeekChar(out var c))
            {
                ReadChar();
                _stringBuilder.Append(c);
                if (c == '\\')
                {
                    if (PeekChar(out var next))
                    {
                        ReadChar();
                        _stringBuilder.Append(next);
                        continue;
                    }
                    else
                    {
                        break;
                    }
                }
                if (c == '\'')
                {
                    break;
                }
                if (IsEndOfLine(c))
                {
                    break;
                }
            }
        }

        // Scan a single-line comment (entered after // is already appended)
        void ScanSingleLineCommentIntoBuilder()
        {
            while (PeekChar(out var c))
            {
                if (IsEndOfLine(c))
                    break;
                ReadChar();
                _stringBuilder.Append(c);
            }
        }

        // Scan a multi-line comment (entered after /* is already appended)
        void ScanMultiLineCommentIntoBuilder()
        {
            while (PeekChar(out var c))
            {
                ReadChar();
                _stringBuilder.Append(c);
                if (c == '*' && PeekChar(out var next) && next == '/')
                {
                    ReadChar();
                    _stringBuilder.Append('/');
                    break;
                }
            }
        }
    }

    private Token ParseTripleQuotedStringLiteral()
    {
        // Entered after consuming/appending opening: """
        // Raw content (no escape decoding). Newlines are allowed.
        // Closing delimiter must be first non-whitespace on its line.

        var content = new StringBuilder();

        // Track indentation at the start of the current line.
        var lineIndent = new StringBuilder();
        bool lineHasNonWhitespace = false;
        bool firstChar = true;
        bool indentBuffered = false; // true when we have buffered whitespace that has not been flushed to content

        while (true)
        {
            if (!PeekChar(out var c))
            {
                // Unterminated raw string. Return what we have.
                break;
            }

            // Closing delimiter: """ at current position, only if first non-whitespace on line.
            if (c == '"' && !lineHasNonWhitespace && TryPeekTripleQuote())
            {
                var trimIndent = lineIndent.ToString();

                // Consume closing delimiter into raw text.
                ReadChar(); _stringBuilder.Append('"');
                ReadChar(); _stringBuilder.Append('"');
                ReadChar(); _stringBuilder.Append('"');

                var rawText = GetStringBuilderValue();

                var value = TrimTripleQuotedValue(content.ToString(), trimIndent);
                return new Token(
                    SyntaxKind.MultiLineStringLiteralToken,
                    rawText,
                    string.Intern(value));
            }

            // Consume character.
            ReadChar();
            _stringBuilder.Append(c);

            // Drop the very first newline after the opening delimiter (C# raw string style).
            if (firstChar)
            {
                firstChar = false;
                if (c == '\r')
                {
                    // If CRLF, also drop the LF.
                    if (PeekChar(out var lf) && lf == '\n')
                    {
                        ReadChar();
                        _stringBuilder.Append(lf);
                    }

                    // Reset line state.
                    lineIndent.Clear();
                    lineHasNonWhitespace = false;
                    indentBuffered = false;
                    continue;
                }

                if (c == '\n' || c == '\u0085' || c == '\u2028' || c == '\u2029')
                {
                    lineIndent.Clear();
                    lineHasNonWhitespace = false;
                    indentBuffered = false;
                    continue;
                }

                // If first char isn't a newline, keep it.
            }

            // Track line state + accumulate content.
            if (c == '\r')
            {
                content.Append(c);
                lineIndent.Clear();
                lineHasNonWhitespace = false;
                indentBuffered = false;
                continue;
            }

            if (c == '\n' || c == '\u0085' || c == '\u2028' || c == '\u2029')
            {
                content.Append(c);
                lineIndent.Clear();
                lineHasNonWhitespace = false;
                indentBuffered = false;
                continue;
            }

            if (!lineHasNonWhitespace && (c == ' ' || c == '\t'))
            {
                // Buffer indentation but do not commit it to content until we see a non-whitespace character.
                lineIndent.Append(c);
                indentBuffered = true;
                continue;
            }

            if (!lineHasNonWhitespace)
            {
                lineHasNonWhitespace = true;

                // Commit buffered indentation to content now that we know this line has real content.
                if (indentBuffered && lineIndent.Length > 0)
                {
                    content.Append(lineIndent);
                }

                indentBuffered = false;
            }

            content.Append(c);
        }

        // Unterminated: return raw token + raw value.
        var unterminatedRaw = GetStringBuilderValue();
        return new Token(
            SyntaxKind.MultiLineStringLiteralToken,
            unterminatedRaw,
            string.Intern(content.ToString()));

        bool TryPeekTripleQuote()
        {
            var checkpoint = _currentPosition;
            _textSource.PushPosition();

            if (!ReadChar(out var q1) || q1 != '"')
            {
                _textSource.PopAndRestorePosition();
                _currentPosition = checkpoint;
                return false;
            }

            if (!ReadChar(out var q2) || q2 != '"')
            {
                _textSource.PopAndRestorePosition();
                _currentPosition = checkpoint;
                return false;
            }

            if (!ReadChar(out var q3) || q3 != '"')
            {
                _textSource.PopAndRestorePosition();
                _currentPosition = checkpoint;
                return false;
            }

            _textSource.PopAndRestorePosition();
            _currentPosition = checkpoint;
            return true;
        }

        static string TrimTripleQuotedValue(string value, string trimIndent)
        {
            if (trimIndent.Length == 0)
                return value;

            var sb = new StringBuilder(value.Length);

            int i = 0;
            while (i < value.Length)
            {
                int lineStart = i;

                while (i < value.Length && value[i] != '\n' && value[i] != '\r' && value[i] != '\u0085' && value[i] != '\u2028' && value[i] != '\u2029')
                    i++;

                var line = value.Substring(lineStart, i - lineStart);

                if (line.StartsWith(trimIndent, StringComparison.Ordinal))
                    sb.Append(line.Substring(trimIndent.Length));
                else
                    sb.Append(line);

                if (i < value.Length)
                {
                    if (value[i] == '\r' && i + 1 < value.Length && value[i + 1] == '\n')
                    {
                        sb.Append("\r\n");
                        i += 2;
                    }
                    else
                    {
                        sb.Append(value[i]);
                        i++;
                    }
                }
            }

            return sb.ToString();
        }
    }

    private Token ParseNumber(ref char ch, bool negative = false)
    {
        bool isFloat = false;
        bool isDouble = false;
        bool isDecimal = false;
        bool isLong = false;
        bool isByte = false;
        bool hasDecimal = ch == '.';
        bool hasExponent = false;

        if (negative)
        {
            _stringBuilder.Append('-');

            if (ReadChar(out ch))
            {
                _stringBuilder.Append(ch);
                hasDecimal |= ch == '.';
            }
            else
            {
                ch = '\0';
            }
        }

        // Binary integer literal: 0b1010_0101 (optionally with integral suffix like L/l or B/b for byte)
        // NOTE: This must run BEFORE suffix parsing; otherwise the 'b' in 0b would be mistaken for the byte suffix.
        if (!hasDecimal)
        {
            // At this point _stringBuilder contains the first digit already (or "-" + first digit).
            // We only recognize the binary prefix when the last appended digit is '0' and the next char is 'b'/'B'.
            if (_stringBuilder.Length > 0 && _stringBuilder[_stringBuilder.Length - 1] == '0' && PeekChar(out var binPrefix) && (binPrefix == 'b' || binPrefix == 'B'))
            {
                ReadChar();
                _stringBuilder.Append(binPrefix); // append 'b'/'B'

                bool sawBit = false;
                ulong value = 0;

                // Read 0/1/_ digits
                while (PeekChar(out var bitCh))
                {
                    if (bitCh == '_')
                    {
                        ReadChar();
                        _stringBuilder.Append(bitCh);
                        continue;
                    }

                    if (bitCh == '0' || bitCh == '1')
                    {
                        ReadChar();
                        _stringBuilder.Append(bitCh);
                        sawBit = true;

                        // value = value * 2 + bit
                        value = (value << 1) | (ulong)(bitCh - '0');
                        continue;
                    }

                    break;
                }

                // Require at least one binary digit after the prefix.
                if (!sawBit)
                {
                    var text0 = GetStringBuilderValue();
                    ReportDiagnostic(DiagnosticInfo.Create(
                        CompilerDiagnostics.NumericLiteralOutOfRange,
                        new TextSpan(_tokenStartPosition, text0.Length)
                    ));
                    return new Token(SyntaxKind.NumericLiteralToken, text0, 0, text0.Length);
                }

                // Optional integral suffix for binary literals.
                bool binIsLong = false;
                bool binIsByte = false;
                if (PeekChar(out var suffix) && (suffix == 'l' || suffix == 'L' || suffix == 'b' || suffix == 'B'))
                {
                    ReadChar();
                    _stringBuilder.Append(suffix);
                    if (suffix == 'l' || suffix == 'L')
                        binIsLong = true;
                    else
                        binIsByte = true;
                }

                var text2 = GetStringBuilderValue();

                if (binIsByte)
                {
                    if (value <= byte.MaxValue)
                        return new Token(SyntaxKind.NumericLiteralToken, text2, (byte)value, text2.Length);

                    ReportDiagnostic(DiagnosticInfo.Create(
                        CompilerDiagnostics.NumericLiteralOutOfRange,
                        new TextSpan(_tokenStartPosition, text2.Length)
                    ));
                    return new Token(SyntaxKind.NumericLiteralToken, text2, (byte)0, text2.Length);
                }

                if (binIsLong)
                {
                    if (value <= (ulong)long.MaxValue)
                        return new Token(SyntaxKind.NumericLiteralToken, text2, (long)value, text2.Length);

                    ReportDiagnostic(DiagnosticInfo.Create(
                        CompilerDiagnostics.NumericLiteralOutOfRange,
                        new TextSpan(_tokenStartPosition, text2.Length)
                    ));
                    return new Token(SyntaxKind.NumericLiteralToken, text2, 0L, text2.Length);
                }

                // Default: choose int if it fits, else long if it fits.
                if (value <= int.MaxValue)
                    return new Token(SyntaxKind.NumericLiteralToken, text2, (int)value, text2.Length);

                if (value <= (ulong)long.MaxValue)
                    return new Token(SyntaxKind.NumericLiteralToken, text2, (long)value, text2.Length);

                ReportDiagnostic(DiagnosticInfo.Create(
                    CompilerDiagnostics.NumericLiteralOutOfRange,
                    new TextSpan(_tokenStartPosition, text2.Length)
                ));
                return new Token(SyntaxKind.NumericLiteralToken, text2, 0, text2.Length);
            }
        }

        if (ch != '.')
        {
            // Integer part
            while (PeekChar(out ch) && (char.IsDigit(ch) || ch == '_'))
            {
                ReadChar();
                _stringBuilder.Append(ch);
            }

            // Decimal point
            if (PeekChar(out ch) && ch == '.')
            {
                var decimalCheckpoint = _currentPosition;
                _textSource.PushPosition();
                ReadChar();

                if (PeekChar(out ch) && char.IsDigit(ch))
                {
                    _textSource.PopPosition();
                    hasDecimal = true;
                    _stringBuilder.Append('.');
                    do
                    {
                        ReadChar();
                        _stringBuilder.Append(ch);
                    }
                    while (PeekChar(out ch) && (char.IsDigit(ch) || ch == '_'));
                }
                else
                {
                    _textSource.PopAndRestorePosition();
                    _currentPosition = decimalCheckpoint;
                }
            }
        }
        else
        {
            // Leading decimal point without integer part
            while (PeekChar(out ch) && (char.IsDigit(ch) || ch == '_'))
            {
                ReadChar();
                _stringBuilder.Append(ch);
            }
        }

        // Exponent part (e.g., e+10)
        if (PeekChar(out ch) && (ch == 'e' || ch == 'E'))
        {
            hasExponent = true;
            ReadChar(); _stringBuilder.Append(ch);

            if (PeekChar(out ch) && (ch == '+' || ch == '-'))
            {
                ReadChar(); _stringBuilder.Append(ch);
            }

            while (PeekChar(out ch) && (char.IsDigit(ch) || ch == '_'))
            {
                ReadChar();
                _stringBuilder.Append(ch);
            }
        }

        // Suffix
        if (PeekChar(out ch) && (ch == 'f' || ch == 'F' || ch == 'd' || ch == 'D' || ch == 'm' || ch == 'M' || ch == 'l' || ch == 'L' || ch == 'b' || ch == 'B'))
        {
            ReadChar();
            _stringBuilder.Append(ch);

            switch (ch)
            {
                case 'm':
                case 'M':
                    isDecimal = true;
                    break;

                case 'f':
                case 'F':
                    isFloat = true;
                    break;

                case 'd':
                case 'D':
                    isDouble = true;
                    break;

                case 'l':
                case 'L':
                    isLong = true;
                    break;

                case 'b':
                case 'B':
                    isByte = true;
                    break;
            }
        }

        var text = GetStringBuilderValue();

        // Decimal literal
        if (isDecimal || text.EndsWith("m", StringComparison.OrdinalIgnoreCase))
        {
            // Strip suffix and separators
            var numericText = text[..^1].Replace("_", string.Empty);

            // Note: Raven follows C# here: decimal literals do not support exponent notation.
            if (numericText.IndexOf('e') >= 0 || numericText.IndexOf('E') >= 0)
            {
                ReportDiagnostic(DiagnosticInfo.Create(
                    CompilerDiagnostics.NumericLiteralOutOfRange,
                    new TextSpan(_tokenStartPosition, text.Length)
                ));

                return new Token(SyntaxKind.NumericLiteralToken, text, 0m, text.Length);
            }

            if (decimal.TryParse(numericText, NumberStyles.Number, CultureInfo.InvariantCulture, out var decimalValue))
            {
                return new Token(SyntaxKind.NumericLiteralToken, text, decimalValue, text.Length);
            }
            else
            {
                ReportDiagnostic(DiagnosticInfo.Create(
                    CompilerDiagnostics.NumericLiteralOutOfRange,
                    new TextSpan(_tokenStartPosition, text.Length)
                ));
                return new Token(SyntaxKind.NumericLiteralToken, text, 0m, text.Length);
            }
        }

        // Float literal
        if (isFloat || text.EndsWith("f", StringComparison.OrdinalIgnoreCase))
        {
            var numericText = text.EndsWith("f", StringComparison.OrdinalIgnoreCase)
                ? text[..^1]
                : text;
            numericText = numericText.Replace("_", string.Empty);

            if (float.TryParse(numericText, NumberStyles.Float, CultureInfo.InvariantCulture, out var floatValue))
            {
                return new Token(SyntaxKind.NumericLiteralToken, text, floatValue, text.Length);
            }
            else
            {
                ReportDiagnostic(DiagnosticInfo.Create(
                    CompilerDiagnostics.NumericLiteralOutOfRange,
                    new TextSpan(_tokenStartPosition, text.Length)
                ));
                return new Token(SyntaxKind.NumericLiteralToken, text, 0f, text.Length);
            }
        }

        // Double literal (explicit 'd' suffix OR any literal with '.'/exponent). Unsuffixed real literals default to double.
        if (isDouble || hasDecimal || hasExponent || text.EndsWith("d", StringComparison.OrdinalIgnoreCase))
        {
            var numericText = text.EndsWith("d", StringComparison.OrdinalIgnoreCase)
                ? text[..^1]
                : text;
            numericText = numericText.Replace("_", string.Empty);

            if (double.TryParse(numericText, NumberStyles.Float, CultureInfo.InvariantCulture, out var doubleValue))
            {

                return new Token(SyntaxKind.NumericLiteralToken, text, doubleValue, text.Length);
            }
            else
            {
                ReportDiagnostic(DiagnosticInfo.Create(
                    CompilerDiagnostics.NumericLiteralOutOfRange,
                    new TextSpan(_tokenStartPosition, text.Length)
                ));
                return new Token(SyntaxKind.NumericLiteralToken, text, 0d, text.Length);
            }
        }

        // Integer literal
        // Strip separators and (optional) suffix.
        var numericIntText = text.Replace("_", string.Empty);

        // Byte suffix
        if (isByte || numericIntText.EndsWith("b", StringComparison.OrdinalIgnoreCase))
        {
            if (numericIntText.EndsWith("b", StringComparison.OrdinalIgnoreCase))
                numericIntText = numericIntText[..^1];

            if (byte.TryParse(numericIntText, NumberStyles.Integer, CultureInfo.InvariantCulture, out var byteValue))
            {
                return new Token(SyntaxKind.NumericLiteralToken, text, byteValue, text.Length);
            }

            ReportDiagnostic(DiagnosticInfo.Create(
                CompilerDiagnostics.NumericLiteralOutOfRange,
                new TextSpan(_tokenStartPosition, text.Length)
            ));
            return new Token(SyntaxKind.NumericLiteralToken, text, (byte)0, text.Length);
        }

        // Long suffix
        if (isLong || numericIntText.EndsWith("l", StringComparison.OrdinalIgnoreCase))
        {
            if (numericIntText.EndsWith("l", StringComparison.OrdinalIgnoreCase))
                numericIntText = numericIntText[..^1];

            if (long.TryParse(numericIntText, NumberStyles.Integer, CultureInfo.InvariantCulture, out var longValue))
            {
                return new Token(SyntaxKind.NumericLiteralToken, text, longValue, text.Length);
            }

            ReportDiagnostic(DiagnosticInfo.Create(
                CompilerDiagnostics.NumericLiteralOutOfRange,
                new TextSpan(_tokenStartPosition, text.Length)
            ));
            return new Token(SyntaxKind.NumericLiteralToken, text, 0L, text.Length);
        }

        // Default integer literal chooses the smallest integral type starting at int (not byte).

        if (int.TryParse(numericIntText, NumberStyles.Integer, CultureInfo.InvariantCulture, out var intValue))
        {
            return new Token(SyntaxKind.NumericLiteralToken, text, intValue, text.Length);
        }

        if (long.TryParse(numericIntText, NumberStyles.Integer, CultureInfo.InvariantCulture, out var longValue2))
        {
            return new Token(SyntaxKind.NumericLiteralToken, text, longValue2, text.Length);
        }

        ReportDiagnostic(DiagnosticInfo.Create(
            CompilerDiagnostics.NumericLiteralOutOfRange,
            new TextSpan(_tokenStartPosition, text.Length)
        ));
        return new Token(SyntaxKind.NumericLiteralToken, text, 0, text.Length);
    }

    // NOTE: Comment lexing uses IsEndOfLine to ensure single-line comments never consume newline tokens.
    private bool IsEndOfLine(char ch)
    {
        return ch == '\n'
            || ch == '\r'
            || ch == '\u0085'
            || ch == '\u2028'
            || ch == '\u2029';
    }

    private string GetStringBuilderValue() => string.Intern(_stringBuilder.ToString());

    private TextSpan GetTokenStartPositionSpan() => new TextSpan(_tokenStartPosition, 0);

    private TextSpan GetEndPositionSpan(int offset = 0) => new TextSpan(_currentPosition + offset, 0);

    private char ReadChar()
    {
        var value = ReadCore();
        return value == -1 ? '\0' : (char)value;
    }

    private int ReadCore()
    {
        var ch = _textSource.Read();
        _currentPosition++;
        return ch;
    }

    private bool ReadChar(out char ch)
    {
        var value = ReadCore();
        if (value == -1)
        {
            ch = default;
            return false;
        }
        ch = (char)value;
        return true;
    }

    private bool PeekChar(out char ch)
    {
        var value = _textSource.Peek();
        if (value == -1)
        {
            ch = default;
            return false;
        }
        ch = (char)value;
        return true;
    }


    private bool ReadWhile(char ch)
    {
        if (PeekChar(out var next) && next == ch)
        {
            ReadChar(); // advances _textSource and updates _currentPosition
            return true;
        }

        return false;
    }

    public bool IsEndOfFile => _textSource.Peek() == -1;
}
