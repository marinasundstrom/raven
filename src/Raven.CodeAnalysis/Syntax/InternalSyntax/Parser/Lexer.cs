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
        List<DiagnosticInfo> diagnostics = new List<DiagnosticInfo>();

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
                    return new Token(SyntaxKind.IdentifierToken, GetStringBuilderValue(), diagnostics: diagnostics);
                }

                ReadChar(out ch2);
                _stringBuilder.Append(ch2);

                while (PeekChar(out ch2) && SyntaxFacts.IsIdentifierPartCharacter(ch2))
                {
                    ReadChar();
                    _stringBuilder.Append(ch2);
                }

                var text = GetStringBuilderValue();
                var valueText = string.Intern(text.Substring(1));
                return new Token(SyntaxKind.IdentifierToken, text, valueText, diagnostics: diagnostics);
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
                            return new Token(SyntaxKind.TrueKeyword, GetStringBuilderValue(), true, _stringBuilder.Length, diagnostics: diagnostics);

                        case SyntaxKind.FalseKeyword:
                            return new Token(SyntaxKind.FalseKeyword, GetStringBuilderValue(), false, _stringBuilder.Length, diagnostics: diagnostics);
                    }

                    return new Token(syntaxKind, GetStringBuilderValue(), diagnostics: diagnostics);
                }
                else if (char.IsDigit(ch) || ch == '.')
                    return ParseNumber(diagnostics, ref ch);
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
                            return ParseNumber(diagnostics, ref ch, true);
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
                                return new Token(kind, GetStringBuilderValue(), diagnostics: diagnostics);
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
                                        return new Token(SyntaxKind.MultiLineCommentTrivia, GetStringBuilderValue(), diagnostics: diagnostics);
                                    }
                                }

                                // Unterminated comment; return what we have. (Keep newline/EOF behavior consistent.)
                                // If you have a dedicated diagnostic for this, you can add it here.
                                return new Token(SyntaxKind.MultiLineCommentTrivia, GetStringBuilderValue(), diagnostics: diagnostics);
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
                        return new Token(SyntaxKind.LessThanToken, chStr);

                    case '>':
                        if (PeekChar(out ch2) && ch2 == '=')
                        {
                            ReadChar();
                            return new Token(SyntaxKind.GreaterThanOrEqualsToken, ">=");
                        }
                        return new Token(SyntaxKind.GreaterThanToken, chStr);


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
                                diagnostics.Add(DiagnosticInfo.Create(
                                    CompilerDiagnostics.UnterminatedCharacterLiteral,
                                    GetTokenStartPositionSpan()));
                                return new Token(SyntaxKind.CharacterLiteralToken, GetStringBuilderValue(), diagnostics: diagnostics);
                            }

                            char character;

                            if (ch == '\\')
                            {
                                _stringBuilder.Append(ch); // backslash

                                if (!ReadChar(out ch))
                                {
                                    diagnostics.Add(DiagnosticInfo.Create(
                                        CompilerDiagnostics.UnterminatedCharacterLiteral,
                                        GetTokenStartPositionSpan()));
                                    return new Token(SyntaxKind.CharacterLiteralToken, GetStringBuilderValue(), diagnostics: diagnostics);
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
                                        diagnostics.Add(DiagnosticInfo.Create(
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
                                diagnostics.Add(DiagnosticInfo.Create(
                                    CompilerDiagnostics.UnterminatedCharacterLiteral,
                                    GetTokenStartPositionSpan()));
                                return new Token(SyntaxKind.CharacterLiteralToken, GetStringBuilderValue(), diagnostics: diagnostics);
                            }

                            _stringBuilder.Append(ch); // closing quote

                            return new Token(
                                SyntaxKind.CharacterLiteralToken,
                                GetStringBuilderValue(),
                                character,
                                _stringBuilder.Length,
                                diagnostics: diagnostics);
                        }

                    case '"':
                        _stringBuilder.Append(ch); // Append opening quote

                        var interpolationDepth = 0;

                        while (true)
                        {
                            if (!PeekChar(out ch2))
                            {
                                diagnostics.Add(
                                    DiagnosticInfo.Create(
                                        CompilerDiagnostics.NewlineInConstant,
                                        GetTokenStartPositionSpan()));
                                break;
                            }

                            ReadChar();

                            if (ch2 == '"' && interpolationDepth == 0) // Found closing quote
                            {
                                _stringBuilder.Append(ch2);
                                break;
                            }

                            if (IsEndOfLine(ch2)) // Check EOL before adding anything
                            {
                                diagnostics.Add(
                                    DiagnosticInfo.Create(
                                        CompilerDiagnostics.NewlineInConstant,
                                        GetTokenStartPositionSpan()));
                                break;
                            }

                            if (ch2 == '\\')
                            {
                                _stringBuilder.Append(ch2);

                                if (!PeekChar(out var escaped))
                                {
                                    diagnostics.Add(
                                        DiagnosticInfo.Create(
                                            CompilerDiagnostics.NewlineInConstant,
                                            GetTokenStartPositionSpan()));
                                    break;
                                }

                                ReadChar();

                                if (IsEndOfLine(escaped))
                                {
                                    diagnostics.Add(
                                        DiagnosticInfo.Create(
                                            CompilerDiagnostics.NewlineInConstant,
                                            GetTokenStartPositionSpan()));
                                    break;
                                }

                                _stringBuilder.Append(escaped);
                                continue;
                            }

                            if (interpolationDepth == 0 && ch2 == '$')
                            {
                                _stringBuilder.Append(ch2);

                                if (PeekChar(out var next) && next == '{')
                                {
                                    ReadChar();
                                    _stringBuilder.Append(next);
                                    interpolationDepth = 1;
                                }

                                continue;
                            }

                            if (interpolationDepth > 0)
                            {
                                if (ch2 == '{')
                                {
                                    _stringBuilder.Append(ch2);
                                    interpolationDepth++;
                                    continue;
                                }

                                if (ch2 == '}')
                                {
                                    _stringBuilder.Append(ch2);
                                    interpolationDepth--;
                                    continue;
                                }
                            }

                            _stringBuilder.Append(ch2);
                        }

                        var str = GetStringBuilderValue();
                        ReadOnlySpan<char> innerText = str.Length >= 2
                            ? str.AsSpan(1, str.Length - 2)
                            : ReadOnlySpan<char>.Empty;
                        var decoded = SyntaxFacts.DecodeStringLiteralContent(innerText, out var hadInvalidEscape);

                        if (hadInvalidEscape)
                        {
                            diagnostics.Add(
                                DiagnosticInfo.Create(
                                    CompilerDiagnostics.InvalidEscapeSequence,
                                    GetTokenStartPositionSpan()));
                        }

                        return new Token(
                            SyntaxKind.StringLiteralToken,
                            str,
                            string.Intern(decoded),
                            diagnostics: diagnostics);

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
            /*diagnostics.Add(DiagnosticInfo.Create(
                     CompilerDiagnostics.UnknownCharacter,
                     new TextSpan(_tokenStartPosition, 1)
                 ));*/

            return new Token(SyntaxKind.None, ch.ToString());
        }

        return new Token(SyntaxKind.EndOfFileToken, string.Empty);
    }

    private Token ParseNumber(List<DiagnosticInfo> diagnostics, ref char ch, bool negative = false)
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
                diagnostics.Add(DiagnosticInfo.Create(
                    CompilerDiagnostics.NumericLiteralOutOfRange,
                    new TextSpan(_tokenStartPosition, text.Length)
                ));

                return new Token(SyntaxKind.NumericLiteralToken, text, 0m, text.Length, diagnostics: diagnostics);
            }

            if (decimal.TryParse(numericText, NumberStyles.Number, CultureInfo.InvariantCulture, out var decimalValue))
            {
                return new Token(SyntaxKind.NumericLiteralToken, text, decimalValue, text.Length, diagnostics: diagnostics);
            }
            else
            {
                diagnostics.Add(DiagnosticInfo.Create(
                    CompilerDiagnostics.NumericLiteralOutOfRange,
                    new TextSpan(_tokenStartPosition, text.Length)
                ));
                return new Token(SyntaxKind.NumericLiteralToken, text, 0m, text.Length, diagnostics: diagnostics);
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
                return new Token(SyntaxKind.NumericLiteralToken, text, floatValue, text.Length, diagnostics: diagnostics);
            }
            else
            {
                diagnostics.Add(DiagnosticInfo.Create(
                    CompilerDiagnostics.NumericLiteralOutOfRange,
                    new TextSpan(_tokenStartPosition, text.Length)
                ));
                return new Token(SyntaxKind.NumericLiteralToken, text, 0f, text.Length, diagnostics: diagnostics);
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

                return new Token(SyntaxKind.NumericLiteralToken, text, doubleValue, text.Length, diagnostics: diagnostics);
            }
            else
            {
                diagnostics.Add(DiagnosticInfo.Create(
                    CompilerDiagnostics.NumericLiteralOutOfRange,
                    new TextSpan(_tokenStartPosition, text.Length)
                ));
                return new Token(SyntaxKind.NumericLiteralToken, text, 0d, text.Length, diagnostics: diagnostics);
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
                return new Token(SyntaxKind.NumericLiteralToken, text, byteValue, text.Length, diagnostics: diagnostics);
            }

            diagnostics.Add(DiagnosticInfo.Create(
                CompilerDiagnostics.NumericLiteralOutOfRange,
                new TextSpan(_tokenStartPosition, text.Length)
            ));
            return new Token(SyntaxKind.NumericLiteralToken, text, (byte)0, text.Length, diagnostics: diagnostics);
        }

        // Long suffix
        if (isLong || numericIntText.EndsWith("l", StringComparison.OrdinalIgnoreCase))
        {
            if (numericIntText.EndsWith("l", StringComparison.OrdinalIgnoreCase))
                numericIntText = numericIntText[..^1];

            if (long.TryParse(numericIntText, NumberStyles.Integer, CultureInfo.InvariantCulture, out var longValue))
            {
                return new Token(SyntaxKind.NumericLiteralToken, text, longValue, text.Length, diagnostics: diagnostics);
            }

            diagnostics.Add(DiagnosticInfo.Create(
                CompilerDiagnostics.NumericLiteralOutOfRange,
                new TextSpan(_tokenStartPosition, text.Length)
            ));
            return new Token(SyntaxKind.NumericLiteralToken, text, 0L, text.Length, diagnostics: diagnostics);
        }

        // Default integer literal chooses the smallest integral type starting at int (not byte).

        if (int.TryParse(numericIntText, NumberStyles.Integer, CultureInfo.InvariantCulture, out var intValue))
        {
            return new Token(SyntaxKind.NumericLiteralToken, text, intValue, text.Length, diagnostics: diagnostics);
        }

        if (long.TryParse(numericIntText, NumberStyles.Integer, CultureInfo.InvariantCulture, out var longValue2))
        {
            return new Token(SyntaxKind.NumericLiteralToken, text, longValue2, text.Length, diagnostics: diagnostics);
        }

        diagnostics.Add(DiagnosticInfo.Create(
            CompilerDiagnostics.NumericLiteralOutOfRange,
            new TextSpan(_tokenStartPosition, text.Length)
        ));
        return new Token(SyntaxKind.NumericLiteralToken, text, 0, text.Length, diagnostics: diagnostics);
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
