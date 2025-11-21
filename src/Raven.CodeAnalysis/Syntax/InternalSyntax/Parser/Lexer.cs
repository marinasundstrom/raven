using System;
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
    }

    public void CreateCheckpoint()
    {
        _textSource.PushPosition();
    }

    public void RewindToCheckpoint()
    {
        var position = _textSource.PopAndRestorePosition();
        _lookaheadTokens.Clear();
        _currentPosition = position;
        _tokenStartPosition = position;
    }

    public void ClearCheckpoint()
    {
        _textSource.PopPosition();
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
                        return new Token(SyntaxKind.MinusToken, chStr);

                    case '/':
                        return new Token(SyntaxKind.SlashToken, chStr);

                    case '*':
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
                        return new Token(SyntaxKind.QuestionToken, chStr);

                    case '&':
                        if (PeekChar(out ch2) && ch2 == '&')
                        {
                            ReadChar();
                            return new Token(SyntaxKind.AmpersandAmpersandToken, "&&");
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
        if (PeekChar(out ch) && (ch == 'f' || ch == 'F' || ch == 'd' || ch == 'D'))
        {
            isFloat = true;
            ReadChar(); _stringBuilder.Append(ch);
        }

        var text = GetStringBuilderValue();

        // Float literal
        if (text.EndsWith("f", StringComparison.OrdinalIgnoreCase))
        {
            var numericText = text[..^1].Replace("_", string.Empty);
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

        // Double literal
        if (hasDecimal || hasExponent || text.EndsWith("d", StringComparison.OrdinalIgnoreCase))
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

        // Integer literal (default case)
        var numericIntText = text.Replace("_", string.Empty);
        if (int.TryParse(numericIntText, NumberStyles.Integer, CultureInfo.InvariantCulture, out var intValue))
        {
            return new Token(SyntaxKind.NumericLiteralToken, text, intValue, text.Length, diagnostics: diagnostics);
        }
        // Long literal
        else if (long.TryParse(numericIntText, NumberStyles.Integer, CultureInfo.InvariantCulture, out var longValue))
        {
            return new Token(SyntaxKind.NumericLiteralToken, text, longValue, text.Length, diagnostics: diagnostics);
        }
        else
        {
            diagnostics.Add(DiagnosticInfo.Create(
                CompilerDiagnostics.NumericLiteralOutOfRange,
                new TextSpan(_tokenStartPosition, text.Length)
            ));
            return new Token(SyntaxKind.NumericLiteralToken, text, 0, text.Length, diagnostics: diagnostics);
        }
    }

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
        var value = _textSource.Peek();
        if (value == ch)
        {
            _textSource.Read();
            return true;
        }
        return false;
    }

    public bool IsEndOfFile => _textSource.Peek() == -1;
}
