using System.Globalization;
using System.Text;

namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

internal class Lexer : ILexer
{
    private readonly TextReader _textReader;
    private readonly StringBuilder _stringBuilder = new StringBuilder();
    private readonly List<Token> _lookaheadTokens = new List<Token>();
    private int _currentPosition = 0;
    private int _tokenStartPosition = 0;

    public Lexer(TextReader textReader)
    {
        _textReader = textReader;
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

            if (char.IsLetterOrDigit(ch))
            {

                _stringBuilder.Append(ch);

                if (char.IsLetter(ch))
                {
                    SyntaxKind syntaxKind = SyntaxKind.IdentifierToken;

                    while (PeekChar(out ch) && (char.IsLetter(ch) || char.IsDigit(ch) || ch == '_' || ch == '$'))
                    {
                        ReadChar();
                        _stringBuilder.Append(ch);
                    }

                    if (!SyntaxFacts.ParseReservedWord(_stringBuilder.ToString(), out syntaxKind))
                    {
                        syntaxKind = SyntaxKind.IdentifierToken;
                    }

                    switch (syntaxKind)
                    {
                        case SyntaxKind.TrueKeyword:
                            return new Token(SyntaxKind.TrueKeyword, _stringBuilder.ToString(), true, _stringBuilder.Length, diagnostics: diagnostics);

                        case SyntaxKind.FalseKeyword:
                            return new Token(SyntaxKind.FalseKeyword, _stringBuilder.ToString(), false, _stringBuilder.Length, diagnostics: diagnostics);
                    }

                    return new Token(syntaxKind, _stringBuilder.ToString(), diagnostics: diagnostics);
                }
                else if (char.IsDigit(ch))
                {
                    bool isFloat = false;
                    bool hasDecimal = false;
                    bool hasExponent = false;

                    // Integer part
                    while (PeekChar(out ch) && char.IsDigit(ch))
                    {
                        ReadChar();
                        _stringBuilder.Append(ch);
                    }

                    // Decimal point
                    if (PeekChar(out ch) && ch == '.')
                    {
                        hasDecimal = true;
                        ReadChar(); _stringBuilder.Append('.');
                        while (PeekChar(out ch) && char.IsDigit(ch))
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

                        while (PeekChar(out ch) && char.IsDigit(ch))
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

                    var text = _stringBuilder.ToString();

                    // Float literal
                    if (text.EndsWith("f", StringComparison.OrdinalIgnoreCase))
                    {
                        var numericText = text[..^1];
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
                    if (int.TryParse(text, NumberStyles.Integer, CultureInfo.InvariantCulture, out var intValue))
                    {
                        return new Token(SyntaxKind.NumericLiteralToken, text, intValue, text.Length, diagnostics: diagnostics);
                    }
                    // Long literal
                    else if (long.TryParse(text, NumberStyles.Integer, CultureInfo.InvariantCulture, out var longValue))
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
                        return new Token(SyntaxKind.MinusToken, chStr);

                    case '/':
                        return new Token(SyntaxKind.SlashToken, chStr);

                    case '*':
                        return new Token(SyntaxKind.StarToken, chStr);

                    case '%':
                        return new Token(SyntaxKind.PercentToken, chStr);

                    case '.':
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
                        return new Token(SyntaxKind.EqualsToken, chStr);

                    case '|':
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
                        return new Token(SyntaxKind.AmpersandToken, chStr);

                    case '<':
                        if (PeekChar(out ch2) && ch2 == '=')
                        {
                            ReadChar();
                            return new Token(SyntaxKind.LessThanEqualsToken, "<=");
                        }
                        return new Token(SyntaxKind.LessThanToken, chStr);

                    case '>':
                        if (PeekChar(out ch2) && ch2 == '=')
                        {
                            ReadChar();
                            return new Token(SyntaxKind.GreaterOrEqualsToken, ">=");
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
                                return new Token(SyntaxKind.CharacterLiteralToken, _stringBuilder.ToString(), diagnostics: diagnostics);
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
                                    return new Token(SyntaxKind.CharacterLiteralToken, _stringBuilder.ToString(), diagnostics: diagnostics);
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
                                return new Token(SyntaxKind.CharacterLiteralToken, _stringBuilder.ToString(), diagnostics: diagnostics);
                            }

                            _stringBuilder.Append(ch); // closing quote

                            return new Token(
                                SyntaxKind.CharacterLiteralToken,
                                _stringBuilder.ToString(),
                                character,
                                _stringBuilder.Length,
                                diagnostics: diagnostics);
                        }

                    case '\"':
                        _stringBuilder.Append(ch); // Append opening quote

                        while (PeekChar(out ch2))
                        {
                            ReadChar();

                            if (ch2 == '\"') // Found closing quote
                            {
                                _stringBuilder.Append(ch2);
                                break;
                            }

                            if (IsEndOfFile) // Check EOF before adding anything
                            {
                                diagnostics.Add(
                                    DiagnosticInfo.Create(
                                        CompilerDiagnostics.NewlineInConstant,
                                        GetTokenStartPositionSpan()
                                    ));

                                break;
                            }

                            _stringBuilder.Append(ch2);
                        }

                        var str = _stringBuilder.ToString();

                        return new Token(SyntaxKind.StringLiteralToken, str, str[1..^1], diagnostics: diagnostics);

                    /*

                    case '`':
                        return new Token(SyntaxKind.BackquoteToken, chStr);

                    */

                    case '\t':
                        return new Token(SyntaxKind.TabToken, "\t");

                    case '\n':
                        return new Token(LineFeedTokenKind, "\n");

                    case '\r':
                        if (MergeCarriageReturnAndLineFeed && PeekChar(out ch2) && ch2 == '\n')
                        {
                            ReadChar();
                            return new Token(CarriageReturnLineFeedTokenKind, "\r\n");
                        }
                        return new Token(SyntaxKind.CarriageReturnToken, "\r");
                }
            }

            return new Token(SyntaxKind.None, ch.ToString());
        }

        return new Token(SyntaxKind.EndOfFileToken, string.Empty);
    }

    private TextSpan GetTokenStartPositionSpan() => new TextSpan(_tokenStartPosition, 0);

    private TextSpan GetEndPositionSpan(int offset = 0) => new TextSpan(_currentPosition + offset, 0);

    private char ReadChar() => (char)ReadCore();

    private int ReadCore()
    {
        var ch = _textReader.Read();
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
        var value = _textReader.Peek();
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
        var value = _textReader.Peek();
        if (value == ch)
        {
            _textReader.Read();
            return true;
        }
        return false;
    }

    public bool IsEndOfFile => _textReader.Peek() == -1;
}