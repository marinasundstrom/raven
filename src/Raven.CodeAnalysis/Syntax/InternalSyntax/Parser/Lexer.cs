using System.Text;

namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

internal class Lexer : ILexer
{
    private readonly TextReader _textReader;
    private StringBuilder? _stringBuilder;
    private readonly List<SyntaxToken> _lookaheadTokens = new List<SyntaxToken>();
    private int _currentPosition = 0;
    private int _tokenStartPosition = 0;

    public Lexer(TextReader textReader)
    {
        this._textReader = textReader;
    }

    public SyntaxToken ReadToken()
    {
        SyntaxToken token;

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

    public SyntaxToken PeekToken(int index = 0)
    {
        // Ensure the lookahead tokens list is populated up to the requested index
        while (_lookaheadTokens.Count <= index)
        {
            _lookaheadTokens.Add(ReadTokenCore());
        }
        return _lookaheadTokens[index];
    }

    private SyntaxToken ReadTokenCore()
    {
        List<DiagnosticInfo> diagnostics = new List<DiagnosticInfo>();

        _tokenStartPosition = _currentPosition;

        while (ReadChar(out var ch))
        {
            if (char.IsLetterOrDigit(ch))
            {
                _stringBuilder = new StringBuilder();

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

                    return new SyntaxToken(syntaxKind, _stringBuilder.ToString(), diagnostics: diagnostics);
                }
                else if (char.IsDigit(ch))
                {
                    while (PeekChar(out ch) && char.IsDigit(ch))
                    {
                        ReadChar();
                        _stringBuilder.Append(ch);
                    }

                    return new SyntaxToken(SyntaxKind.NumericLiteralToken, int.Parse(_stringBuilder.ToString()), _stringBuilder.Length, diagnostics: diagnostics);
                }
            }
            else
            {
                string chStr = string.Intern(ch.ToString());

                switch (ch)
                {
                    case '+':
                        return new SyntaxToken(SyntaxKind.PlusToken, chStr);

                    case '-':
                        return new SyntaxToken(SyntaxKind.MinusToken, chStr);

                    case '/':
                        /*
                        if (PeekChar(out var ch10) && ch10 == '/')
                        {
                            ReadChar();
                            return new InternalSyntax.SyntaxTr(SyntaxKind.LessThanEqualsToken, "<=");
                        }
                        */
                        return new SyntaxToken(SyntaxKind.SlashToken, chStr);

                    case '*':
                        return new SyntaxToken(SyntaxKind.StarToken, chStr);

                    case '%':
                        return new SyntaxToken(SyntaxKind.PercentToken, chStr);

                    case '.':
                        return new SyntaxToken(SyntaxKind.DotToken, chStr);

                    case ',':
                        return new SyntaxToken(SyntaxKind.CommaToken, chStr);

                    case ':':
                        return new SyntaxToken(SyntaxKind.ColonToken, chStr);

                    case ';':
                        return new SyntaxToken(SyntaxKind.SemicolonToken, chStr);

                    /*
                    
                   case '"':
                       return new SyntaxToken(SyntaxKind.DoublequoteToken, chStr);

                   case '\'':
                       return new SyntaxToken(SyntaxKind.SinglequoteToken, chStr);

                   case '`':
                       return new SyntaxToken(SyntaxKind.BackquoteToken, chStr);

                   */

                    case '(':
                        return new SyntaxToken(SyntaxKind.OpenParenToken, chStr);

                    case ')':
                        return new SyntaxToken(SyntaxKind.CloseParenToken, chStr);

                    case '{':
                        return new SyntaxToken(SyntaxKind.OpenBraceToken, chStr);

                    case '}':
                        return new SyntaxToken(SyntaxKind.CloseBraceToken, chStr);

                    /*

                case '[':
                    return new SyntaxToken(SyntaxKind.OpenSquareToken, chStr);

                case ']':
                    return new SyntaxToken(SyntaxKind.CloseSquareToken, chStr);
                    */

                    case '=':
                        return new SyntaxToken(SyntaxKind.EqualsToken, chStr);

                    case '!':
                        if (PeekChar(out var ch6) && ch6 == '=')
                        {
                            ReadChar();
                            return new SyntaxToken(SyntaxKind.NotEqualsToken, "!=");
                        }
                        return new SyntaxToken(SyntaxKind.ExclamationToken, chStr);

                    case '<':
                        if (PeekChar(out var ch2) && ch2 == '=')
                        {
                            ReadChar();
                            return new SyntaxToken(SyntaxKind.LessThanEqualsToken, "<=");
                        }
                        return new SyntaxToken(SyntaxKind.LessThanToken, chStr);

                    case '>':
                        if (PeekChar(out var ch3) && ch3 == '=')
                        {
                            ReadChar();
                            return new SyntaxToken(SyntaxKind.GreaterOrEqualsToken, ">=");
                        }
                        return new SyntaxToken(SyntaxKind.GreaterThanToken, chStr);


                    case ' ':
                        int length = 1;

                        while (ReadWhile(' '))
                        {
                            length++;
                        }

                        return new SyntaxToken(SyntaxKind.Whitespace, string.Intern(new string(' ', length)));


                    case '\'':
                        _stringBuilder = new StringBuilder();

                        _stringBuilder.Append(ch);

                        while (PeekChar(out var ch9))
                        {
                            _stringBuilder.Append(ch9);
                            ReadChar();

                            if (IsEndOfFile)
                            {
                                diagnostics.Add(
                                    DiagnosticInfo.Create(
                                        CompilerDiagnostics.NewlineInConstant,
                                        GetTokenStartPositionSpan()
                                    ));

                                break;
                            }

                            if (PeekChar(out ch9) && ch9 == '\'')
                            {
                                ReadChar();
                                _stringBuilder.Append(ch9);
                                break;
                            }
                        }

                        return new SyntaxToken(SyntaxKind.CharacterLiteralToken, _stringBuilder.ToString(), diagnostics: diagnostics);


                    case '\"':
                        _stringBuilder = new StringBuilder();

                        _stringBuilder.Append(ch);

                        while (PeekChar(out var ch8) && ch8 != '\"')
                        {
                            _stringBuilder.Append(ch8);
                            ReadChar();

                            if (IsEndOfFile)
                            {
                                diagnostics.Add(
                                    DiagnosticInfo.Create(
                                        CompilerDiagnostics.NewlineInConstant,
                                        GetTokenStartPositionSpan()
                                    ));

                                break;
                            }

                            if (PeekChar(out ch8) && ch8 == '\"')
                            {
                                ReadChar();
                                _stringBuilder.Append(ch8);
                                break;
                            }
                        }

                        return new SyntaxToken(SyntaxKind.StringLiteralToken, _stringBuilder.ToString(), diagnostics: diagnostics);


                    case '\t':
                        return new SyntaxToken(SyntaxKind.TabToken, "\t");

                    case '\n':
                        return new SyntaxToken(SyntaxKind.EndOfLineToken, "\n");

                    case '\r':
                        return new SyntaxToken(SyntaxKind.CarriageReturnToken, "\r");
                }
            }

            return new SyntaxToken(SyntaxKind.None, ch.ToString());
        }

        return new SyntaxToken(SyntaxKind.EndOfFileToken, string.Empty);
    }

    private TextSpan GetTokenStartPositionSpan()
    {
        //System.Console.WriteLine("Hello" + ", World!);
        return new TextSpan(_tokenStartPosition, 0);
    }

    private TextSpan GetEndPositionSpan(int offset = 0)
    {
        //System.Console.WriteLine("Hello" + ", World!);
        return new TextSpan(_currentPosition + offset, 0);
    }

    private char ReadChar()
    {
        return (char)ReadCore();
    }

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