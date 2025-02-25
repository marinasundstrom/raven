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
            _lookaheadTokens.Add(ReadTokenCore());
        }
        return _lookaheadTokens[index];
    }

    private Token ReadTokenCore()
    {
        List<DiagnosticInfo> diagnostics = new List<DiagnosticInfo>();

        _tokenStartPosition = _currentPosition;

        while (ReadChar(out var ch))
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

                    return new Token(syntaxKind, _stringBuilder.ToString(), diagnostics: diagnostics);
                }
                else if (char.IsDigit(ch))
                {
                    while (PeekChar(out ch) && char.IsDigit(ch))
                    {
                        ReadChar();
                        _stringBuilder.Append(ch);
                    }

                    return new Token(SyntaxKind.NumericLiteralToken, int.Parse(_stringBuilder.ToString()), _stringBuilder.Length, diagnostics: diagnostics);
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
                        return new Token(SyntaxKind.MinusToken, chStr);

                    case '/':
                        /*
                        if (PeekChar(out var ch10) && ch10 == '/')
                        {
                            ReadChar();
                            return new InternalSyntax.SyntaxTr(SyntaxKind.LessThanEqualsToken, "<=");
                        }
                        */
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

                    /*
                    
                   case '"':
                       return new Token(SyntaxKind.DoublequoteToken, chStr);

                   case '\'':
                       return new Token(SyntaxKind.SinglequoteToken, chStr);

                   case '`':
                       return new Token(SyntaxKind.BackquoteToken, chStr);

                   */

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
                        return new Token(SyntaxKind.EqualsToken, chStr);

                    case '!':
                        if (PeekChar(out var ch6) && ch6 == '=')
                        {
                            ReadChar();
                            return new Token(SyntaxKind.NotEqualsToken, "!=");
                        }
                        return new Token(SyntaxKind.ExclamationToken, chStr);

                    case '<':
                        if (PeekChar(out var ch2) && ch2 == '=')
                        {
                            ReadChar();
                            return new Token(SyntaxKind.LessThanEqualsToken, "<=");
                        }
                        return new Token(SyntaxKind.LessThanToken, chStr);

                    case '>':
                        if (PeekChar(out var ch3) && ch3 == '=')
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

                        return new Token(SyntaxKind.CharacterLiteralToken, _stringBuilder.ToString(), diagnostics: diagnostics);


                    case '\"':
                        _stringBuilder.Append(ch); // Append opening quote

                        while (PeekChar(out var ch8))
                        {
                            ReadChar();

                            if (ch8 == '\"') // Found closing quote
                            {
                                _stringBuilder.Append(ch8);
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

                            _stringBuilder.Append(ch8);
                        }

                        return new Token(SyntaxKind.StringLiteralToken, _stringBuilder.ToString(), diagnostics: diagnostics);

                    case '\t':
                        return new Token(SyntaxKind.TabToken, "\t");

                    case '\n':
                        return new Token(SyntaxKind.EndOfLineToken, "\n");

                    case '\r':
                        return new Token(SyntaxKind.CarriageReturnToken, "\r");
                }
            }

            return new Token(SyntaxKind.None, ch.ToString());
        }

        return new Token(SyntaxKind.EndOfFileToken, string.Empty);
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