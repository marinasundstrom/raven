using System.Text;

namespace Raven.CodeAnalysis.Syntax.Parser;

internal class Lexer : ILexer
{
    private readonly TextReader _textReader;
    private StringBuilder? _stringBuilder;
    private InternalSyntax.SyntaxToken? _lookaheadToken;

    public Lexer(TextReader textReader)
    {
        this._textReader = textReader;
    }

    public InternalSyntax.SyntaxToken ReadToken()
    {
        if (_lookaheadToken != null)
        {
            var token = (InternalSyntax.SyntaxToken)_lookaheadToken;
            _lookaheadToken = null;
            return token;
        }
        return ReadTokenCore();
    }

    public InternalSyntax.SyntaxToken PeekToken()
    {
        if (_lookaheadToken == null)
        {
            _lookaheadToken = ReadTokenCore();
        }
        return (InternalSyntax.SyntaxToken)_lookaheadToken;
    }

    private InternalSyntax.SyntaxToken ReadTokenCore()
    {
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

                    if (!Syntax.SyntaxFacts.ParseReservedWord(_stringBuilder.ToString(), out syntaxKind))
                    {
                        syntaxKind = SyntaxKind.IdentifierToken;
                    }

                    return new InternalSyntax.SyntaxToken(syntaxKind, _stringBuilder.ToString());
                }
                else if (char.IsDigit(ch))
                {
                    while (PeekChar(out ch) && char.IsDigit(ch))
                    {
                        ReadChar();
                        _stringBuilder.Append(ch);
                    }

                    return new InternalSyntax.SyntaxToken(SyntaxKind.NumericLiteralToken, _stringBuilder.ToString());
                }
            }
            else
            {
                string chStr = string.Intern(ch.ToString());

                switch (ch)
                {
                    case '+':
                        return new InternalSyntax.SyntaxToken(SyntaxKind.PlusToken, chStr);

                    case '-':
                        return new InternalSyntax.SyntaxToken(SyntaxKind.MinusToken, chStr);

                    case '/':
                        return new InternalSyntax.SyntaxToken(SyntaxKind.SlashToken, chStr);

                    case '*':
                        return new InternalSyntax.SyntaxToken(SyntaxKind.StarToken, chStr);

                    case '%':
                        return new InternalSyntax.SyntaxToken(SyntaxKind.PercentToken, chStr);

                    case '.':
                        return new InternalSyntax.SyntaxToken(SyntaxKind.DotToken, chStr);

                    case ',':
                        return new InternalSyntax.SyntaxToken(SyntaxKind.CommaToken, chStr);

                    case ':':
                        return new InternalSyntax.SyntaxToken(SyntaxKind.ColonToken, chStr);

                    case ';':
                        return new InternalSyntax.SyntaxToken(SyntaxKind.SemicolonToken, chStr);

                    /*
                    
                   case '"':
                       return new SyntaxToken(SyntaxKind.DoublequoteToken, chStr);

                   case '\'':
                       return new SyntaxToken(SyntaxKind.SinglequoteToken, chStr);

                   case '`':
                       return new SyntaxToken(SyntaxKind.BackquoteToken, chStr);

                   */

                    case '(':
                        return new InternalSyntax.SyntaxToken(SyntaxKind.OpenParenToken, chStr);

                    case ')':
                        return new InternalSyntax.SyntaxToken(SyntaxKind.CloseParenToken, chStr);

                    case '{':
                        return new InternalSyntax.SyntaxToken(SyntaxKind.OpenBraceToken, chStr);

                    case '}':
                        return new InternalSyntax.SyntaxToken(SyntaxKind.CloseBraceToken, chStr);

                    /*

                case '[':
                    return new SyntaxToken(SyntaxKind.OpenSquareToken, chStr);

                case ']':
                    return new SyntaxToken(SyntaxKind.CloseSquareToken, chStr);
                    */

                    case '=':
                        return new InternalSyntax.SyntaxToken(SyntaxKind.EqualsToken, chStr);

                    case '!':
                        if (PeekChar(out var ch6) && ch == '=')
                        {
                            ReadChar();
                            return new InternalSyntax.SyntaxToken(SyntaxKind.NotEqualsToken, "!=");
                        }
                        return new InternalSyntax.SyntaxToken(SyntaxKind.ExclamationToken, chStr);

                    case '<':
                        if (PeekChar(out var ch2) && ch == '=')
                        {
                            ReadChar();
                            return new InternalSyntax.SyntaxToken(SyntaxKind.LessThanEqualsToken, "<=");
                        }
                        return new InternalSyntax.SyntaxToken(SyntaxKind.LessThanToken, chStr);

                    case '>':
                        if (PeekChar(out var ch3) && ch == '=')
                        {
                            ReadChar();
                            return new InternalSyntax.SyntaxToken(SyntaxKind.GreaterOrEqualsToken, ">=");
                        }
                        return new InternalSyntax.SyntaxToken(SyntaxKind.GreaterThanToken, chStr);


                    case ' ':
                        int length = 1;

                        while (ReadWhile(' '))
                        {
                            length++;
                        }

                        return new InternalSyntax.SyntaxToken(SyntaxKind.Whitespace, string.Intern(new string(' ', length)));


                    case '\'':
                        _stringBuilder = new StringBuilder();

                        _stringBuilder.Append(ch);

                        while (PeekChar(out var ch9) && !IsEndOfLine)
                        {
                            _stringBuilder.Append(ch9);
                            ReadChar();

                            if (PeekChar(out ch9) && ch9 == '\'')
                            {
                                ReadChar();
                                _stringBuilder.Append(ch9);
                                break;
                            }
                        }

                        return new InternalSyntax.SyntaxToken(SyntaxKind.CharacterLiteralToken, _stringBuilder.ToString());


                    case '\"':
                        _stringBuilder = new StringBuilder();

                        _stringBuilder.Append(ch);

                        while (PeekChar(out var ch8) && ch8 != '\"' && !IsEndOfLine)
                        {
                            _stringBuilder.Append(ch8);
                            ReadChar();

                            if (PeekChar(out ch8) && ch8 == '\"')
                            {
                                ReadChar();
                                _stringBuilder.Append(ch8);
                                break;
                            }
                        }

                        return new InternalSyntax.SyntaxToken(SyntaxKind.StringLiteralToken, _stringBuilder.ToString());


                    case '\t':
                        return new InternalSyntax.SyntaxToken(SyntaxKind.TabToken, "\t");

                    case '\n':
                        return new InternalSyntax.SyntaxToken(SyntaxKind.EndOfLineToken, "\n");

                    case '\r':
                        return new InternalSyntax.SyntaxToken(SyntaxKind.CarriageReturnToken, "\r");
                }
            }

            return new InternalSyntax.SyntaxToken(SyntaxKind.None, ch.ToString());
        }

        return new InternalSyntax.SyntaxToken(SyntaxKind.EndOfFileToken, string.Empty);
    }

    private void ReadChar()
    {
        _textReader.Read();
    }


    private bool ReadChar(out char ch)
    {
        var value = _textReader.Read();
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

    public bool IsEndOfLine => _textReader.Peek() == -1;
}