namespace Raven.CodeAnalysis.Syntax;

public static class SyntaxFacts
{
    private static readonly IDictionary<string, SyntaxKind> _keywordStrings = new Dictionary<string, SyntaxKind> {
        { "void", SyntaxKind.VoidKeyword },
        { "int", SyntaxKind.IntKeyword },
        { "string", SyntaxKind.StringKeyword },
        { "bool", SyntaxKind.BoolKeyword },
        { "char", SyntaxKind.CharKeyword },
        { "import", SyntaxKind.ImportKeyword },
        { "namespace", SyntaxKind.NamespaceKeyword },
        { "let", SyntaxKind.LetKeyword },
        { "if", SyntaxKind.IfKeyword },
        { "else", SyntaxKind.ElseKeyword },
        { "return", SyntaxKind.ReturnKeyword },
        { "true", SyntaxKind.TrueKeyword },
        { "false", SyntaxKind.FalseKeyword },
        //{ "is", SyntaxKind.IsKeyword },
        { "not", SyntaxKind.NotKeyword },
        //{ "and", SyntaxKind.AndKeyword },
        //{ "or", SyntaxKind.OrKeyword },
    };

    public static string? GetSyntaxTokenText(this SyntaxKind kind)
    {
        return kind switch
        {
            SyntaxKind.WhitespaceTrivia => " ",
            SyntaxKind.EndOfLineToken => "\n",
            SyntaxKind.CarriageReturnToken => "\r",
            SyntaxKind.OpenParenToken => "(",
            SyntaxKind.CloseParenToken => ")",
            SyntaxKind.OpenBraceToken => "{",
            SyntaxKind.CloseBraceToken => "}",
            //SyntaxKind.OpenSquareToken => "[",
            //SyntaxKind.CloseSquareToken => "]",
            // SyntaxKind.OpenAngleToken => "<",
            // SyntaxKind.CloseAngleToken => ">",
            // SyntaxKind.AssignToken => "=",
            SyntaxKind.EqualsToken => "=",
            /*SyntaxKind.NotEqualsToken => "!=",
            SyntaxKind.GreaterEqualsToken => ">=",
            SyntaxKind.LessEqualsToken => "<=",
            SyntaxKind.DotToken => ".",*/
            SyntaxKind.PlusToken => "+",
            /*SyntaxKind.DashToken => "-",
            SyntaxKind.SlashToken => "/",
            SyntaxKind.BackslashToken => "\\",
            SyntaxKind.StarToken => "*",
            SyntaxKind.PercentToken => "%", */
            SyntaxKind.ColonToken => ":",
            SyntaxKind.SemicolonToken => ";",
            /*
            SyntaxKind.DoublequoteToken => "\"",
            SyntaxKind.SinglequoteToken => "'",
            SyntaxKind.BackquoteToken => "`", */
            SyntaxKind.IfKeyword => "if",
            //SyntaxKind.VarKeyword => "var",
            SyntaxKind.ElseKeyword => "else ",
            //SyntaxKind.Var" => "var",
            SyntaxKind.LetKeyword => "let",
            _ => null
        };
    }

    /*
        public static int GetSyntaxLength(this SyntaxKind kind)
        {
            switch (kind)
            {
                case SyntaxKind.WhitespaceTrivia:
                case SyntaxKind.NewlineTrivia:
                    return 1;

                case SyntaxKind.OpeningParenToken:
                case SyntaxKind.ClosingParenToken:
                case SyntaxKind.OpenBraceToken:
                case SyntaxKind.CloseBraceToken:
                case SyntaxKind.OpenSquareToken:
                case SyntaxKind.CloseSquareToken:
                case SyntaxKind.OpenAngleToken:
                case SyntaxKind.CloseAngleToken:
                    return 1;

                case SyntaxKind.AssignToken:
                    return 1;

                case SyntaxKind.EqualsToken:
                case SyntaxKind.NotEqualsToken:
                case SyntaxKind.GreaterEqualsToken:
                case SyntaxKind.LessEqualsToken:
                    return 2;

                case SyntaxKind.DotToken:
                    return 1;

                case SyntaxKind.PlusToken:
                case SyntaxKind.DashToken:
                case SyntaxKind.SlashToken:
                case SyntaxKind.BackslashToken:
                case SyntaxKind.StarToken:
                case SyntaxKind.PercentToken:
                case SyntaxKind.ColonToken:
                case SyntaxKind.SemicolonToken:
                case SyntaxKind.DoublequoteToken:
                case SyntaxKind.SinglequoteToken:
                case SyntaxKind.BackquoteToken:
                    return 1;

                case SyntaxKind.VarToken:
                case SyntaxKind.:
                    return 3;
                case SyntaxKind.IfToken:
                    return 2;
                case SyntaxKind.ElseToken:
                    return 4;
            }

            return -1;
        }

        */

    public static bool ParseReservedWord(string text, out SyntaxKind syntaxKind)
    {
        if (_keywordStrings.TryGetValue(text, out syntaxKind))
        {
            return true;
        }

        return false;
    }

    public static bool TryResolveOperatorPrecedence(SyntaxKind kind, out int precedence)
    {
        switch (kind)
        {
            case SyntaxKind.PercentToken:
            case SyntaxKind.StarToken:
            case SyntaxKind.SlashToken:
                precedence = 5;
                break;

            case SyntaxKind.PlusToken:
            case SyntaxKind.MinusToken:
                //case SyntaxKind.DashToken:
                precedence = 4;
                break;

            /*
            case SyntaxKind.EqualsEqualsToken:
            case SyntaxKind.BangEqualsToken: */
            case SyntaxKind.LessThanToken:
            case SyntaxKind.LessThanEqualsToken:
            case SyntaxKind.GreaterThanToken:
            case SyntaxKind.GreaterOrEqualsToken:
                precedence = 3;
                break;

            /*
            case SyntaxKind.AmpersandToken:
            case SyntaxKind.AmpersandAmpersandToken:
                return 2;

            case SyntaxKind.PipeToken:
            case SyntaxKind.PipePipeToken:
            case SyntaxKind.HatToken:
                return 1;
                */

            default:
                precedence = -1;
                return false;
        }

        return true;
    }

    public static bool IsKeywordKind(SyntaxKind kind)
    {
        return kind is > (SyntaxKind)56 and < (SyntaxKind)71;
    }
}