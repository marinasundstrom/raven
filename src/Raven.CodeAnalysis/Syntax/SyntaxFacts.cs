namespace Raven.CodeAnalysis.Syntax;

public static class SyntaxFacts
{
    private static readonly IDictionary<string, SyntaxKind> _keywordStrings = new Dictionary<string, SyntaxKind>
    {
        { "and", SyntaxKind.AndKeyword },
        { "bool", SyntaxKind.BoolKeyword },
        { "char", SyntaxKind.CharKeyword },
        { "else", SyntaxKind.ElseKeyword },
        { "enum", SyntaxKind.EnumKeyword },
        { "false", SyntaxKind.FalseKeyword },
        { "func", SyntaxKind.FuncKeyword },
        { "if", SyntaxKind.IfKeyword },
        { "import", SyntaxKind.ImportKeyword },
        { "in", SyntaxKind.InKeyword },
        { "int", SyntaxKind.IntKeyword },
        { "is", SyntaxKind.IsKeyword },
        { "let", SyntaxKind.LetKeyword },
        { "namespace", SyntaxKind.NamespaceKeyword },
        { "new", SyntaxKind.NewKeyword },
        { "not", SyntaxKind.NotKeyword },
        { "null", SyntaxKind.NullKeyword },
        { "or", SyntaxKind.OrKeyword },
        { "out", SyntaxKind.OutKeyword },
        { "ref", SyntaxKind.RefKeyword },
        { "return", SyntaxKind.ReturnKeyword },
        { "string", SyntaxKind.StringKeyword },
        { "true", SyntaxKind.TrueKeyword },
        { "var", SyntaxKind.VarKeyword },
        { "void", SyntaxKind.VoidKeyword },
        { "while", SyntaxKind.WhileKeyword },
        { "struct", SyntaxKind.StructKeyword },
        { "class", SyntaxKind.ClassKeyword },
        { "self", SyntaxKind.SelfKeyword },
        { "prop", SyntaxKind.PropKeyword },
        { "init", SyntaxKind.InitKeyword },
        { "public", SyntaxKind.PublicKeyword },
        { "private", SyntaxKind.PrivateKeyword },
        { "internal", SyntaxKind.InternalKeyword },
        { "protected", SyntaxKind.ProtectedKeyword },
        { "static", SyntaxKind.StaticKeyword },
        { "abstract", SyntaxKind.AbstractKeyword },
        { "sealed", SyntaxKind.SealedKeyword },
        { "override", SyntaxKind.OverrideKeyword },
        { "get", SyntaxKind.GetKeyword },
        { "set", SyntaxKind.SetKeyword }
    };

    private static readonly HashSet<SyntaxKind> _keywordKinds =
    [.. _keywordStrings.Values];

    public static bool IsKeywordKind(SyntaxKind kind)
    {
        return _keywordKinds.Contains(kind);
    }

    public static bool ParseReservedWord(string text, out SyntaxKind kind)
    {
        return _keywordStrings.TryGetValue(text, out kind);
    }

    public static string? GetSyntaxTokenText(this SyntaxKind kind)
    {
        return kind switch
        {
            // === Trivia ===
            SyntaxKind.WhitespaceTrivia => " ",
            SyntaxKind.LineFeedToken => "\n",
            SyntaxKind.CarriageReturnToken => "\r",
            SyntaxKind.CarriageReturnLineFeedToken => "\r\n",
            SyntaxKind.NewLineToken => "\n", // alternate alias
            SyntaxKind.TabToken => "\t",

            // === Grouping & Delimiters ===
            SyntaxKind.OpenParenToken => "(",
            SyntaxKind.CloseParenToken => ")",
            SyntaxKind.OpenBraceToken => "{",
            SyntaxKind.CloseBraceToken => "}",
            SyntaxKind.OpenBracketToken => "[",
            SyntaxKind.CloseBracketToken => "]",

            SyntaxKind.CommaToken => ",",
            SyntaxKind.DotToken => ".",
            SyntaxKind.SemicolonToken => ";",
            SyntaxKind.ColonToken => ":",
            SyntaxKind.QuestionToken => "?",
            SyntaxKind.ArrowToken => "=>",

            // === Operators ===
            SyntaxKind.EqualsToken => "=",
            SyntaxKind.EqualsEqualsToken => "==",
            SyntaxKind.NotEqualsToken => "!=",
            SyntaxKind.LessThanToken => "<",
            SyntaxKind.GreaterThanToken => ">",
            SyntaxKind.LessThanOrEqualsToken => "<=",
            SyntaxKind.GreaterThanOrEqualsToken => ">=",

            SyntaxKind.PlusToken => "+",
            SyntaxKind.MinusToken => "-",
            SyntaxKind.StarToken => "*",
            SyntaxKind.SlashToken => "/",
            SyntaxKind.PercentToken => "%",
            SyntaxKind.CaretToken => "^",
            SyntaxKind.AmpersandToken => "&",
            SyntaxKind.BarToken => "|",
            SyntaxKind.ExclamationToken => "!",

            // === Keywords ===
            SyntaxKind.VoidKeyword => "void",
            SyntaxKind.NullKeyword => "null",
            SyntaxKind.IntKeyword => "int",
            SyntaxKind.StringKeyword => "string",
            SyntaxKind.BoolKeyword => "bool",
            SyntaxKind.CharKeyword => "char",

            SyntaxKind.ImportKeyword => "import",
            SyntaxKind.NamespaceKeyword => "namespace",
            SyntaxKind.LetKeyword => "let",
            SyntaxKind.VarKeyword => "var",

            SyntaxKind.IfKeyword => "if",
            SyntaxKind.ElseKeyword => "else",
            SyntaxKind.WhileKeyword => "while",
            SyntaxKind.ReturnKeyword => "return",
            SyntaxKind.NewKeyword => "new",
            SyntaxKind.TrueKeyword => "true",
            SyntaxKind.FalseKeyword => "false",

            SyntaxKind.IsKeyword => "is",
            SyntaxKind.NotKeyword => "not",
            SyntaxKind.AndKeyword => "and",
            SyntaxKind.OrKeyword => "or",

            SyntaxKind.RefKeyword => "ref",
            SyntaxKind.OutKeyword => "out",
            SyntaxKind.InKeyword => "in",

            SyntaxKind.FuncKeyword => "func",
            SyntaxKind.EnumKeyword => "enum",
            SyntaxKind.StructKeyword => "struct",
            SyntaxKind.ClassKeyword => "class",
            SyntaxKind.SelfKeyword => "self",
            SyntaxKind.PropKeyword => "prop",
            SyntaxKind.InitKeyword => "init",

            SyntaxKind.GetKeyword => "get",
            SyntaxKind.SetKeyword => "set",

            _ => throw new Exception("Not a valid token kind")
        };
    }

    public static bool TryResolveOperatorPrecedence(SyntaxKind kind, out int precedence)
    {
        switch (kind)
        {
            case SyntaxKind.StarToken:
            case SyntaxKind.SlashToken:
            case SyntaxKind.PercentToken:
                precedence = 5;
                return true;

            case SyntaxKind.PlusToken:
            case SyntaxKind.MinusToken:
                precedence = 4;
                return true;

            case SyntaxKind.LessThanToken:
            case SyntaxKind.LessThanOrEqualsToken:
            case SyntaxKind.GreaterThanToken:
            case SyntaxKind.GreaterThanOrEqualsToken:
            case SyntaxKind.EqualsEqualsToken:
            case SyntaxKind.NotEqualsToken:
                precedence = 3;
                return true;

            case SyntaxKind.AndToken:
                precedence = 2;
                return true;

            case SyntaxKind.OrToken:
                precedence = 1;
                return true;

            default:
                precedence = -1;
                return false;
        }
    }

    public static bool IsExpression(SyntaxKind kind)
    {
        switch (kind)
        {
            case SyntaxKind.NumericLiteralExpression:
            case SyntaxKind.TrueLiteralExpression:
            case SyntaxKind.FalseLiteralExpression:
            case SyntaxKind.CharacterLiteralExpression:
            case SyntaxKind.StringLiteralExpression:
            case SyntaxKind.NullLiteralExpression:

            case SyntaxKind.IdentifierName:
            case SyntaxKind.QualifiedName:
            case SyntaxKind.GenericName:
            case SyntaxKind.AliasQualifiedName:
            case SyntaxKind.NullableType:
            case SyntaxKind.UnionType:

            case SyntaxKind.ObjectCreationExpression:
            case SyntaxKind.CollectionExpression:
            case SyntaxKind.ParenthesizedExpression:
            case SyntaxKind.SimpleMemberAccessExpression:
            case SyntaxKind.ElementAccessExpression:
            case SyntaxKind.InvocationExpression:

            case SyntaxKind.UnaryExpression:
            case SyntaxKind.UnaryPlusExpression:
            case SyntaxKind.UnaryMinusExpression:
            case SyntaxKind.AddressOfExpression:

            case SyntaxKind.BinaryExpression:
            case SyntaxKind.AddExpression:
            case SyntaxKind.SubtractExpression:
            case SyntaxKind.MultiplyExpression:
            case SyntaxKind.DivideExpression:
            case SyntaxKind.ModuloExpression:
            case SyntaxKind.PowerExpression:
            case SyntaxKind.EqualsExpression:
            case SyntaxKind.NotEqualsExpression:
            case SyntaxKind.LessThanExpression:
            case SyntaxKind.GreaterThanExpression:
            case SyntaxKind.LessThanOrEqualsExpression:
            case SyntaxKind.GreaterThanOrEqualsExpression:
            case SyntaxKind.LogicalAndExpression:
            case SyntaxKind.LogicalOrExpression:
            case SyntaxKind.LogicalNotExpression:

            case SyntaxKind.SimpleAssignmentExpression:

            case SyntaxKind.IfExpression:
            case SyntaxKind.WhileExpression:

            case SyntaxKind.IsPatternExpression:
            case SyntaxKind.SimpleLambdaExpression:
            case SyntaxKind.ParenthesizedLambdaExpression:
                return true;

            default:
                return false;
        }
    }

    public static bool IsStatement(SyntaxKind kind)
    {
        switch (kind)
        {
            case SyntaxKind.Block:
            case SyntaxKind.ExpressionStatement:
            case SyntaxKind.ReturnStatement:
            case SyntaxKind.LocalDeclaration:
            case SyntaxKind.EmptyStatement:
            case SyntaxKind.GlobalStatement:
            case SyntaxKind.LocalFunctionStatement:
                return true;

            default:
                return false;
        }
    }

    public static bool IsLiteralExpression(SyntaxKind kind) => kind is
        SyntaxKind.NumericLiteralExpression or
        SyntaxKind.TrueLiteralExpression or
        SyntaxKind.FalseLiteralExpression or
        SyntaxKind.CharacterLiteralExpression or
        SyntaxKind.StringLiteralExpression or
        SyntaxKind.NullLiteralExpression;

    public static bool IsAssignmentExpression(SyntaxKind kind)
    {
        return kind == SyntaxKind.SimpleAssignmentExpression;
    }

    public static bool IsTypeSyntax(SyntaxKind kind)
    {
        return kind switch
        {
            SyntaxKind.PredefinedType => true,
            SyntaxKind.IdentifierName => true,
            SyntaxKind.GenericName => true,
            SyntaxKind.QualifiedName => true,
            SyntaxKind.AliasQualifiedName => true,
            SyntaxKind.NullableType => true,
            SyntaxKind.UnionType => true,
            _ => false
        };
    }

    public static bool IsUnaryOperatorToken(SyntaxKind kind)
    {
        return kind switch
        {
            SyntaxKind.PlusToken => true,        // Unary +
            SyntaxKind.MinusToken => true,       // Unary -
            SyntaxKind.ExclamationToken => true, // Logical not
            SyntaxKind.AmpersandToken => true,   // Address-of
            _ => false
        };
    }
}