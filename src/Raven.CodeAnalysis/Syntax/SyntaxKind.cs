namespace Raven.CodeAnalysis.Syntax;

public enum SyntaxKind
{
    None = 0,

    // === List & Root ===
    List = 1,
    CompilationUnit = 2,

    // === Names & Types ===
    IdentifierName = 10,
    GenericName = 11,
    QualifiedName = 12,
    AliasQualifiedName = 13,
    NullableType = 14,
    UnionType = 15,
    PredefinedType = 16,
    TypeArgumentList = 17,
    TypeArgument = 18,

    // === Declarations ===
    NamespaceDeclaration = 30,
    FileScopedNamespaceDeclaration = 31,
    ImportDirective = 32,
    EnumDeclaration = 33,
    EnumMemberDeclaration = 34,
    MethodDeclaration = 35,
    ReturnTypeAnnotation = 36,
    ParameterList = 37,
    Parameter = 38,
    LocalDeclaration = 39,
    VariableDeclaration = 40,
    VariableDeclarator = 41,
    TypeAnnotation = 42,

    // === Statements ===
    GlobalStatement = 50,
    ExpressionStatement = 51,
    ReturnStatement = 52,
    LocalFunctionStatement = 53,
    EmptyStatement = 54,

    // === Control Flow Constructs ===
    IfExpression = 60,
    ElseClause = 61,
    WhileExpression = 62,
    Block = 63,

    // === Core Expressions ===
    ObjectCreationExpression = 70,
    CollectionExpression = 71,
    CollectionElement = 72,
    ParenthesizedExpression = 73,
    UnaryExpression = 74,
    BinaryExpression = 75,
    SimpleAssignmentExpression = 76,
    InvocationExpression = 77,
    SimpleMemberAccessExpression = 78,
    ElementAccessExpression = 79,

    // === Lambda ===
    SimpleLambdaExpression = 90,
    ParenthesizedLambdaExpression = 91,

    // === Pattern Matching ===
    IsPatternExpression = 100,
    DeclarationPattern = 101,
    NotPattern = 102,
    AndPattern = 103,
    OrPattern = 104,
    SingleVariableDesignation = 105,

    // === Argument Lists ===
    ArgumentList = 110,
    Argument = 111,
    BracketedArgumentList = 112,

    // === Literal Expressions ===
    NumericLiteralExpression = 120,
    TrueLiteralExpression = 121,
    FalseLiteralExpression = 122,
    CharacterLiteralExpression = 123,
    StringLiteralExpression = 124,
    NullLiteralExpression = 125,

    // === Detailed Expression Kinds ===
    // Arithmetic
    AddExpression = 130,
    SubtractExpression = 131,
    MultiplyExpression = 132,
    DivideExpression = 133,
    ModuloExpression = 134,
    PowerExpression = 135,

    // Comparison
    EqualsExpression = 140,
    NotEqualsExpression = 141,
    LessThanExpression = 142,
    GreaterThanExpression = 143,
    LessThanOrEqualExpression = 144,
    GreaterThanOrEqualExpression = 145,

    // Logical
    LogicalAndExpression = 150,
    LogicalOrExpression = 151,
    LogicalNotExpression = 152,
    UnaryPlusExpression = 153,
    UnaryMinusExpression = 154,
    AddressOfExpression = 155,
    EqualsValueClause = 156,


    // === Tokens — Operators, Keywords, Identifiers ===

    ArrowToken = 160,

    // Identifiers & Literals
    IdentifierToken = 170,
    NumericLiteralToken = 171,
    CharacterLiteralToken = 172,
    StringLiteralToken = 173,

    // === Keywords (must match 57–82) ===
    VoidKeyword = 57,
    NullKeyword = 58,
    IntKeyword = 59,
    StringKeyword = 60,
    BoolKeyword = 61,
    CharKeyword = 62,
    ImportKeyword = 63,
    NamespaceKeyword = 64,
    LetKeyword = 65,
    VarKeyword = 66,
    IfKeyword = 67,
    ElseKeyword = 68,
    WhileKeyword = 69,
    ReturnKeyword = 70,
    NewKeyword = 71,
    TrueKeyword = 72,
    FalseKeyword = 73,
    IsKeyword = 74,
    NotKeyword = 75,
    AndKeyword = 76,
    OrKeyword = 77,
    FuncKeyword = 78,
    EnumKeyword = 79,

    // Modifier Keywords (add near other keywords and update IsKeywordKind accordingly)
    RefKeyword = 80,
    OutKeyword = 81,
    InKeyword = 82,

    // Punctuation & Operators
    OrToken = 180,
    AndToken = 181,
    LessThanToken = 182,
    GreaterThanToken = 183,
    GreaterOrEqualsToken = 184,
    LessThanEqualsToken = 185,

    OpenParenToken = 186,
    CloseParenToken = 187,
    OpenBraceToken = 188,
    CloseBraceToken = 189,
    OpenBracketToken = 190,
    CloseBracketToken = 191,

    CommaToken = 192,
    EqualsToken = 193,
    EqualsEqualsToken = 194,
    NotEqualsToken = 195,

    PlusToken = 196,
    MinusToken = 197,
    PercentToken = 198,
    SlashToken = 199,
    StarToken = 200,

    DotToken = 201,
    CaretToken = 202,
    BarToken = 203,
    ExclamationToken = 204,
    QuestionToken = 205,
    AmpersandToken = 206,

    ColonToken = 207,
    SemicolonToken = 208,

    // === Trivia ===
    WhitespaceTrivia = 220,
    TabTrivia = 221,
    LineFeedTrivia = 222,
    CarriageReturnTrivia = 223,
    CarriageReturnLineFeedTrivia = 224,
    EndOfLineTrivia = 225,
    SingleLineCommentTrivia = 226,
    MultiLineCommentTrivia = 227,
    SkippedTokensTrivia = 228,

    // Line Tokens
    Whitespace = 240,
    TabToken = 241,
    LineFeedToken = 242,
    CarriageReturnToken = 243,
    CarriageReturnLineFeedToken = 244,
    NewLineToken = 245,

    EndOfFileToken = 999
}