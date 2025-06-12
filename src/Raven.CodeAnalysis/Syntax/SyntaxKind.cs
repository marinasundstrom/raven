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
    SimpleLambdaExpression = 80,
    ParenthesizedLambdaExpression = 81,

    // === Pattern Matching ===
    IsPatternExpression = 90,
    DeclarationPattern = 91,
    NotPattern = 92,
    AndPattern = 93,
    OrPattern = 94,
    SingleVariableDesignation = 95,

    // === Argument Lists ===
    ArgumentList = 100,
    Argument = 101,
    BracketedArgumentList = 102,

    // === Literal Expressions ===
    NumericLiteralExpression = 110,
    TrueLiteralExpression = 111,
    FalseLiteralExpression = 112,
    CharacterLiteralExpression = 113,
    StringLiteralExpression = 114,
    NullLiteralExpression = 115,

    // === Detailed Expression Kinds ===
    // Arithmetic
    AddExpression = 120,
    SubtractExpression = 121,
    MultiplyExpression = 122,
    DivideExpression = 123,
    ModuloExpression = 124,
    PowerExpression = 125,

    // Comparison
    EqualsExpression = 130,
    NotEqualsExpression = 131,
    LessThanExpression = 132,
    GreaterThanExpression = 133,
    LessThanOrEqualExpression = 134,
    GreaterThanOrEqualExpression = 135,

    // Logical
    LogicalAndExpression = 140,
    LogicalOrExpression = 141,
    LogicalNotExpression = 142,
    UnaryPlusExpression = 143,
    UnaryMinusExpression = 144,
    AddressOfExpression = 145,
    EqualsValueClause = 146,

    // === Tokens — Operators, Keywords, Identifiers ===

    ArrowToken = 160,

    // Identifiers & Literals
    IdentifierToken = 170,
    NumericLiteralToken = 171,
    CharacterLiteralToken = 172,
    StringLiteralToken = 173,

    // Keywords — moved to separate block to avoid overlap
    VoidKeyword = 300,
    NullKeyword = 301,
    IntKeyword = 302,
    StringKeyword = 303,
    BoolKeyword = 304,
    CharKeyword = 305,
    ImportKeyword = 306,
    NamespaceKeyword = 307,
    LetKeyword = 308,
    VarKeyword = 309,
    IfKeyword = 310,
    ElseKeyword = 311,
    WhileKeyword = 312,
    ReturnKeyword = 313,
    NewKeyword = 314,
    TrueKeyword = 315,
    FalseKeyword = 316,
    IsKeyword = 317,
    NotKeyword = 318,
    AndKeyword = 319,
    OrKeyword = 320,
    FuncKeyword = 321,
    EnumKeyword = 322,
    RefKeyword = 323,
    OutKeyword = 324,
    InKeyword = 325,

    // Punctuation & Operators
    OrToken = 340,
    AndToken = 341,
    LessThanToken = 342,
    GreaterThanToken = 343,
    GreaterOrEqualsToken = 344,
    LessThanEqualsToken = 345,

    OpenParenToken = 346,
    CloseParenToken = 347,
    OpenBraceToken = 348,
    CloseBraceToken = 349,
    OpenBracketToken = 350,
    CloseBracketToken = 351,

    CommaToken = 352,
    EqualsToken = 353,
    EqualsEqualsToken = 354,
    NotEqualsToken = 355,

    PlusToken = 356,
    MinusToken = 357,
    PercentToken = 358,
    SlashToken = 359,
    StarToken = 360,

    DotToken = 361,
    CaretToken = 362,
    BarToken = 363,
    ExclamationToken = 364,
    QuestionToken = 365,
    AmpersandToken = 366,

    ColonToken = 367,
    SemicolonToken = 368,

    // === Trivia ===
    WhitespaceTrivia = 400,
    TabTrivia = 401,
    LineFeedTrivia = 402,
    CarriageReturnTrivia = 403,
    CarriageReturnLineFeedTrivia = 404,
    EndOfLineTrivia = 405,
    SingleLineCommentTrivia = 406,
    MultiLineCommentTrivia = 407,
    SkippedTokensTrivia = 408,

    // Line Tokens
    Whitespace = 420,
    TabToken = 421,
    LineFeedToken = 422,
    CarriageReturnToken = 423,
    CarriageReturnLineFeedToken = 424,
    NewLineToken = 425,

    EndOfFileToken = 999
}