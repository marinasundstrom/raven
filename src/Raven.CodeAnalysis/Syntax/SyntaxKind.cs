namespace Raven.CodeAnalysis.Syntax;

public enum SyntaxKind
{
    None = 0,

    // === Root & List ===
    List = 1,
    CompilationUnit = 2,

    // === Names & Types === (10–49)
    IdentifierName = 10,
    GenericName = 11,
    QualifiedName = 12,
    AliasQualifiedName = 13,

    PredefinedType = 20,
    NullableType = 21,
    UnionType = 22,

    TypeAnnotationClause = 30,
    TypeArgumentList = 31,
    TypeArgument = 32,

    // === Declarations === (50–99)
    NamespaceDeclaration = 50,
    FileScopedNamespaceDeclaration = 51,
    ImportDirective = 52,

    EnumDeclaration = 60,
    EnumMemberDeclaration = 61,

    ClassDeclaration = 70,
    StructDeclaration = 71,
    PropertyDeclaration = 72,
    FieldDeclaration = 73,
    InitializerDeclaration = 74,

    MethodDeclaration = 80,
    ArrowTypeClause = 81,
    ParameterList = 82,
    Parameter = 83,
    BracketedParameterList = 84,

    LocalDeclaration = 90,
    VariableDeclaration = 91,
    VariableDeclarator = 92,

    AccessorList = 93,
    GetAccessorDeclaration = 94,
    SetAccessorDeclaration = 95,
    ConstructorDeclaration = 96,
    NamedConstructorDeclaration = 97,

    IndexerDeclaration = 98,


    // === Statements === (100–129)
    GlobalStatement = 100,
    LocalFunctionStatement = 101,
    ExpressionStatement = 102,
    ReturnStatement = 103,
    EmptyStatement = 104,
    Block = 105,
    LocalDeclarationStatement = 106,

    // === Control Flow === (130–159)
    IfExpression = 130,
    ElseClause = 131,
    WhileExpression = 132,

    // === Expressions === (160–199)
    ObjectCreationExpression = 160,
    CollectionExpression = 161,
    CollectionElement = 162,
    ParenthesizedExpression = 163,
    TupleExpression = 164,

    UnaryExpression = 170,
    BinaryExpression = 171,
    SimpleAssignmentExpression = 172,
    InvocationExpression = 173,
    SimpleMemberAccessExpression = 174,
    ElementAccessExpression = 175,
    EqualsValueClause = 176,
    AddressOfExpression = 177,
    NameColon = 178,
    SelfExpression = 179,
    ArrowExpressionClause = 180,

    // === Lambda === (200–209)
    SimpleLambdaExpression = 200,
    ParenthesizedLambdaExpression = 201,

    // === Pattern Matching === (210–229)
    IsPatternExpression = 210,
    DeclarationPattern = 211,
    NotPattern = 212,
    AndPattern = 213,
    OrPattern = 214,
    SingleVariableDesignation = 215,

    // === Argument Lists === (230–239)
    ArgumentList = 230,
    Argument = 231,
    BracketedArgumentList = 232,

    // === Literals === (240–259)
    NumericLiteralExpression = 240,
    TrueLiteralExpression = 241,
    FalseLiteralExpression = 242,
    CharacterLiteralExpression = 243,
    StringLiteralExpression = 244,
    NullLiteralExpression = 245,

    // === Detailed Expressions === (260–299)

    // Arithmetic
    AddExpression = 260,
    SubtractExpression = 261,
    MultiplyExpression = 262,
    DivideExpression = 263,
    ModuloExpression = 264,
    PowerExpression = 265,

    // Comparison
    EqualsExpression = 270,
    NotEqualsExpression = 271,
    LessThanExpression = 272,
    GreaterThanExpression = 273,
    LessThanOrEqualsExpression = 274,
    GreaterThanOrEqualsExpression = 275,

    // Logical
    LogicalAndExpression = 280,
    LogicalOrExpression = 281,
    LogicalNotExpression = 282,
    UnaryPlusExpression = 283,
    UnaryMinusExpression = 284,

    // === Tokens === (1000–1099)

    ArrowToken = 1000,
    FatArrowToken = 1001,

    // Identifiers & Literals
    IdentifierToken = 1010,
    NumericLiteralToken = 1011,
    CharacterLiteralToken = 1012,
    StringLiteralToken = 1013,

    // === Keywords === (1100–1199)

    // Types & Literals
    VoidKeyword = 1100,
    NullKeyword = 1101,
    IntKeyword = 1102,
    StringKeyword = 1103,
    BoolKeyword = 1104,
    CharKeyword = 1105,

    // Declarations
    ImportKeyword = 1110,
    NamespaceKeyword = 1111,
    LetKeyword = 1112,
    VarKeyword = 1113,
    FuncKeyword = 1114,
    EnumKeyword = 1115,
    StructKeyword = 1116,
    ClassKeyword = 1117,
    InitKeyword = 1119,
    SelfKeyword = 1120,

    // Control Flow
    IfKeyword = 1130,
    ElseKeyword = 1131,
    WhileKeyword = 1132,
    ReturnKeyword = 1133,
    NewKeyword = 1134,
    TrueKeyword = 1135,
    FalseKeyword = 1136,

    // Pattern/Modifiers
    IsKeyword = 1140,
    NotKeyword = 1141,
    AndKeyword = 1142,
    OrKeyword = 1143,
    RefKeyword = 1144,
    OutKeyword = 1145,
    InKeyword = 1146,
    PublicKeyword = 1147,
    PrivateKeyword = 1148,
    InternalKeyword = 1149,
    ProtectedKeyword = 1150,
    StaticKeyword = 1151,
    AbstractKeyword = 1152,
    SealedKeyword = 1153,
    OverrideKeyword = 1154,

    GetKeyword = 1155,
    SetKeyword = 1156,

    // === Punctuation === (1200–1299)

    // Binary
    PlusToken = 1200,
    MinusToken = 1201,
    PercentToken = 1202,
    SlashToken = 1203,
    StarToken = 1204,
    CaretToken = 1205,

    // Logical & Bitwise
    OrToken = 1210,
    AndToken = 1211,
    BarToken = 1212,
    AmpersandToken = 1213,

    // Comparison
    LessThanToken = 1220,
    GreaterThanToken = 1221,
    GreaterThanOrEqualsToken = 1222,
    LessThanOrEqualsToken = 1223,
    EqualsToken = 1224,
    EqualsEqualsToken = 1225,
    NotEqualsToken = 1226,
    ExclamationToken = 1227,

    // Delimiters
    DotToken = 1230,
    CommaToken = 1231,
    ColonToken = 1232,
    SemicolonToken = 1233,
    QuestionToken = 1234,

    // Grouping
    OpenParenToken = 1240,
    CloseParenToken = 1241,
    OpenBraceToken = 1242,
    CloseBraceToken = 1243,
    OpenBracketToken = 1244,
    CloseBracketToken = 1245,

    // === Trivia === (1300–1329)
    WhitespaceTrivia = 1300,
    TabTrivia = 1301,
    LineFeedTrivia = 1302,
    CarriageReturnTrivia = 1303,
    CarriageReturnLineFeedTrivia = 1304,
    EndOfLineTrivia = 1305,
    SingleLineCommentTrivia = 1306,
    MultiLineCommentTrivia = 1307,
    SkippedTokensTrivia = 1308,

    // === Line Tokens === (1330–1349)
    Whitespace = 1330,
    TabToken = 1331,
    LineFeedToken = 1332,
    CarriageReturnToken = 1333,
    CarriageReturnLineFeedToken = 1334,
    NewLineToken = 1335,

    // === End ===
    EndOfFileToken = 1999
}