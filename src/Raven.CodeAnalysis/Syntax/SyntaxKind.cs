namespace Raven.CodeAnalysis.Syntax;

public enum SyntaxKind
{
    None,

    List,

    IdentifierName,
    GenericName,
    QualifiedName,
    AliasQualifiedName,
    NullableType,
    UnionType,

    TypeArgumentList,
    TypeArgument,

    NumericLiteralExpression,
    TrueLiteralExpression,
    FalseLiteralExpression,
    CharacterLiteralExpression,
    StringLiteralExpression,
    NullLiteralExpression,

    UnaryPlusExpression,
    UnaryMinusExpression,
    NotExpression,
    AddressOfExpression,

    SimpleLambdaExpression,
    ParenthesizedLambdaExpression,

    CompilationUnit,

    ImportDirective,

    NamespaceDeclaration,
    FileScopedNamespaceDeclaration,

    EnumDeclaration,
    EnumMemberDeclaration,

    MethodDeclaration,
    ReturnTypeAnnotation,
    ParameterList,
    Parameter,

    RefKeyword,
    OutKeyword,
    InKeyword,

    GlobalStatement,

    ExpressionStatement,
    ReturnStatement,
    LocalFunctionStatement,
    EmptyStatement,

    IfExpression,
    ElseClause,
    WhileExpression,
    Block,
    ObjectCreationExpression,
    CollectionExpression,
    CollectionElement,

    VariableDeclaration,
    VariableDeclarator,
    ArrowToken,
    TypeAnnotation,
    EqualsValueClause,
    LocalDeclaration,

    SimpleAssignmentExpression,

    ParenthesizedExpression,
    UnaryExpression,
    BinaryExpression,

    AddExpression,
    SubtractExpression,
    MultiplyExpression,
    DivideExpression,
    ModuloExpression,
    PowerExpression,
    EqualsExpression,
    NotEqualsExpression,
    LessThanExpression,
    GreaterThanExpression,
    LessThanOrEqualExpression,
    GreaterThanOrEqualExpression,
    LogicalAndExpression,
    LogicalOrExpression,

    IsPatternExpression,
    DeclarationPattern, // Pattern
    NotPattern,
    AndPattern,
    OrPattern,
    SingleVariableDesignation, //VariableDesignation

    SimpleMemberAccessExpression,
    //PointeMemberAccessExpression,
    InvocationExpression,
    ArgumentList,
    ElementAccessExpression,
    BracketedArgumentList,
    Argument,

    // Token Kinds
    IdentifierToken,
    NumericLiteralToken,
    CharacterLiteralToken,
    StringLiteralToken,

    PredefinedType,

    VoidKeyword,
    NullKeyword,
    IntKeyword,
    StringKeyword,
    BoolKeyword,
    CharKeyword,
    ImportKeyword,
    NamespaceKeyword,
    LetKeyword,
    VarKeyword,
    IfKeyword,
    ElseKeyword,
    WhileKeyword,
    ReturnKeyword,
    NewKeyword,
    TrueKeyword,
    FalseKeyword,
    IsKeyword,
    NotKeyword,
    AndKeyword,
    OrKeyword,
    FuncKeyword,
    EnumKeyword,


    OrToken,
    AndToken,

    LessThanToken,
    GreaterThanToken,
    GreaterOrEqualsToken,
    LessThanEqualsToken,

    OpenParenToken,
    CloseParenToken,
    OpenBraceToken,
    CloseBraceToken,
    OpenBracketToken,
    CloseBracketToken,

    CommaToken,
    EqualsToken,
    EqualsEqualsToken,
    NotEqualsToken,
    PlusToken,
    MinusToken,
    PercentToken,
    SlashToken,
    StarToken,
    DotToken,
    CaretToken,
    BarToken,
    ExclamationToken,
    QuestionToken,
    AmpersandToken,

    ColonToken,
    SemicolonToken,

    // Trivia Kinds
    WhitespaceTrivia,
    TabTrivia, // \t
    LineFeedTrivia, // \n separately
    CarriageReturnTrivia, // \r separately
    CarriageReturnLineFeedTrivia, // \r\n separately
    EndOfLineTrivia, // Used for all line sequences as one trivia

    SingleLineCommentTrivia,
    MultiLineCommentTrivia,
    SkippedTokensTrivia,

    Whitespace,
    TabToken, // \t

    LineFeedToken, // \n
    CarriageReturnToken, // \r
    CarriageReturnLineFeedToken, // \r\n
    NewLineToken, // Used for all line sequences represented as one token

    EndOfFileToken
}