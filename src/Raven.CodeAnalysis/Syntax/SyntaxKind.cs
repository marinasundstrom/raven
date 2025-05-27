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

    CompilationUnit,

    ImportDirective,

    NamespaceDeclaration,
    FileScopedNamespaceDeclaration,

    MethodDeclaration,
    ReturnTypeAnnotation,
    ParameterList,
    Parameter,

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
    FunKeyword,


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

    ColonToken,
    SemicolonToken,

    // Trivia Kinds
    WhitespaceTrivia,
    LineFeedTrivia,
    CarriageReturnTrivia,
    CarriageReturnLineFeedTrivia,
    TabTrivia,
    SingleLineCommentTrivia,
    MultiLineCommentTrivia,
    SkippedTokensTrivia,
    EndOfLineTrivia,

    Whitespace,
    TabToken,

    EndOfLineToken,
    CarriageReturnToken,
    EndOfFileToken,

}