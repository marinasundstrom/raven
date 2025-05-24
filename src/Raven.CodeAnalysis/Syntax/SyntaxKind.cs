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
    ParameterList,
    Parameter,
    GlobalStatement,

    ExpressionStatement,
    ReturnStatement,
    IfExpression,
    ElseClause,
    WhileExpression,
    Block,
    ObjectCreationExpression,
    CollectionExpression,
    CollectionElement,

    VariableDeclaration,
    VariableDeclarator,
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
    NotKeyword,

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
    EmptyStatement
}