namespace Raven.CodeAnalysis.Syntax;

public enum SyntaxKind
{
    None,

    List,

    IdentifierName,
    NumericLiteralExpression,
    BooleanLiteralExpression,
    StringLiteralExpression,

    QualifiedName,
    AliasQualifiedName,

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
    IfStatement,
    ElseClause,
    Block,

    VariableDeclaration,
    VariableDeclarator,
    TypeAnnotation,
    EqualsValueClause,
    LocalDeclaration,

    ParenthesizedExpression,
    UnaryExpression,
    BinaryExpression,

    AddExpression,
    SubtractExpression,
    MultiplyExpression,
    DivideExpression,
    ModuloExpression,
    EqualsExpression,
    NotEqualsExpression,
    LessThanExpression,
    GreaterThanExpression,
    LessThanOrEqualExpression,
    GreaterThanOrEqualExpression,

    // Token Kinds
    IdentifierToken,
    NumericLiteralToken,

    VoidKeyword,
    IntKeyword,
    StringKeyword,
    BoolKeyword,
    CharKeyword,
    ImportKeyword,
    NamespaceKeyword,
    LetKeyword,
    IfKeyword,
    ElseKeyword,
    ReturnKeyword,
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

    CommaToken,
    EqualsToken,
    NotEqualsToken,
    PlusToken,
    MinusToken,
    PercentToken,
    SlashToken,
    StarToken,
    CaretToken,

    ColonToken,
    SemicolonToken,

    // Trivia Kinds
    WhitespaceTrivia,
    LineFeedTrivia,
    CarriageReturnTrivia,
    CarriageReturnLineFeedTrivia,
    TabTrivia,
    CommentTrivia,


    EndOfLineTrivia,
    Whitespace,
    TabToken,

    EndOfLineToken,
    CarriageReturnToken,
    EndOfFileToken
}