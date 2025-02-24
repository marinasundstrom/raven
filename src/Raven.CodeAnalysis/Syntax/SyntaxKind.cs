﻿namespace Raven.CodeAnalysis.Syntax;

public enum SyntaxKind
{
    None,

    List,

    IdentifierName,
    GenericName,
    QualifiedName,
    AliasQualifiedName,

    TypeArgumentList,
    TypeArgument,

    NumericLiteralExpression,
    TrueLiteralExpression,
    FalseLiteralExpression,
    CharacterLiteralExpression,
    StringLiteralExpression,
    NothingLiteralExpression,

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
    Argument,

    // Token Kinds
    IdentifierToken,
    NumericLiteralToken,
    CharacterLiteralToken,
    StringLiteralToken,

    PredefinedType,

    VoidKeyword,
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
    DotToken,
    CaretToken,
    ExclamationToken,

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