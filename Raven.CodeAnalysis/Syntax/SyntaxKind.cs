namespace Raven.CodeAnalysis.Syntax;

public enum SyntaxKind
{
    None,

    List,

    IdentifierName,
    NumericLiteralExpression,

    QualifiedName,
    AliasQualifiedName,

    CompilationUnit,

    ImportDirective,

    // Node Kinds
    MethodDeclaration,
    GlobalStatement,
    ExpressionStatement,
    ReturnStatement,
    IfStatement,
    ElseClause,
    Block,
    Parameter,
    ConditionExpression,
    AssignmentExpression,
    VariableDeclaration,
    VariableDeclarator,
    TypeAnnotation,
    EqualsValueClause,
    LocalDeclaration,

    // Token Kinds
    IdentifierToken,
    OpenParenToken,
    CloseParenToken,
    OpenBraceToken,
    CloseBraceToken,

    VoidKeyword,
    IntKeyword,
    StringKeyword,
    LetKeyword,
    IfKeyword,
    ElseKeyword,
    ReturnKeyword,

    CommaToken,
    EqualsToken,
    PlusToken,
    GreaterThanToken,
    ColonToken,
    SemicolonToken,
    NumericLiteralToken,

    // Trivia Kinds
    WhitespaceTrivia,
    TabTrivia,
    EndOfLineTrivia,
    CarriageReturnTrivia,
    CarriageReturnLineFeedTrivia,
    CommentTrivia,


    Whitespace,
    TabToken,
    EndOfLineToken,
    CarriageReturnToken,
    EndOfFileToken
}