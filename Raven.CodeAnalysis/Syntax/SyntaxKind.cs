namespace Raven.CodeAnalysis.Syntax;

public enum SyntaxKind
{
    None,

    IdentifierName,
    NumericLiteralExpression,

    QualifiedName,
    AliasQualifiedName,

    CompilationUnit,

    ImportDirective,

    // List Kinds
    SyntaxList,
    SeparatedSyntaxList,

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
    CommentTrivia,
    Tab,

    EndOfLineToken,
    CarriageReturnToken,
    EndOfFileTokenToken
}