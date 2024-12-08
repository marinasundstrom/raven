namespace Raven.CodeAnalysis.Syntax;

public enum SyntaxKind
{
    Unknown,

    IdentifierName,
    NumericLiteralExpression,

    QualifiedName,
    AliasQualifiedName,

    // List Kinds
    SyntaxList,
    SeparatedSyntaxList,

    // Node Kinds
    MethodDeclaration,
    ReturnStatement,
    IfStatement,
    ElseClause,
    Block,
    Parameter,
    ConditionExpression,
    AssignmentExpression,

    // Token Kinds
    IdentifierToken,
    OpenParenToken,
    CloseParenToken,
    OpenBraceToken,
    CloseBraceToken,

    VoidKeyword,
    IntKeyword,
    StringKeyword,
    IfKeyword,
    ElseKeyword,
    ReturnKeyword,

    CommaToken,
    EqualsToken,
    PlusToken,
    GreaterThanToken,
    SemicolonToken,
    NumericLiteralToken,

    // Trivia Kinds
    WhitespaceTrivia,
    CommentTrivia
}
