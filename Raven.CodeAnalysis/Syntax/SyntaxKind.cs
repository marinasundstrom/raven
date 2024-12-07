namespace Raven.CodeAnalysis.Syntax;

public enum SyntaxKind
{
    // List Kinds
    SyntaxList,
    SeparatedSyntaxList,

    // Node Kinds
    MethodDeclaration,
    IfStatement,
    ElseClause,
    Block,
    Parameter,
    ConditionExpression,
    AssignmentExpression,

    // Token Kinds
    VoidKeyword,
    IdentifierToken,
    OpenParenToken,
    CloseParenToken,
    OpenBraceToken,
    CloseBraceToken,
    IntKeyword,
    StringKeyword,
    IfKeyword,
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
