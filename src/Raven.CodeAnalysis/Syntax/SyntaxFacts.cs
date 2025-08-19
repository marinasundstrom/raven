namespace Raven.CodeAnalysis.Syntax;

public static partial class SyntaxFacts
{
    public static bool TryResolveOperatorPrecedence(SyntaxKind kind, out int precedence)
    {
        switch (kind)
        {
            case SyntaxKind.StarToken:
            case SyntaxKind.SlashToken:
            case SyntaxKind.PercentToken:
                precedence = 5;
                return true;

            case SyntaxKind.PlusToken:
            case SyntaxKind.MinusToken:
                precedence = 4;
                return true;

            case SyntaxKind.LessThanToken:
            case SyntaxKind.LessThanOrEqualsToken:
            case SyntaxKind.GreaterThanToken:
            case SyntaxKind.GreaterThanOrEqualsToken:
            case SyntaxKind.EqualsEqualsToken:
            case SyntaxKind.NotEqualsToken:
                precedence = 3;
                return true;

            case SyntaxKind.AndToken:
                precedence = 2;
                return true;

            case SyntaxKind.OrToken:
                precedence = 1;
                return true;

            default:
                precedence = -1;
                return false;
        }
    }

    public static bool IsExpression(SyntaxKind kind)
    {
        switch (kind)
        {
            case SyntaxKind.NumericLiteralExpression:
            case SyntaxKind.TrueLiteralExpression:
            case SyntaxKind.FalseLiteralExpression:
            case SyntaxKind.CharacterLiteralExpression:
            case SyntaxKind.StringLiteralExpression:
            case SyntaxKind.NullLiteralExpression:

            case SyntaxKind.IdentifierName:
            case SyntaxKind.QualifiedName:
            case SyntaxKind.GenericName:
            case SyntaxKind.AliasQualifiedName:
            case SyntaxKind.NullableType:
            case SyntaxKind.UnionType:

            case SyntaxKind.ObjectCreationExpression:
            case SyntaxKind.CollectionExpression:
            case SyntaxKind.ParenthesizedExpression:
            case SyntaxKind.SimpleMemberAccessExpression:
            case SyntaxKind.ElementAccessExpression:
            case SyntaxKind.InvocationExpression:

            case SyntaxKind.UnaryExpression:
            case SyntaxKind.UnaryPlusExpression:
            case SyntaxKind.UnaryMinusExpression:
            case SyntaxKind.AddressOfExpression:

            case SyntaxKind.BinaryExpression:
            case SyntaxKind.AddExpression:
            case SyntaxKind.SubtractExpression:
            case SyntaxKind.MultiplyExpression:
            case SyntaxKind.DivideExpression:
            case SyntaxKind.ModuloExpression:
            case SyntaxKind.PowerExpression:
            case SyntaxKind.EqualsExpression:
            case SyntaxKind.NotEqualsExpression:
            case SyntaxKind.LessThanExpression:
            case SyntaxKind.GreaterThanExpression:
            case SyntaxKind.LessThanOrEqualsExpression:
            case SyntaxKind.GreaterThanOrEqualsExpression:
            case SyntaxKind.LogicalAndExpression:
            case SyntaxKind.LogicalOrExpression:
            case SyntaxKind.LogicalNotExpression:

            case SyntaxKind.SimpleAssignmentExpression:

            case SyntaxKind.IfExpression:
            case SyntaxKind.WhileExpression:

            case SyntaxKind.IsPatternExpression:
            case SyntaxKind.SimpleLambdaExpression:
            case SyntaxKind.ParenthesizedLambdaExpression:
                return true;

            default:
                return false;
        }
    }

    public static bool IsStatement(SyntaxKind kind)
    {
        switch (kind)
        {
            case SyntaxKind.Block:
            case SyntaxKind.ExpressionStatement:
            case SyntaxKind.ReturnStatement:
            case SyntaxKind.LocalDeclaration:
            case SyntaxKind.EmptyStatement:
            case SyntaxKind.GlobalStatement:
            case SyntaxKind.LocalFunctionStatement:
                return true;

            default:
                return false;
        }
    }

    public static bool IsLiteralExpression(SyntaxKind kind) => kind is
        SyntaxKind.NumericLiteralExpression or
        SyntaxKind.TrueLiteralExpression or
        SyntaxKind.FalseLiteralExpression or
        SyntaxKind.CharacterLiteralExpression or
        SyntaxKind.StringLiteralExpression or
        SyntaxKind.NullLiteralExpression;

    public static bool IsAssignmentExpression(SyntaxKind kind)
    {
        return kind == SyntaxKind.SimpleAssignmentExpression;
    }

    public static bool IsTypeSyntax(SyntaxKind kind)
    {
        return kind switch
        {
            SyntaxKind.PredefinedType => true,
            SyntaxKind.IdentifierName => true,
            SyntaxKind.GenericName => true,
            SyntaxKind.QualifiedName => true,
            SyntaxKind.AliasQualifiedName => true,
            SyntaxKind.NullableType => true,
            SyntaxKind.UnionType => true,
            _ => false
        };
    }

    public static bool IsUnaryOperatorToken(SyntaxKind kind)
    {
        return kind switch
        {
            SyntaxKind.PlusToken => true,        // Unary +
            SyntaxKind.MinusToken => true,       // Unary -
            SyntaxKind.ExclamationToken => true, // Logical not
            SyntaxKind.AmpersandToken => true,   // Address-of
            _ => false
        };
    }
}