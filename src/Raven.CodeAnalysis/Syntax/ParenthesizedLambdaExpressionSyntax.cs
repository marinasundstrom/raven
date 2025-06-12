namespace Raven.CodeAnalysis.Syntax;

public partial class ParenthesizedLambdaExpressionSyntax : LambdaExpressionSyntax
{
    public override partial SyntaxToken FuncKeyword { get; }
    public partial ParameterListSyntax ParameterList { get; }
    public partial ReturnTypeAnnotationSyntax ReturnType { get; }
    public override partial SyntaxToken ArrowToken { get; }
    public override partial ExpressionSyntax ExpressionBody { get; }

    internal ParenthesizedLambdaExpressionSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode? parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public ParenthesizedLambdaExpressionSyntax(
        SyntaxToken funcKeyword,
        ParameterListSyntax parameterList,
        ReturnTypeAnnotationSyntax returnType,
        SyntaxToken arrowToken,
        ExpressionSyntax? expressionBody)
        : this(new InternalSyntax.ParenthesizedLambdaExpressionSyntax(funcKeyword.Green, (InternalSyntax.ParameterListSyntax)parameterList.Green, (InternalSyntax.ReturnTypeAnnotationSyntax)returnType.Green, arrowToken.Green, (InternalSyntax.ExpressionSyntax?)expressionBody.Green, null))
    {
    }
}

public static partial class SyntaxFactory
{
    public static ParenthesizedLambdaExpressionSyntax ParenthesizedLambdaExpression(
        SyntaxToken funcKeyword,
        ParameterListSyntax parameterList,
        ReturnTypeAnnotationSyntax returnType,
        SyntaxToken arrowToken,
        ExpressionSyntax? expressionBody)
        => new ParenthesizedLambdaExpressionSyntax(funcKeyword, parameterList, returnType, arrowToken, expressionBody);
}