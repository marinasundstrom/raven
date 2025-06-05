using Raven.CodeAnalysis.Syntax.InternalSyntax;

namespace Raven.CodeAnalysis.Syntax;

public partial class SimpleLambdaExpressionSyntax : LambdaExpressionSyntax
{
    public override partial SyntaxToken FuncKeyword { get; }
    public partial ParameterSyntax Parameter { get; }
    public partial ReturnTypeAnnotationSyntax ReturnType { get; }
    public override partial SyntaxToken ArrowToken { get; }
    public override partial ExpressionSyntax ExpressionBody { get; }

    internal SimpleLambdaExpressionSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode? parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public SimpleLambdaExpressionSyntax(
        SyntaxToken funcKeyword,
        ParameterSyntax parameter,
        ReturnTypeAnnotationSyntax returnType,
        SyntaxToken arrowToken,
        ExpressionSyntax? expressionBody)
        : this(new InternalSyntax.SimpleLambdaExpressionSyntax(funcKeyword.Green, (InternalSyntax.ParameterSyntax)parameter.Green, (InternalSyntax.ReturnTypeAnnotationSyntax)returnType.Green, arrowToken.Green, (InternalSyntax.ExpressionSyntax?)expressionBody.Green, null))
    {
    }
}

public static partial class SyntaxFactory
{
    public static SimpleLambdaExpressionSyntax SimpleLambdaExpression(
        SyntaxToken funcKeyword,
        ParameterSyntax parameter,
        ReturnTypeAnnotationSyntax returnType,
        SyntaxToken arrowToken,
        ExpressionSyntax? expressionBody)
        => new SimpleLambdaExpressionSyntax(funcKeyword, parameter, returnType, arrowToken, expressionBody);
}