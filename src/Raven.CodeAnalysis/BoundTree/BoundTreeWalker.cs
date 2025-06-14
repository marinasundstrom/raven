namespace Raven.CodeAnalysis;

internal class BoundTreeWalker : BoundTreeVisitor
{
    public override void Visit(BoundNode node)
    {
        switch (node)
        {
            case BoundExpression expr:
                VisitExpression(expr);
                break;
            default:
                throw new NotImplementedException($"Unhandled node type: {node.GetType().Name}");
        }
    }

    public virtual void VisitExpression(BoundExpression node)
    {
        switch (node)
        {
            case BoundLiteralExpression lit:
                VisitLiteralExpression(lit);
                break;
            case BoundLocalAccess local:
                VisitLocalAccess(local);
                break;
            case BoundParameterAccess par:
                VisitParameterAccess(par);
                break;
            case BoundBinaryExpression bin:
                VisitBinaryExpression(bin);
                break;
            case BoundInvocationExpression call:
                VisitInvocationExpression(call);
                break;
            case BoundLambdaExpression lambda:
                VisitLambdaExpression(lambda);
                break;
            case BoundBlockExpression block:
                VisitBlockExpression(block);
                break;
            // Add others as needed
            default:
                throw new NotImplementedException($"Unhandled expression: {node.GetType().Name}");
        }
    }

    // Override these in your tree walker

    public override void VisitLiteralExpression(BoundLiteralExpression node) { }
    public override void VisitVariableExpression(BoundVariableExpression node) { }
    public override void VisitLocalAccess(BoundLocalAccess node) { }
    public override void VisitParameterAccess(BoundParameterAccess node) { }

    public virtual void VisitStatement(BoundStatement statement)
    {
        switch (statement)
        {
            case BoundLocalFunctionStatement localFunctionStatement:
                VisitLocalFunctionStatement(localFunctionStatement);
                break;
            case BoundExpressionStatement expressionStatement:
                VisitExpressionStatement(expressionStatement);
                break;
            case BoundLocalDeclarationStatement localDeclaration:
                VisitLocalDeclarationStatement(localDeclaration);
                break;
            case BoundReturnStatement ret:
                VisitReturnStatement(ret);
                break;
        }
    }

    public override void VisitReturnStatement(BoundReturnStatement node)
    {
        if (node.Expression is not null)
            VisitExpression(node.Expression);
    }

    public override void VisitBinaryExpression(BoundBinaryExpression node)
    {
        VisitExpression(node.Left);
        VisitExpression(node.Right);
    }

    public override void VisitInvocationExpression(BoundInvocationExpression node)
    {
        if (node.Receiver is not null)
            VisitExpression(node.Receiver);
        foreach (var arg in node.Arguments)
            VisitExpression(arg);
    }

    public override void VisitLambdaExpression(BoundLambdaExpression node)
    {
        VisitExpression(node.Body);
    }

    public override void VisitBlockExpression(BoundBlockExpression node)
    {
        foreach (var s in node.Statements)
        {
            VisitStatement(s);
        }
    }
}