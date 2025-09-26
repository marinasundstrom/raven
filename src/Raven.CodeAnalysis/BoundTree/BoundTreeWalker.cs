using System.Reflection.Emit;

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
            case BoundBlockStatement statement:
                VisitBlockStatement(statement);
                break;
            default:
                throw new NotImplementedException($"Unhandled node type: {node.GetType().Name}");
        }
    }

    public virtual void VisitExpression(BoundExpression node)
    {
        switch (node)
        {
            case BoundUnitExpression self:
                VisitUnitExpression(self);
                break;
            case BoundSelfExpression self:
                VisitSelfExpression(self);
                break;
            case BoundLiteralExpression lit:
                VisitLiteralExpression(lit);
                break;
            case BoundLocalAccess local:
                VisitLocalAccess(local);
                break;
            case BoundFieldAccess field:
                VisitFieldAccess(field);
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
            case BoundParenthesizedExpression paren:
                VisitParenthesizedExpression(paren);
                break;
            case BoundCastExpression cast:
                VisitCastExpression(cast);
                break;
            case BoundAsExpression asExpr:
                VisitAsExpression(asExpr);
                break;
            case BoundDelegateCreationExpression delegateCreation:
                VisitDelegateCreationExpression(delegateCreation);
                break;
            case BoundMethodGroupExpression methodGroup:
                VisitMethodGroupExpression(methodGroup);
                break;
            case BoundMemberAccessExpression memberAccess:
                VisitMemberAccessExpression(memberAccess);
                break;
            case BoundObjectCreationExpression creation:
                foreach (var arg in creation.Arguments)
                    VisitExpression(arg);
                break;
            case BoundWhileExpression whileExpr:
                VisitExpression(whileExpr.Condition);
                VisitExpression(whileExpr.Body);
                break;
            case BoundForExpression forExpr:
                VisitExpression(forExpr.Collection);
                VisitExpression(forExpr.Body);
                break;
            case BoundAssignmentExpression assign:
                VisitExpression(assign.Right);
                break;
            case BoundUnaryExpression unary:
                VisitExpression(unary.Operand);
                break;
            case BoundTupleExpression tuple:
                foreach (var e in tuple.Elements)
                    VisitExpression(e);
                break;
            case BoundTypeOfExpression typeOfExpression:
                VisitTypeOfExpression(typeOfExpression);
                break;
            // Add others as needed
            default:
                break;
        }
    }

    // Override these in your tree walker

    public override void VisitLiteralExpression(BoundLiteralExpression node) { }
    public override void VisitVariableExpression(BoundVariableExpression node) { }
    public override void VisitLocalAccess(BoundLocalAccess node) { }
    public override void VisitParameterAccess(BoundParameterAccess node) { }
    public override void VisitSelfExpression(BoundSelfExpression self) { }


    public virtual void VisitStatement(BoundStatement statement)
    {
        switch (statement)
        {
            case BoundFunctionStatement functionStatement:
                VisitFunctionStatement(functionStatement);
                break;
            case BoundExpressionStatement expressionStatement:
                VisitExpressionStatement(expressionStatement);
                break;
            case BoundAssignmentStatement assignmentStatement:
                VisitAssignmentStatement(assignmentStatement);
                break;
            case BoundLocalDeclarationStatement localDeclaration:
                VisitLocalDeclarationStatement(localDeclaration);
                break;
            case BoundReturnStatement ret:
                VisitReturnStatement(ret);
                break;
            case BoundIfStatement ifStmt:
                VisitIfStatement(ifStmt);
                break;
            case BoundLabeledStatement labeledStatement:
                VisitLabeledStatement(labeledStatement);
                break;
            case BoundGotoStatement gotoStatement:
                VisitGotoStatement(gotoStatement);
                break;
            case BoundWhileStatement whileStmt:
                VisitWhileStatement(whileStmt);
                break;
            case BoundForStatement forStmt:
                VisitForStatement(forStmt);
                break;
            case BoundTryStatement tryStmt:
                VisitTryStatement(tryStmt);
                break;
            case BoundBlockStatement blockStmt:
                VisitBlockStatement(blockStmt);
                break;
        }
    }

    public override void VisitReturnStatement(BoundReturnStatement node)
    {
        if (node.Expression is not null)
            VisitExpression(node.Expression);
    }

    public override void VisitAssignmentStatement(BoundAssignmentStatement node)
    {
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

    public override void VisitParenthesizedExpression(BoundParenthesizedExpression node)
    {
        VisitExpression(node.Expression);
    }

    public override void VisitCastExpression(BoundCastExpression node)
    {
        VisitExpression(node.Expression);
    }

    public override void VisitAsExpression(BoundAsExpression node)
    {
        VisitExpression(node.Expression);
    }

    public override void VisitLabeledStatement(BoundLabeledStatement node)
    {
        VisitStatement(node.Statement);
    }

    public override void VisitGotoStatement(BoundGotoStatement node)
    {
    }

    public virtual void VisitTypeOfExpression(BoundTypeOfExpression node) { }

    public virtual void VisitDelegateCreationExpression(BoundDelegateCreationExpression node)
    {
        VisitMethodGroupExpression(node.MethodGroup);
    }

    public virtual void VisitMethodGroupExpression(BoundMethodGroupExpression node)
    {
        if (node.Receiver is not null)
            VisitExpression(node.Receiver);
    }

    public virtual void VisitIfStatement(BoundIfStatement node)
    {
        VisitExpression(node.Condition);
        VisitStatement(node.ThenNode);
        if (node.ElseNode is not null)
            VisitStatement(node.ElseNode);
    }

    public virtual void VisitWhileStatement(BoundWhileStatement node)
    {
        VisitExpression(node.Condition);
        VisitStatement(node.Body);
    }

    public virtual void VisitForStatement(BoundForStatement node)
    {
        VisitExpression(node.Collection);
        VisitStatement(node.Body);
    }

    public virtual void VisitTryStatement(BoundTryStatement node)
    {
        VisitBlockStatement(node.TryBlock);

        foreach (var catchClause in node.CatchClauses)
            VisitCatchClause(catchClause);

        if (node.FinallyBlock is not null)
            VisitBlockStatement(node.FinallyBlock);
    }

    public virtual void VisitCatchClause(BoundCatchClause node)
    {
        VisitBlockStatement(node.Block);
    }

    public virtual void VisitBlockStatement(BoundBlockStatement node)
    {
        foreach (var s in node.Statements)
            VisitStatement(s);
    }
}
