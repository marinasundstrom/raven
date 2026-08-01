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
            case BoundStatement statement:
                VisitStatement(statement);
                break;
            case BoundVariableDeclarator declarator:
                VisitVariableDeclarator(declarator);
                break;
            case BoundMatchArm arm:
                VisitMatchArm(arm);
                break;
            default:
                DefaultVisit(node);
                break;
        }
    }

    public override void VisitExpression(BoundExpression node) => base.VisitExpression(node);

    // Override these in your tree walker

    public override void VisitLiteralExpression(BoundLiteralExpression node) { }
    public override void VisitVariableExpression(BoundVariableExpression node) { }
    public override void VisitLocalAccess(BoundLocalAccess node) { }
    public override void VisitParameterAccess(BoundParameterAccess node) { }
    public override void VisitSelfExpression(BoundSelfExpression self) { }


    public override void VisitStatement(BoundStatement statement) => base.VisitStatement(statement);

    public override void VisitReturnStatement(BoundReturnStatement node)
    {
        if (node.Expression is not null)
            VisitExpression(node.Expression);
    }

    public override void VisitThrowStatement(BoundThrowStatement node)
    {
        VisitExpression(node.Expression);
    }

    public override void VisitThrowExpression(BoundThrowExpression node)
    {
        VisitExpression(node.Expression);
    }

    public override void VisitReturnExpression(BoundReturnExpression node)
    {
        if (node.Expression is not null)
            VisitExpression(node.Expression);
    }

    public override void VisitNullCoalesceExpression(BoundNullCoalesceExpression node)
    {
        VisitExpression(node.Left);
        VisitExpression(node.Right);
    }

    public override void VisitAssignmentStatement(BoundAssignmentStatement node)
    {
        VisitExpression(node.Expression);
    }

    public override void VisitLocalDeclarationStatement(BoundLocalDeclarationStatement node)
    {
        foreach (var declarator in node.Declarators)
            VisitVariableDeclarator(declarator);
    }

    public override void VisitMatchStatement(BoundMatchStatement node)
    {
        VisitExpression(node.Expression);
        foreach (var arm in node.Arms)
            VisitMatchArm(arm);
    }

    public override void VisitMatchArm(BoundMatchArm node)
    {
        VisitPattern(node.Pattern);

        if (node.Guard is not null)
            VisitExpression(node.Guard);

        VisitExpression(node.Expression);
    }

    public override void VisitVariableDeclarator(BoundVariableDeclarator node)
    {
        if (node.Initializer is not null)
            VisitExpression(node.Initializer);
    }

    public override void VisitExpressionStatement(BoundExpressionStatement node)
    {
        VisitExpression(node.Expression);
    }

    public override void VisitBreakStatement(BoundBreakStatement node)
    {
    }

    public override void VisitContinueStatement(BoundContinueStatement node)
    {
    }

    public override void VisitYieldReturnStatement(BoundYieldReturnStatement node)
    {
        VisitExpression(node.Expression);
    }

    public override void VisitYieldBreakStatement(BoundYieldBreakStatement node)
    {
    }

    public override void VisitConditionalGotoStatement(BoundConditionalGotoStatement node)
    {
        VisitExpression(node.Condition);
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
        if (node.ExtensionReceiver is not null && !ReferenceEquals(node.ExtensionReceiver, node.Receiver))
            VisitExpression(node.ExtensionReceiver);
        foreach (var arg in node.Arguments)
            VisitExpression(arg);
    }

    public override void VisitAwaitExpression(BoundAwaitExpression node)
    {
        VisitExpression(node.Expression);
    }

    public override void VisitFunctionExpression(BoundFunctionExpression node)
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

    public override void VisitTryExpression(BoundTryExpression node)
    {
        VisitExpression(node.Expression);
    }

    public override void VisitPropagateExpression(BoundPropagateExpression node)
    {
        VisitExpression(node.Operand);
    }

    public override void VisitParenthesizedExpression(BoundParenthesizedExpression node)
    {
        VisitExpression(node.Expression);
    }

    public override void VisitConversionExpression(BoundConversionExpression node)
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

    public override void VisitTypeOfExpression(BoundTypeOfExpression node) { }

    public override void VisitDelegateCreationExpression(BoundDelegateCreationExpression node)
    {
        VisitMethodGroupExpression(node.MethodGroup);
    }

    public override void VisitConditionalAccessExpression(BoundConditionalAccessExpression node)
    {
        VisitExpression(node.Receiver);
        VisitExpression(node.WhenNotNull);
    }

    public override void VisitCarrierConditionalAccessExpression(BoundCarrierConditionalAccessExpression node)
    {
        VisitExpression(node.Receiver);
        VisitExpression(node.WhenPresent);
    }

    public override void VisitIfExpression(BoundIfExpression node)
    {
        VisitExpression(node.Condition);
        VisitExpression(node.ThenBranch);
        if (node.ElseBranch is not null)
            VisitExpression(node.ElseBranch);
    }

    public override void VisitIndexerAccessExpression(BoundIndexerAccessExpression node)
    {
        VisitExpression(node.Receiver);
        foreach (var argument in node.Arguments)
            VisitExpression(argument);
    }

    public override void VisitArrayAccessExpression(BoundArrayAccessExpression node)
    {
        VisitExpression(node.Receiver);
        foreach (var index in node.Indices)
            VisitExpression(index);
    }

    public override void VisitCollectionExpression(BoundCollectionExpression node)
    {
        foreach (var element in node.Elements)
            VisitExpression(element);
    }

    public override void VisitDictionaryExpression(BoundDictionaryExpression node)
    {
        foreach (var element in node.Elements)
        {
            switch (element)
            {
                case DictionaryEntryBinding entry:
                    VisitExpression(entry.Key);
                    VisitExpression(entry.Value);
                    break;
                case DictionarySpreadBinding spread:
                    VisitExpression(spread.Expression);
                    break;
                case DictionaryComprehensionBinding comprehension:
                    VisitExpression(comprehension.Source);
                    if (comprehension.Condition is not null)
                        VisitExpression(comprehension.Condition);
                    VisitExpression(comprehension.KeySelector);
                    VisitExpression(comprehension.ValueSelector);
                    break;
            }
        }
    }

    public override void VisitCollectionComprehensionExpression(BoundCollectionComprehensionExpression node)
    {
        VisitExpression(node.Source);
        if (node.Condition is not null)
            VisitExpression(node.Condition);
        VisitExpression(node.Selector);
    }

    public override void VisitSpreadElement(BoundSpreadElement node)
    {
        VisitExpression(node.Expression);
    }

    public override void VisitIsPatternExpression(BoundIsPatternExpression node)
    {
        VisitExpression(node.Expression);
        VisitPattern(node.Pattern);
    }

    public override void VisitMatchExpression(BoundMatchExpression node)
    {
        VisitExpression(node.Expression);
        foreach (var arm in node.Arms)
            VisitMatchArm(arm);
    }

    public override void VisitPattern(BoundPattern node)
    {
        switch (node)
        {
            case BoundAndPattern andPattern:
                VisitAndPattern(andPattern);
                break;
            case BoundCasePattern casePattern:
                VisitCasePattern(casePattern);
                break;
            case BoundConstantPattern constantPattern:
                VisitConstantPattern(constantPattern);
                break;
            case BoundDeclarationPattern declarationPattern:
                VisitDeclarationPattern(declarationPattern);
                break;
            case BoundDeconstructPattern deconstructPattern:
                VisitDeconstructPattern(deconstructPattern);
                break;
            case BoundDiscardPattern discardPattern:
                VisitDiscardPattern(discardPattern);
                break;
            case BoundNotPattern notPattern:
                VisitNotPattern(notPattern);
                break;
            case BoundOrPattern orPattern:
                VisitOrPattern(orPattern);
                break;
            case BoundPositionalPattern positionalPattern:
                VisitPositionalPattern(positionalPattern);
                break;
            case BoundDictionaryPattern dictionaryPattern:
                VisitDictionaryPattern(dictionaryPattern);
                break;
            case BoundPropertyPattern propertyPattern:
                VisitPropertyPattern(propertyPattern);
                break;
            case BoundComparisonPattern relationalPattern:
                VisitComparisonPattern(relationalPattern);
                break;
            default:
                break;
        }
    }

    public override void VisitAndPattern(BoundAndPattern node)
    {
        VisitPattern(node.Left);
        VisitPattern(node.Right);
    }

    public override void VisitCasePattern(BoundCasePattern node)
    {
        foreach (var argument in node.Arguments)
            VisitPattern(argument);
    }

    public override void VisitConstantPattern(BoundConstantPattern node)
    {
        if (node.Expression is not null)
            VisitExpression(node.Expression);
    }

    public override void VisitDeclarationPattern(BoundDeclarationPattern node)
    {
        VisitDesignator(node.Designator);
    }

    public override void VisitDeconstructPattern(BoundDeconstructPattern node)
    {
        foreach (var argument in node.Arguments)
            VisitPattern(argument);
    }

    public override void VisitDesignator(BoundDesignator node)
    {
        switch (node)
        {
            case BoundDiscardDesignator discardDesignator:
                VisitDiscardDesignator(discardDesignator);
                break;
            case BoundSingleVariableDesignator singleVariableDesignator:
                VisitSingleVariableDesignator(singleVariableDesignator);
                break;
        }
    }

    public override void VisitNotPattern(BoundNotPattern node)
    {
        VisitPattern(node.Pattern);
    }

    public override void VisitOrPattern(BoundOrPattern node)
    {
        VisitPattern(node.Left);
        VisitPattern(node.Right);
    }

    public override void VisitPositionalPattern(BoundPositionalPattern node)
    {
        foreach (var element in node.Elements)
            VisitPattern(element);
    }

    public override void VisitDictionaryPattern(BoundDictionaryPattern node)
    {
        if (node.Designator is not null)
            VisitDesignator(node.Designator);

        foreach (var entry in node.Entries)
        {
            VisitExpression(entry.Key);
            VisitPattern(entry.Pattern);
        }
    }

    public override void VisitPropertyPattern(BoundPropertyPattern node)
    {
        if (node.Designator is not null)
            VisitDesignator(node.Designator);

        foreach (var property in node.Properties)
            VisitPattern(property.Pattern);
    }

    public override void VisitComparisonPattern(BoundComparisonPattern node)
    {
        VisitExpression(node.Value);
    }

    public override void VisitAddressOfExpression(BoundAddressOfExpression node)
    {
        if (node.Receiver is not null)
            VisitExpression(node.Receiver);
    }

    public override void VisitMethodGroupExpression(BoundMethodGroupExpression node)
    {
        if (node.Receiver is not null)
            VisitExpression(node.Receiver);
    }

    public override void VisitFieldAccess(BoundFieldAccess node)
    {
        if (node.Receiver is not null)
            VisitExpression(node.Receiver);
    }

    public override void VisitMemberAccessExpression(BoundMemberAccessExpression node)
    {
        if (node.Receiver is not null)
            VisitExpression(node.Receiver);
    }

    public override void VisitPointerMemberAccessExpression(BoundPointerMemberAccessExpression node)
    {
        VisitExpression(node.PointerReceiver);
    }

    public override void VisitIfStatement(BoundIfStatement node)
    {
        VisitExpression(node.Condition);
        VisitStatement(node.ThenNode);
        if (node.ElseNode is not null)
            VisitStatement(node.ElseNode);
    }

    public override void VisitWhileStatement(BoundWhileStatement node)
    {
        VisitExpression(node.Condition);
        VisitStatement(node.Body);
    }

    public override void VisitForStatement(BoundForStatement node)
    {
        VisitExpression(node.Collection);
        VisitStatement(node.Body);
    }

    public override void VisitTryStatement(BoundTryStatement node)
    {
        VisitBlockStatement(node.TryBlock);

        foreach (var catchClause in node.CatchClauses)
            VisitCatchClause(catchClause);

        if (node.FinallyBlock is not null)
            VisitBlockStatement(node.FinallyBlock);
    }

    public override void VisitCatchClause(BoundCatchClause node)
    {
        VisitBlockStatement(node.Block);
    }

    public override void VisitBlockStatement(BoundBlockStatement node)
    {
        foreach (var s in node.Statements)
            VisitStatement(s);
    }
}
