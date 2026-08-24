namespace Raven.CodeAnalysis;

internal static class BoundNodeFacts
{
    public static bool IsAbruptExpression(BoundExpression expression)
    {
        switch (expression)
        {
            case BoundReturnExpression:
            case BoundThrowExpression:
            case BoundBreakExpression:
            case BoundContinueExpression:
                return true;
            case BoundRequiredResultExpression requiredResult:
                return IsAbruptExpression(requiredResult.Operand);
            case BoundParenthesizedExpression parenthesized:
                return IsAbruptExpression(parenthesized.Expression);
            case BoundConversionExpression conversion:
                return IsAbruptExpression(conversion.Expression);
            case BoundUnaryExpression unary:
                return IsAbruptExpression(unary.Operand);
            case BoundBinaryExpression binary:
                if (IsAbruptExpression(binary.Left))
                    return true;

                var operatorKind = binary.Operator.OperatorKind &
                    ~(BinaryOperatorKind.Lifted | BinaryOperatorKind.Checked);
                return operatorKind is not (BinaryOperatorKind.LogicalAnd or BinaryOperatorKind.LogicalOr) &&
                    IsAbruptExpression(binary.Right);
            case BoundMemberAccessExpression { Receiver: { } receiver }:
                return IsAbruptExpression(receiver);
            case BoundPointerMemberAccessExpression pointerMemberAccess:
                return IsAbruptExpression(pointerMemberAccess.PointerReceiver);
            case BoundInvocationExpression invocation:
                return invocation.Receiver is not null && IsAbruptExpression(invocation.Receiver) ||
                    invocation.ExtensionReceiver is not null && IsAbruptExpression(invocation.ExtensionReceiver) ||
                    invocation.Arguments.Any(IsAbruptExpression);
            case BoundObjectCreationExpression objectCreation:
                return objectCreation.Receiver is not null && IsAbruptExpression(objectCreation.Receiver) ||
                    objectCreation.Arguments.Any(IsAbruptExpression) ||
                    ObjectInitializerIsAbrupt(objectCreation.Initializer);
            case BoundArrayAccessExpression arrayAccess:
                return IsAbruptExpression(arrayAccess.Receiver) ||
                    arrayAccess.Indices.Any(IsAbruptExpression);
            case BoundIndexerAccessExpression indexerAccess:
                return IsAbruptExpression(indexerAccess.Receiver) ||
                    indexerAccess.Arguments.Any(IsAbruptExpression);
            case BoundIndexExpression index:
                return IsAbruptExpression(index.Value);
            case BoundAssignmentExpression assignment:
                return IsAbruptExpression(assignment.Left) ||
                    IsAbruptExpression(assignment.Right);
            case BoundConditionalAccessExpression conditionalAccess:
                return IsAbruptExpression(conditionalAccess.Receiver);
            case BoundIfExpression { ElseBranch: not null } ifExpression:
                return IsAbruptExpression(ifExpression.Condition) ||
                    IsAbruptExpression(ifExpression.ThenBranch) &&
                    IsAbruptExpression(ifExpression.ElseBranch);
            case BoundMatchExpression { Arms.IsDefaultOrEmpty: false } matchExpression:
                return IsAbruptExpression(matchExpression.Expression) ||
                    matchExpression.Arms.All(static arm => IsAbruptExpression(arm.Expression));
            case BoundBlockExpression block:
                {
                    var last = block.Statements.LastOrDefault();
                    return last is not null && IsAbruptStatement(last);
                }
            default:
                return false;
        }
    }

    private static bool IsAbruptStatement(BoundStatement statement)
    {
        return statement switch
        {
            BoundReturnStatement or BoundThrowStatement or BoundGotoStatement or
            BoundBreakStatement or BoundContinueStatement => true,
            BoundExpressionStatement expressionStatement => IsAbruptExpression(expressionStatement.Expression),
            BoundBlockStatement block when block.Statements.LastOrDefault() is { } last => IsAbruptStatement(last),
            BoundIfStatement { ElseNode: not null } ifStatement =>
                IsAbruptStatement(ifStatement.ThenNode) && IsAbruptStatement(ifStatement.ElseNode),
            _ => false,
        };
    }

    private static bool ObjectInitializerIsAbrupt(BoundObjectInitializer? initializer)
    {
        if (initializer is null)
            return false;

        return initializer.Entries.Any(static entry => entry switch
        {
            BoundObjectInitializerAssignmentEntry assignment => IsAbruptExpression(assignment.Value),
            BoundObjectInitializerExpressionEntry expression => IsAbruptExpression(expression.Expression),
            _ => false,
        });
    }

    public static bool MatchArmGuardGuaranteesMatch(BoundExpression? guard)
    {
        if (guard is null)
            return true;

        return TryEvaluateBooleanConstant(guard) == true;
    }

    public static bool ContainsControlTransfer(BoundExpression expression)
    {
        var finder = new ControlTransferFinder();
        finder.VisitExpression(expression);
        return finder.Found;
    }

    private sealed class ControlTransferFinder : BoundTreeWalker
    {
        public bool Found { get; private set; }

        public override void VisitStatement(BoundStatement statement)
        {
            if (!Found)
                base.VisitStatement(statement);
        }

        public override void VisitExpression(BoundExpression node)
        {
            if (!Found)
                base.VisitExpression(node);
        }

        public override void VisitGotoStatement(BoundGotoStatement node) => Found = true;
        public override void VisitReturnStatement(BoundReturnStatement node) => Found = true;
        public override void VisitThrowStatement(BoundThrowStatement node) => Found = true;
        public override void VisitBreakStatement(BoundBreakStatement node) => Found = true;
        public override void VisitContinueStatement(BoundContinueStatement node) => Found = true;
        public override void VisitReturnExpression(BoundReturnExpression node) => Found = true;
        public override void VisitThrowExpression(BoundThrowExpression node) => Found = true;
        public override void VisitBreakExpression(BoundBreakExpression node) => Found = true;
        public override void VisitContinueExpression(BoundContinueExpression node) => Found = true;

        public override void VisitFunctionExpression(BoundFunctionExpression node)
        {
            // Nested callables transfer within their own control-flow scope.
        }
    }

    private static bool? TryEvaluateBooleanConstant(BoundExpression expression)
    {
        expression = UnwrapBooleanExpression(expression);

        if (expression is BoundLiteralExpression literal)
        {
            return literal.Kind switch
            {
                BoundLiteralExpressionKind.TrueLiteral => true,
                BoundLiteralExpressionKind.FalseLiteral => false,
                _ when literal.Value is bool value => value,
                _ => null,
            };
        }

        return null;
    }

    private static BoundExpression UnwrapBooleanExpression(BoundExpression expression)
    {
        while (true)
        {
            switch (expression)
            {
                case BoundParenthesizedExpression parenthesized:
                    expression = parenthesized.Expression;
                    continue;
                case BoundConversionExpression cast when cast.Conversion.IsIdentity:
                    expression = cast.Expression;
                    continue;
                case BoundAsExpression asExpression when asExpression.Conversion.IsIdentity:
                    expression = asExpression.Expression;
                    continue;
            }

            return expression;
        }
    }
}
