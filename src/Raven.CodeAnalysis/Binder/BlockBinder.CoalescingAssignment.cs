using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

partial class BlockBinder
{
    private BoundExpression GuardNullCoalescingAssignment(BoundExpression expression, SyntaxNode syntax)
    {
        // Only walk the assignment scaffolding produced by binding. Receiver
        // initializers and user RHS expressions have already been bound and must
        // not be interpreted as additional coalescing assignments.
        BoundStatement GuardStatement(BoundStatement statement) => statement switch
        {
            BoundExpressionStatement value => new BoundExpressionStatement(GuardNullCoalescingAssignment(value.Expression, syntax)),
            BoundIfStatement conditional => new BoundIfStatement(conditional.Condition,
                GuardStatement(conditional.ThenNode),
                conditional.ElseNode is null ? null : GuardStatement(conditional.ElseNode)),
            _ => statement
        };

        if (expression is BoundBlockExpression block)
            return new BoundBlockExpression(block.Statements.Select(GuardStatement), block.UnitType,
                block.LocalsToDispose, block.IntroduceILScope);

        if (expression is not BoundAssignmentExpression assignment)
            return expression;
        if (assignment.Right is BoundErrorExpression)
            return assignment.Right;

        var target = assignment switch
        {
            BoundFieldAssignmentExpression field => new BoundFieldAccess(field.Receiver, field.Field),
            BoundPropertyAssignmentExpression property => new BoundMemberAccessExpression(property.Receiver, property.Property),
            BoundByRefAssignmentExpression byRef => new BoundDereferenceExpression(byRef.Reference, byRef.ElementType),
            _ => assignment.Left
        };
        // A null pattern uses identity/HasValue semantics, never overloaded ==.
        var nullValue = new BoundLiteralExpression(BoundLiteralExpressionKind.NullLiteral, null!, Compilation.NullTypeSymbol);
        var condition = new BoundIsPatternExpression(target, new BoundConstantPattern(nullValue),
            Compilation.GetSpecialType(SpecialType.System_Boolean));
        var guarded = new BoundBlockExpression([
            new BoundIfStatement(condition, new BoundExpressionStatement(assignment))
        ], assignment.UnitType);
        return StabilizeMutation(guarded, target, syntax);
    }
}
