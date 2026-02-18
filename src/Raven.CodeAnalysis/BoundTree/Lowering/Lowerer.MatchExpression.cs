using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer
{
    public override BoundNode? VisitMatchStatement(BoundMatchStatement node)
    {
        var compilation = GetCompilation();
        var booleanType = compilation.GetSpecialType(SpecialType.System_Boolean);
        var rewrittenArms = RewriteMatchArms(node.Arms, compilation);
        var scrutinee = (BoundExpression)VisitExpression(node.Expression)!;

        var statements = new List<BoundStatement>();
        var scrutineeLocal = EnsureMatchScrutineeLocal(scrutinee, statements, compilation);

        var endLabel = CreateLabel("match_end");
        var needsEndLabel = false;

        for (var armIndex = 0; armIndex < rewrittenArms.Length; armIndex++)
        {
            var arm = rewrittenArms[armIndex];

            var armStatement = ConvertExpressionToStatement(arm.Expression);
            BoundStatement armResult = IsTerminatingStatement(armStatement)
                ? armStatement
                : new BoundBlockStatement([
                    armStatement,
                    new BoundGotoStatement(endLabel)
                ]);

            if (!IsTerminatingStatement(armStatement))
                needsEndLabel = true;

            // Final catch-all arm without guard can be emitted directly.
            // This avoids generating redundant always-true condition scaffolding.
            var isFinalArm = armIndex == rewrittenArms.Length - 1;
            if (isFinalArm && arm.Guard is null && arm.Pattern is BoundDiscardPattern)
            {
                statements.Add(armResult);
                break;
            }

            if (arm.Guard is not null)
                armResult = new BoundIfStatement(arm.Guard, armResult, null);

            var condition = new BoundIsPatternExpression(
                new BoundLocalAccess(scrutineeLocal),
                arm.Pattern,
                booleanType);

            statements.Add(new BoundIfStatement(condition, armResult, null));
        }

        if (needsEndLabel)
            statements.Add(CreateLabelStatement(endLabel));
        return new BoundBlockStatement(statements);
    }

    public override BoundNode? VisitMatchExpression(BoundMatchExpression node)
    {
        var compilation = GetCompilation();
        var booleanType = compilation.GetSpecialType(SpecialType.System_Boolean);
        var unitType = compilation.GetSpecialType(SpecialType.System_Unit);
        var rewrittenArms = RewriteMatchArms(node.Arms, compilation);
        var scrutinee = (BoundExpression)VisitExpression(node.Expression)!;

        var statements = new List<BoundStatement>();
        var scrutineeLocal = EnsureMatchScrutineeLocal(scrutinee, statements, compilation);

        var resultType = node.Type ?? compilation.ErrorTypeSymbol;
        var resultLocal = CreateTempLocal("match_result", resultType, isMutable: true);
        statements.Add(new BoundLocalDeclarationStatement([
            new BoundVariableDeclarator(resultLocal, initializer: null)
        ]));

        var endLabel = CreateLabel("match_end");

        foreach (var arm in rewrittenArms)
        {
            var expression = ApplyConversionIfNeeded(arm.Expression, resultType, compilation);

            var resultLocalAccess = new BoundLocalAccess(resultLocal);

            BoundStatement armResult = new BoundBlockStatement([
                new BoundAssignmentStatement(new BoundLocalAssignmentExpression(resultLocal, resultLocalAccess, expression, unitType)),
                new BoundGotoStatement(endLabel)
            ]);

            if (arm.Guard is not null)
            {
                armResult = new BoundIfStatement(arm.Guard, armResult, null);
            }

            var condition = new BoundIsPatternExpression(
                new BoundLocalAccess(scrutineeLocal),
                arm.Pattern,
                booleanType);

            statements.Add(new BoundIfStatement(condition, armResult, null));
        }

        statements.Add(CreateLabelStatement(endLabel));
        statements.Add(new BoundExpressionStatement(new BoundLocalAccess(resultLocal)));

        return new BoundBlockExpression(statements, unitType);
    }

    private ImmutableArray<RewrittenMatchArm> RewriteMatchArms(
        ImmutableArray<BoundMatchArm> arms,
        Compilation compilation)
    {
        var rewrittenArms = ImmutableArray.CreateBuilder<RewrittenMatchArm>(arms.Length);

        foreach (var arm in arms)
        {
            var pattern = RewriteNullDiscardPattern(arm.Pattern, compilation);
            var guard = arm.Guard is null ? null : (BoundExpression?)VisitExpression(arm.Guard);
            var expression = (BoundExpression)VisitExpression(arm.Expression)!;
            rewrittenArms.Add(new RewrittenMatchArm(pattern, guard, expression));
        }

        return rewrittenArms.MoveToImmutable();
    }

    private ILocalSymbol EnsureMatchScrutineeLocal(
        BoundExpression scrutinee,
        List<BoundStatement> statements,
        Compilation compilation)
    {
        if (scrutinee is BoundLocalAccess localAccess)
            return localAccess.Local;

        var scrutineeType = scrutinee.Type ?? compilation.ErrorTypeSymbol;
        var scrutineeLocal = CreateTempLocal("match_scrutinee", scrutineeType, isMutable: false);
        statements.Add(new BoundLocalDeclarationStatement([
            new BoundVariableDeclarator(scrutineeLocal, scrutinee)
        ]));

        return scrutineeLocal;
    }

    private static BoundPattern RewriteNullDiscardPattern(BoundPattern pattern, Compilation compilation)
    {
        if (pattern is BoundDeclarationPattern
            {
                Type: NullTypeSymbol,
                Designator: BoundDiscardDesignator
            } declarationPattern)
        {
            var objectType = compilation.GetSpecialType(SpecialType.System_Object);
            var literalType = new LiteralTypeSymbol(objectType, constantValue: null!, compilation);
            return new BoundConstantPattern(literalType, declarationPattern.Reason);
        }

        return pattern;
    }

    private static BoundStatement ConvertExpressionToStatement(BoundExpression expression)
    {
        return expression switch
        {
            BoundIfExpression ifExpr => new BoundIfStatement(
                ifExpr.Condition,
                ConvertExpressionToStatement(ifExpr.ThenBranch),
                ifExpr.ElseBranch is not null ? ConvertExpressionToStatement(ifExpr.ElseBranch) : null),
            BoundBlockExpression blockExpr => new BoundBlockStatement(blockExpr.Statements, blockExpr.LocalsToDispose),
            BoundReturnExpression returnExpr => new BoundReturnStatement(returnExpr.Expression),
            BoundThrowExpression throwExpr => new BoundThrowStatement(throwExpr.Expression),
            BoundAssignmentExpression assignmentExpr => new BoundAssignmentStatement(assignmentExpr),
            _ => new BoundExpressionStatement(expression),
        };
    }

    private static bool IsTerminatingStatement(BoundStatement statement)
    {
        return statement switch
        {
            BoundReturnStatement => true,
            BoundThrowStatement => true,
            BoundBlockStatement block when block.Statements.Any() => IsTerminatingStatement(block.Statements.Last()),
            BoundIfStatement { ElseNode: not null } nestedIf =>
                IsTerminatingStatement(nestedIf.ThenNode) &&
                IsTerminatingStatement(nestedIf.ElseNode!),
            _ => false,
        };
    }

    private readonly record struct RewrittenMatchArm(
        BoundPattern Pattern,
        BoundExpression? Guard,
        BoundExpression Expression);
}
