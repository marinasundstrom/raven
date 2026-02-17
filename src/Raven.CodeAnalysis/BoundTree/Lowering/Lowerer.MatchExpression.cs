using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer
{
    public override BoundNode? VisitMatchStatement(BoundMatchStatement node)
    {
        var compilation = GetCompilation();
        var booleanType = compilation.GetSpecialType(SpecialType.System_Boolean);

        var scrutinee = (BoundExpression)VisitExpression(node.Expression)!;
        var scrutineeType = scrutinee.Type ?? compilation.ErrorTypeSymbol;
        ILocalSymbol scrutineeLocal;

        var statements = new List<BoundStatement>();

        if (scrutinee is BoundLocalAccess localAccess)
        {
            scrutineeLocal = localAccess.Local;
        }
        else
        {
            scrutineeLocal = CreateTempLocal("match_scrutinee", scrutineeType, isMutable: false);
            statements.Add(new BoundLocalDeclarationStatement([
                new BoundVariableDeclarator(scrutineeLocal, scrutinee)
            ]));
        }

        var endLabel = CreateLabel("match_end");
        var needsEndLabel = false;

        for (var armIndex = 0; armIndex < node.Arms.Length; armIndex++)
        {
            var arm = node.Arms[armIndex];
            var pattern = RewriteNullDiscardPattern(arm.Pattern, compilation);
            var guard = (BoundExpression?)VisitExpression(arm.Guard);
            var expression = (BoundExpression)VisitExpression(arm.Expression)!;

            var armStatement = ConvertExpressionToStatement(expression);
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
            var isFinalArm = armIndex == node.Arms.Length - 1;
            if (isFinalArm && guard is null && pattern is BoundDiscardPattern)
            {
                statements.Add(armResult);
                break;
            }

            if (guard is not null)
                armResult = new BoundIfStatement(guard, armResult, null);

            var condition = new BoundIsPatternExpression(
                new BoundLocalAccess(scrutineeLocal),
                pattern,
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

        var scrutinee = (BoundExpression)VisitExpression(node.Expression)!;
        var scrutineeType = scrutinee.Type ?? compilation.ErrorTypeSymbol;
        ILocalSymbol scrutineeLocal;

        var statements = new List<BoundStatement>();

        if (scrutinee is BoundLocalAccess localAccess)
        {
            // Reuse an existing local scrutinee to avoid introducing redundant alias temps.
            scrutineeLocal = localAccess.Local;
        }
        else
        {
            scrutineeLocal = CreateTempLocal("match_scrutinee", scrutineeType, isMutable: false);
            statements.Add(new BoundLocalDeclarationStatement([
                new BoundVariableDeclarator(scrutineeLocal, scrutinee)
            ]));
        }

        var resultType = node.Type ?? compilation.ErrorTypeSymbol;
        var resultLocal = CreateTempLocal("match_result", resultType, isMutable: true);
        statements.Add(new BoundLocalDeclarationStatement([
            new BoundVariableDeclarator(resultLocal, initializer: null)
        ]));

        var endLabel = CreateLabel("match_end");

        foreach (var arm in node.Arms)
        {
            var pattern = RewriteNullDiscardPattern(arm.Pattern, compilation);
            var guard = (BoundExpression?)VisitExpression(arm.Guard);
            var expression = (BoundExpression)VisitExpression(arm.Expression)!;

            expression = ApplyConversionIfNeeded(expression, resultType, compilation);

            var resultLocalAccess = new BoundLocalAccess(resultLocal);

            BoundStatement armResult = new BoundBlockStatement([
                new BoundAssignmentStatement(new BoundLocalAssignmentExpression(resultLocal, resultLocalAccess, expression, unitType)),
                new BoundGotoStatement(endLabel)
            ]);

            if (guard is not null)
            {
                armResult = new BoundIfStatement(guard, armResult, null);
            }

            var condition = new BoundIsPatternExpression(
                new BoundLocalAccess(scrutineeLocal),
                pattern,
                booleanType);

            statements.Add(new BoundIfStatement(condition, armResult, null));
        }

        statements.Add(CreateLabelStatement(endLabel));
        statements.Add(new BoundExpressionStatement(new BoundLocalAccess(resultLocal)));

        return new BoundBlockExpression(statements, unitType);
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
}
