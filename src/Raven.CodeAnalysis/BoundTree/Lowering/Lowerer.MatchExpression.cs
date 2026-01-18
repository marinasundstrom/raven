using System.Collections.Generic;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer
{
    public override BoundNode? VisitMatchExpression(BoundMatchExpression node)
    {
        var compilation = GetCompilation();
        var booleanType = compilation.GetSpecialType(SpecialType.System_Boolean);
        var unitType = compilation.GetSpecialType(SpecialType.System_Unit);

        var scrutinee = (BoundExpression)VisitExpression(node.Expression)!;
        var scrutineeType = scrutinee.Type ?? compilation.ErrorTypeSymbol;
        var scrutineeLocal = CreateTempLocal("match_scrutinee", scrutineeType, isMutable: false);

        var statements = new List<BoundStatement>
        {
            new BoundLocalDeclarationStatement([
                new BoundVariableDeclarator(scrutineeLocal, scrutinee)
            ])
        };

        var resultType = node.Type ?? compilation.ErrorTypeSymbol;
        var resultLocal = CreateTempLocal("match_result", resultType, isMutable: true);
        statements.Add(new BoundLocalDeclarationStatement([
            new BoundVariableDeclarator(resultLocal, initializer: null)
        ]));

        var endLabel = CreateLabel("match_end");

        foreach (var arm in node.Arms)
        {
            var pattern = arm.Pattern;
            if (pattern is BoundDeclarationPattern
                {
                    Type: NullTypeSymbol,
                    Designator: BoundDiscardDesignator
                } declarationPattern)
            {
                var objectType = compilation.GetSpecialType(SpecialType.System_Object);
                var literalType = new LiteralTypeSymbol(objectType, constantValue: null!, compilation);
                pattern = new BoundConstantPattern(literalType, declarationPattern.Reason);
            }
            var guard = (BoundExpression?)VisitExpression(arm.Guard);
            var expression = (BoundExpression)VisitExpression(arm.Expression)!;

            expression = ApplyConversionIfNeeded(expression, resultType, compilation);

            BoundStatement armResult = new BoundBlockStatement([
                new BoundAssignmentStatement(new BoundLocalAssignmentExpression(resultLocal, expression, unitType)),
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
}
