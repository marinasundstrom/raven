using System;
using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer
{
    public override BoundNode? VisitConversionExpression(BoundConversionExpression node)
    {
        var lambda = node.Expression as BoundLambdaExpression;
        if (lambda is null &&
            node.Expression is BoundConversionExpression { Expression: BoundLambdaExpression convertedLambda, IsExplicit: false })
        {
            lambda = convertedLambda;
        }

        if (lambda is not null &&
            TryLowerExpressionTreeConversion(node, lambda, out var loweredExpressionTree))
        {
            return loweredExpressionTree;
        }

        var rewrittenExpression = (BoundExpression?)Visit(node.Expression) ?? node.Expression;

        if (node.Conversion.IsDiscriminatedUnion && node.Conversion.MethodSymbol is null)
            return LowerDiscriminatedUnionConversion(node, rewrittenExpression);

        if (ReferenceEquals(rewrittenExpression, node.Expression))
            return node;

        return new BoundConversionExpression(rewrittenExpression, node.Type!, node.Conversion);
    }

    private BoundExpression LowerDiscriminatedUnionConversion(BoundConversionExpression node, BoundExpression rewrittenExpression)
    {
        var compilation = GetCompilation();
        var unitType = compilation.GetSpecialType(SpecialType.System_Unit);
        var caseDefinition = rewrittenExpression.Type?.TryGetDiscriminatedUnionCase()
            ?? throw new InvalidOperationException("Missing discriminated union case information.");
        var unionType = (INamedTypeSymbol)node.Type!;
        if (unionType.IsGenericType &&
            unionType.TypeArguments.Any(static arg => arg is ITypeParameterSymbol))
        {
            unionType = (INamedTypeSymbol)caseDefinition.Union;
        }

        // Reproject the case via the target union to keep synthesized case locals concrete.
        var projectedCase = unionType is IDiscriminatedUnionSymbol projectedUnion
            ? projectedUnion.Cases.FirstOrDefault(c => c.Ordinal == caseDefinition.Ordinal) ?? caseDefinition
            : caseDefinition;

        var discriminatorField = GetRequiredUnionField(unionType, DiscriminatedUnionFieldUtilities.TagFieldName);
        var payloadField = DiscriminatedUnionFieldUtilities.GetRequiredPayloadField(unionType, projectedCase);

        var caseType = rewrittenExpression.Type;
        if (caseType is null || caseType.TypeKind == TypeKind.Error)
            caseType = (ITypeSymbol)projectedCase;
        var caseLocal = CreateTempLocal("case", caseType, isMutable: true);
        var unionLocal = CreateTempLocal("union", unionType, isMutable: true);

        var unionLocalLocalAccess = new BoundLocalAccess(unionLocal);

        var statements = new List<BoundStatement>
        {
            new BoundLocalDeclarationStatement(new[] { new BoundVariableDeclarator(caseLocal, rewrittenExpression) }),
            new BoundLocalDeclarationStatement(new[] { new BoundVariableDeclarator(unionLocal, null) }),
            new BoundExpressionStatement(new BoundLocalAssignmentExpression(
                unionLocal,
                unionLocalLocalAccess,
                new BoundDefaultValueExpression(unionType),
                unitType)),
            new BoundExpressionStatement(new BoundFieldAssignmentExpression(
                new BoundLocalAccess(unionLocal),
                discriminatorField,
                new BoundLiteralExpression(
                    BoundLiteralExpressionKind.NumericLiteral,
                    projectedCase.Ordinal,
                    discriminatorField.Type),
                unitType,
                requiresReceiverAddress: true)),
            new BoundExpressionStatement(new BoundFieldAssignmentExpression(
                new BoundLocalAccess(unionLocal),
                payloadField,
                new BoundLocalAccess(caseLocal),
                unitType,
                requiresReceiverAddress: true)),
            new BoundExpressionStatement(new BoundLocalAccess(unionLocal))
        };

        return new BoundBlockExpression(statements, unitType);
    }

    private static IFieldSymbol GetRequiredUnionField(INamedTypeSymbol unionType, string fieldName)
    {
        foreach (var member in unionType.GetMembers(fieldName))
        {
            if (member is IFieldSymbol field)
                return field;
        }

        throw new InvalidOperationException($"Union '{unionType.Name}' is missing backing field '{fieldName}'.");
    }
}
