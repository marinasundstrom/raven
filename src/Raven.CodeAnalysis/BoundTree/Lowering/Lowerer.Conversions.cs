using System;
using System.Collections.Generic;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer
{
    public override BoundNode? VisitConversionExpression(BoundConversionExpression node)
    {
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
        var unionType = (INamedTypeSymbol)caseDefinition.Union;

        var discriminatorField = GetRequiredUnionField(unionType, DiscriminatedUnionFieldUtilities.TagFieldName);
        var payloadField = DiscriminatedUnionFieldUtilities.GetRequiredPayloadField(unionType, caseDefinition);

        var caseType = rewrittenExpression.Type ?? compilation.ErrorTypeSymbol;
        var caseLocal = CreateTempLocal("case", caseType, isMutable: true);
        var unionLocal = CreateTempLocal("union", unionType, isMutable: true);

        var statements = new List<BoundStatement>
        {
            new BoundLocalDeclarationStatement(new[] { new BoundVariableDeclarator(caseLocal, rewrittenExpression) }),
            new BoundLocalDeclarationStatement(new[] { new BoundVariableDeclarator(unionLocal, null) }),
            new BoundExpressionStatement(new BoundLocalAssignmentExpression(
                unionLocal,
                new BoundDefaultValueExpression(unionType),
                unitType)),
            new BoundExpressionStatement(new BoundFieldAssignmentExpression(
                new BoundLocalAccess(unionLocal),
                discriminatorField,
                new BoundLiteralExpression(
                    BoundLiteralExpressionKind.NumericLiteral,
                    caseDefinition.Ordinal,
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
