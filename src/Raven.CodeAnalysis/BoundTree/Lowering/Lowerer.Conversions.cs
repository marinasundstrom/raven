using System;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer
{
    public override BoundNode? VisitConversionExpression(BoundConversionExpression node)
    {
        var lambda = node.Expression as BoundFunctionExpression;
        if (lambda is null &&
            node.Expression is BoundConversionExpression { Expression: BoundFunctionExpression convertedLambda, IsExplicit: false })
        {
            lambda = convertedLambda;
        }

        if (lambda is not null &&
            TryLowerExpressionTreeConversion(node, lambda, out var loweredExpressionTree))
        {
            return loweredExpressionTree;
        }

        var rewrittenExpression = (BoundExpression?)Visit(node.Expression) ?? node.Expression;

        if (node.Conversion.IsUnion)
        {
            if (ReferenceEquals(rewrittenExpression, node.Expression))
                return node;

            return new BoundConversionExpression(rewrittenExpression, node.Type!, node.Conversion);
        }

        if (ReferenceEquals(rewrittenExpression, node.Expression))
            return node;

        return new BoundConversionExpression(rewrittenExpression, node.Type!, node.Conversion);
    }

    private BoundExpression LowerDiscriminatedUnionConversion(BoundConversionExpression node, BoundExpression rewrittenExpression)
    {
        var caseDefinition = rewrittenExpression.Type?.TryGetUnionCase()
            ?? throw new InvalidOperationException("Missing discriminated union case information.");
        var unionType = (INamedTypeSymbol)node.Type!;
        if (unionType.IsGenericType &&
            unionType.TypeArguments.Any(static arg => arg is ITypeParameterSymbol))
        {
            unionType = (INamedTypeSymbol)caseDefinition.Union;
        }

        // Reproject the case via the target union to keep synthesized case locals concrete.
        var projectedCase = unionType is IUnionSymbol projectedUnion
            ? projectedUnion.CaseTypes.FirstOrDefault(c => c.Ordinal == caseDefinition.Ordinal) ?? caseDefinition
            : caseDefinition;

        var projectedCaseType = (INamedTypeSymbol)projectedCase;
        var unionCtor = node.Conversion.ConstructorSymbol;
        if (unionCtor is null &&
            !unionType.TryGetUnionCarrierConstructor(projectedCaseType, out unionCtor))
        {
            throw new InvalidOperationException(
                $"Missing union constructor for DU conversion from '{projectedCaseType.Name}' to '{unionType.Name}'.");
        }

        return new BoundObjectCreationExpression(
            unionCtor,
            ImmutableArray.Create(rewrittenExpression));
    }
}
