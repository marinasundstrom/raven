using System.Collections.Generic;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal static class AttributeDataFactory
{
    internal static AttributeData? Create(BoundExpression? boundExpression, AttributeSyntax attribute)
    {
        if (boundExpression is not BoundObjectCreationExpression creation ||
            creation.Constructor is not IMethodSymbol ctor ||
            ctor.ContainingType is not INamedTypeSymbol attributeType)
        {
            return null;
        }

        var argumentConstants = ImmutableArray.CreateBuilder<TypedConstant>();

        foreach (var argument in creation.Arguments)
            argumentConstants.Add(CreateTypedConstant(argument));

        return new AttributeData(
            attributeType,
            ctor,
            argumentConstants.ToImmutable(),
            ImmutableArray<KeyValuePair<string, TypedConstant>>.Empty,
            attribute.GetReference());
    }

    internal static TypedConstant CreateTypedConstant(BoundExpression expression)
    {
        switch (expression)
        {
            case BoundLiteralExpression literal:
            {
                var type = literal.GetConvertedType() ?? literal.Type;

                if (literal.Kind == BoundLiteralExpressionKind.NullLiteral)
                    return TypedConstant.CreateNull(type);

                return TypedConstant.CreatePrimitive(type, literal.Value);
            }

            case BoundCastExpression cast:
                return CreateTypedConstant(cast.Expression).WithType(cast.Type);

            case BoundTypeOfExpression typeOfExpression:
                return TypedConstant.CreateType(typeOfExpression.SystemType, typeOfExpression.OperandType);

            case BoundCollectionExpression collection when collection.Type is IArrayTypeSymbol arrayType:
            {
                var elements = ImmutableArray.CreateBuilder<TypedConstant>();
                foreach (var element in collection.Elements)
                    elements.Add(CreateTypedConstant(element));

                return TypedConstant.CreateArray(arrayType, elements.MoveToImmutable());
            }

            case BoundEmptyCollectionExpression emptyCollection:
                return TypedConstant.CreateArray(emptyCollection.Type, ImmutableArray<TypedConstant>.Empty);

            case BoundErrorExpression error:
                return TypedConstant.CreateError(error.Type);
        }

        return TypedConstant.CreateError(expression.Type);
    }
}
