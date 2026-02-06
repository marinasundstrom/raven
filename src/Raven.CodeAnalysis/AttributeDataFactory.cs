using System.Collections.Generic;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal static class AttributeDataFactory
{
    internal static bool TryCreateTypedConstant(BoundExpression expression, out TypedConstant constant)
    {
        if (TryCreateTypedConstantCore(expression, out constant))
            return constant.Kind != TypedConstantKind.Error;

        constant = TypedConstant.CreateError(expression.Type);
        return false;
    }

    internal static AttributeData? Create(BoundExpression? boundExpression, AttributeSyntax attribute)
    {
        if (boundExpression is not BoundObjectCreationExpression creation ||
            creation.Constructor is not IMethodSymbol ctor ||
            ctor.ContainingType is not INamedTypeSymbol attributeType)
        {
            return null;
        }

        var argumentConstants = ImmutableArray.CreateBuilder<TypedConstant>();
        var namedArguments = ImmutableArray.CreateBuilder<KeyValuePair<string, TypedConstant>>();

        foreach (var argument in creation.Arguments)
            argumentConstants.Add(CreateTypedConstant(argument));

        if (creation.Initializer is not null)
        {
            foreach (var entry in creation.Initializer.Entries)
            {
                if (entry is not BoundObjectInitializerAssignmentEntry assignment)
                    continue;

                if (assignment.Member is IPropertySymbol property)
                {
                    var constant = CreateTypedConstant(assignment.Value).WithType(property.Type);
                    namedArguments.Add(new KeyValuePair<string, TypedConstant>(property.Name, constant));
                }
                else if (assignment.Member is IFieldSymbol field)
                {
                    var constant = CreateTypedConstant(assignment.Value).WithType(field.Type);
                    namedArguments.Add(new KeyValuePair<string, TypedConstant>(field.Name, constant));
                }
            }
        }

        return new AttributeData(
            attributeType,
            ctor,
            argumentConstants.ToImmutable(),
            namedArguments.ToImmutable(),
            attribute.GetReference());
    }

    internal static TypedConstant CreateTypedConstant(BoundExpression expression)
    {
        if (TryCreateTypedConstantCore(expression, out var constant))
            return constant;

        return TypedConstant.CreateError(expression.Type);
    }

    private static bool TryCreateTypedConstantCore(BoundExpression expression, out TypedConstant constant)
    {
        switch (expression)
        {
            case BoundLiteralExpression literal:
                {
                    var type = literal.GetConvertedType() ?? literal.Type;

                    if (literal.Kind == BoundLiteralExpressionKind.NullLiteral)
                    {
                        constant = TypedConstant.CreateNull(type);
                        return true;
                    }

                    constant = TypedConstant.CreatePrimitive(type, literal.Value);
                    return true;
                }

            case BoundConversionExpression conversion:
                if (TryCreateTypedConstantCore(conversion.Expression, out var converted))
                {
                    constant = converted.WithType(conversion.Type);
                    return true;
                }
                break;

            case BoundTypeOfExpression typeOfExpression:
                constant = TypedConstant.CreateType(typeOfExpression.SystemType, typeOfExpression.OperandType);
                return true;

            case BoundCollectionExpression collection when collection.Type is IArrayTypeSymbol arrayType:
                {
                    var elements = ImmutableArray.CreateBuilder<TypedConstant>();
                    foreach (var element in collection.Elements)
                    {
                        if (!TryCreateTypedConstantCore(element, out var elementConstant))
                        {
                            constant = TypedConstant.CreateError(arrayType);
                            return false;
                        }

                        elements.Add(elementConstant);
                    }

                    constant = TypedConstant.CreateArray(arrayType, elements.MoveToImmutable());
                    return true;
                }

            case BoundEmptyCollectionExpression emptyCollection:
                constant = TypedConstant.CreateArray(emptyCollection.Type, ImmutableArray<TypedConstant>.Empty);
                return true;

            case BoundFieldAccess fieldAccess when fieldAccess.Field.IsConst:
                constant = TypedConstant.CreatePrimitive(fieldAccess.Type, fieldAccess.Field.GetConstantValue());
                return true;

            case BoundErrorExpression error:
                constant = TypedConstant.CreateError(error.Type);
                return false;
        }

        constant = TypedConstant.CreateError(expression.Type);
        return false;
    }
}
