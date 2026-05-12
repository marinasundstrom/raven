using System;
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

            case BoundFieldAccess fieldAccess when IsConstantField(fieldAccess.Field):
                constant = CreateConstant(fieldAccess.Type, fieldAccess.Field.GetConstantValue());
                return true;

            case BoundMemberAccessExpression { Member: IFieldSymbol field } memberAccess when IsConstantField(field):
                constant = CreateConstant(memberAccess.Type, field.GetConstantValue());
                return true;

            case BoundBinaryExpression binary:
                return TryCreateBinaryConstant(binary, out constant);

            case BoundErrorExpression error:
                constant = TypedConstant.CreateError(error.Type);
                return false;
        }

        constant = TypedConstant.CreateError(expression.Type);
        return false;
    }

    private static TypedConstant CreateConstant(ITypeSymbol? type, object? value)
        => IsEnumType(type)
            ? TypedConstant.CreateEnum(type, value)
            : TypedConstant.CreatePrimitive(type, value);

    private static bool TryCreateBinaryConstant(BoundBinaryExpression binary, out TypedConstant constant)
    {
        if (!TryCreateTypedConstantCore(binary.Left, out var left) ||
            !TryCreateTypedConstantCore(binary.Right, out var right))
        {
            constant = TypedConstant.CreateError(binary.Type);
            return false;
        }

        if (left.Value is null || right.Value is null)
        {
            constant = TypedConstant.CreateError(binary.Type);
            return false;
        }

        var operatorKind = binary.Operator.OperatorKind & ~(BinaryOperatorKind.Lifted | BinaryOperatorKind.Checked);
        var result = operatorKind switch
        {
            BinaryOperatorKind.BitwiseAnd => Convert.ToInt64(left.Value) & Convert.ToInt64(right.Value),
            BinaryOperatorKind.BitwiseOr => Convert.ToInt64(left.Value) | Convert.ToInt64(right.Value),
            BinaryOperatorKind.BitwiseXor => Convert.ToInt64(left.Value) ^ Convert.ToInt64(right.Value),
            _ => (long?)null
        };

        if (result is not { } value)
        {
            constant = TypedConstant.CreateError(binary.Type);
            return false;
        }

        constant = CreateConstant(binary.Type, value);
        return true;
    }

    private static bool IsEnumType(ITypeSymbol? type)
        => type is INamedTypeSymbol { TypeKind: TypeKind.Enum };

    private static bool IsConstantField(IFieldSymbol field)
        => field.IsConst || field.ContainingType?.TypeKind == TypeKind.Enum;
}
