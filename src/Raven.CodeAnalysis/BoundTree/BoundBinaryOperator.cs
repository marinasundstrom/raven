using System;
using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal partial class BoundBinaryOperator
{
    public BinaryOperatorKind OperatorKind { get; }
    public ITypeSymbol LeftType { get; }
    public ITypeSymbol RightType { get; }
    public ITypeSymbol ResultType { get; }

    private BoundBinaryOperator(
        BinaryOperatorKind operatorKind,
        ITypeSymbol left,
        ITypeSymbol right,
        ITypeSymbol result)
    {
        OperatorKind = operatorKind;
        LeftType = left;
        RightType = right;
        ResultType = result;
    }

    public static bool TryLookup(Compilation compilation, SyntaxKind kind, ITypeSymbol left, ITypeSymbol right, out BoundBinaryOperator op)
    {
        if (left is LiteralTypeSymbol litLeft)
            left = litLeft.UnderlyingType;
        if (right is LiteralTypeSymbol litRight)
            right = litRight.UnderlyingType;

        // Nullable reference types are represented with a wrapper symbol in Raven.
        // For built-in operator lookup they should behave like their underlying reference type.
        if (left is NullableTypeSymbol { UnderlyingType: { IsValueType: false } leftUnderlying })
            left = leftUnderlying;

        if (right is NullableTypeSymbol { UnderlyingType: { IsValueType: false } rightUnderlying })
            right = rightUnderlying;

        if (left is ErrorTypeSymbol || right is ErrorTypeSymbol)
        {
            op = new BoundBinaryOperator(BinaryOperatorKind.None, compilation.ErrorTypeSymbol, compilation.ErrorTypeSymbol, compilation.ErrorTypeSymbol);
            return false;
        }

        var nintType = compilation.GetSpecialType(SpecialType.System_IntPtr);
        var boolType = compilation.GetSpecialType(SpecialType.System_Boolean);

        if (TryCreatePointerArithmetic(kind, left, right, nintType, out op))
            return true;

        var leftEnumLike = left is INamedTypeSymbol leftNamed && (leftNamed.TypeKind == TypeKind.Enum || leftNamed.EnumUnderlyingType is not null);
        var rightEnumLike = right is INamedTypeSymbol rightNamed && (rightNamed.TypeKind == TypeKind.Enum || rightNamed.EnumUnderlyingType is not null);

        if (leftEnumLike && rightEnumLike)
        {
            if (AreEquivalentEnumTypes(left, right))
            {
                if (kind == SyntaxKind.EqualsEqualsToken)
                {
                    op = new BoundBinaryOperator(BinaryOperatorKind.Equality, left, right, boolType);
                    return true;
                }

                if (kind == SyntaxKind.NotEqualsToken)
                {
                    op = new BoundBinaryOperator(BinaryOperatorKind.Inequality, left, right, boolType);
                    return true;
                }

                if (kind == SyntaxKind.AmpersandToken)
                {
                    op = new BoundBinaryOperator(BinaryOperatorKind.BitwiseAnd, left, right, left);
                    return true;
                }

                if (kind == SyntaxKind.BarToken)
                {
                    op = new BoundBinaryOperator(BinaryOperatorKind.BitwiseOr, left, right, left);
                    return true;
                }

                if (kind == SyntaxKind.CaretToken)
                {
                    op = new BoundBinaryOperator(BinaryOperatorKind.BitwiseXor, left, right, left);
                    return true;
                }
            }
        }

        if (TryLookupPredefinedOperator(compilation, kind, left, right, boolType, out op))
            return true;

        if (TryLookupTypeParameterConstraintOperator(kind, left, right, boolType, out op))
            return true;

        if (TryLookupConstrainedInterfaceOperator(kind, left, right, out op))
            return true;

        // Reference equality/inequality (C#-style: same reference type)
        if (left.IsReferenceType &&
            right.IsReferenceType &&
            SymbolEqualityComparer.Default.Equals(left, right))
        {
            if (kind == SyntaxKind.EqualsEqualsToken)
            {
                op = new BoundBinaryOperator(BinaryOperatorKind.Equality, left, right, boolType);
                return true;
            }

            if (kind == SyntaxKind.NotEqualsToken)
            {
                op = new BoundBinaryOperator(BinaryOperatorKind.Inequality, left, right, boolType);
                return true;
            }
        }

        // Null literal equality/inequality for nullable/reference operands.
        if (kind is SyntaxKind.EqualsEqualsToken or SyntaxKind.NotEqualsToken)
        {
            if (left.TypeKind == TypeKind.Null && (right.IsReferenceType || right.IsNullable))
            {
                op = new BoundBinaryOperator(
                    kind == SyntaxKind.EqualsEqualsToken ? BinaryOperatorKind.Equality : BinaryOperatorKind.Inequality,
                    left,
                    right,
                    boolType);
                return true;
            }

            if (right.TypeKind == TypeKind.Null && (left.IsReferenceType || left.IsNullable))
            {
                op = new BoundBinaryOperator(
                    kind == SyntaxKind.EqualsEqualsToken ? BinaryOperatorKind.Equality : BinaryOperatorKind.Inequality,
                    left,
                    right,
                    boolType);
                return true;
            }
        }

        // Try lifting for nullable/nullable.
        if (left.IsNullable && right.IsNullable)
        {
            var underlyingLeft = left.GetNullableUnderlyingType() ?? left;
            var underlyingRight = right.GetNullableUnderlyingType() ?? right;

            if (TryLookup(compilation, kind, underlyingLeft, underlyingRight, out var lifted))
            {
                var liftedKind = lifted.OperatorKind & ~(BinaryOperatorKind.Lifted | BinaryOperatorKind.Checked);
                var resultType = liftedKind is BinaryOperatorKind.Equality or BinaryOperatorKind.Inequality
                    ? boolType
                    : lifted.ResultType.MakeNullable();

                op = new BoundBinaryOperator(
                    lifted.OperatorKind | BinaryOperatorKind.Lifted,
                    left,
                    right,
                    resultType);
                return true;
            }
        }

        // Try lifting for nullable-value/non-nullable and non-nullable/nullable-value.
        // Example: int? == int, int == int?, FooStruct? != FooStruct.
        if (kind is SyntaxKind.EqualsEqualsToken or SyntaxKind.NotEqualsToken)
        {
            if (left is NullableTypeSymbol { UnderlyingType: { IsValueType: true } leftValueUnderlying } &&
                right is not NullableTypeSymbol &&
                TryLookup(compilation, kind, leftValueUnderlying, right, out var leftLiftedBase))
            {
                var liftedBaseKind = leftLiftedBase.OperatorKind & ~(BinaryOperatorKind.Lifted | BinaryOperatorKind.Checked);
                if (liftedBaseKind is BinaryOperatorKind.Equality or BinaryOperatorKind.Inequality)
                {
                    op = new BoundBinaryOperator(
                        liftedBaseKind | BinaryOperatorKind.Lifted,
                        left,
                        right,
                        boolType);
                    return true;
                }
            }

            if (right is NullableTypeSymbol { UnderlyingType: { IsValueType: true } rightValueUnderlying } &&
                left is not NullableTypeSymbol &&
                TryLookup(compilation, kind, left, rightValueUnderlying, out var rightLiftedBase))
            {
                var liftedBaseKind = rightLiftedBase.OperatorKind & ~(BinaryOperatorKind.Lifted | BinaryOperatorKind.Checked);
                if (liftedBaseKind is BinaryOperatorKind.Equality or BinaryOperatorKind.Inequality)
                {
                    op = new BoundBinaryOperator(
                        liftedBaseKind | BinaryOperatorKind.Lifted,
                        left,
                        right,
                        boolType);
                    return true;
                }
            }
        }

        op = new BoundBinaryOperator(BinaryOperatorKind.None, compilation.ErrorTypeSymbol, compilation.ErrorTypeSymbol, compilation.ErrorTypeSymbol);
        return false;
    }

    private static bool TryLookupPredefinedOperator(
        Compilation compilation,
        SyntaxKind kind,
        ITypeSymbol left,
        ITypeSymbol right,
        ITypeSymbol boolType,
        out BoundBinaryOperator op)
    {
        if (TryLookupBooleanOperator(kind, left, right, boolType, out op))
            return true;

        if (TryLookupShiftOperator(compilation, kind, left, right, out op))
            return true;

        if (!TryApplyBinaryNumericPromotion(compilation, kind, left, right, out var promotedLeft, out var promotedRight))
        {
            op = default!;
            return false;
        }

        var leftSpecialType = promotedLeft.SpecialType;
        var rightSpecialType = promotedRight.SpecialType;
        if (leftSpecialType == SpecialType.None || rightSpecialType == SpecialType.None || leftSpecialType != rightSpecialType)
        {
            op = default!;
            return false;
        }

        switch (kind)
        {
            case SyntaxKind.PlusToken:
                if (IsArithmeticNumericType(leftSpecialType))
                {
                    op = new BoundBinaryOperator(BinaryOperatorKind.Addition, promotedLeft, promotedRight, promotedLeft);
                    return true;
                }
                break;

            case SyntaxKind.MinusToken:
                if (IsArithmeticNumericType(leftSpecialType))
                {
                    op = new BoundBinaryOperator(BinaryOperatorKind.Subtraction, promotedLeft, promotedRight, promotedLeft);
                    return true;
                }
                break;

            case SyntaxKind.StarToken:
                if (IsArithmeticNumericType(leftSpecialType))
                {
                    op = new BoundBinaryOperator(BinaryOperatorKind.Multiplication, promotedLeft, promotedRight, promotedLeft);
                    return true;
                }
                break;

            case SyntaxKind.SlashToken:
                if (IsArithmeticNumericType(leftSpecialType))
                {
                    op = new BoundBinaryOperator(BinaryOperatorKind.Division, promotedLeft, promotedRight, promotedLeft);
                    return true;
                }
                break;

            case SyntaxKind.PercentToken:
                if (IsArithmeticNumericType(leftSpecialType))
                {
                    op = new BoundBinaryOperator(BinaryOperatorKind.Modulo, promotedLeft, promotedRight, promotedLeft);
                    return true;
                }
                break;

            case SyntaxKind.EqualsEqualsToken:
                if (IsEqualityComparablePredefinedType(leftSpecialType))
                {
                    op = new BoundBinaryOperator(BinaryOperatorKind.Equality, promotedLeft, promotedRight, boolType);
                    return true;
                }
                break;

            case SyntaxKind.NotEqualsToken:
                if (IsEqualityComparablePredefinedType(leftSpecialType))
                {
                    op = new BoundBinaryOperator(BinaryOperatorKind.Inequality, promotedLeft, promotedRight, boolType);
                    return true;
                }
                break;

            case SyntaxKind.GreaterThanToken:
                if (IsOrderedNumericType(leftSpecialType))
                {
                    op = new BoundBinaryOperator(BinaryOperatorKind.GreaterThan, promotedLeft, promotedRight, boolType);
                    return true;
                }
                break;

            case SyntaxKind.LessThanToken:
                if (IsOrderedNumericType(leftSpecialType))
                {
                    op = new BoundBinaryOperator(BinaryOperatorKind.LessThan, promotedLeft, promotedRight, boolType);
                    return true;
                }
                break;

            case SyntaxKind.GreaterThanOrEqualsToken:
                if (IsOrderedNumericType(leftSpecialType))
                {
                    op = new BoundBinaryOperator(BinaryOperatorKind.GreaterThanOrEqual, promotedLeft, promotedRight, boolType);
                    return true;
                }
                break;

            case SyntaxKind.LessThanOrEqualsToken:
                if (IsOrderedNumericType(leftSpecialType))
                {
                    op = new BoundBinaryOperator(BinaryOperatorKind.LessThanOrEqual, promotedLeft, promotedRight, boolType);
                    return true;
                }
                break;

            case SyntaxKind.AmpersandToken:
                if (IsBitwiseIntegralType(leftSpecialType))
                {
                    op = new BoundBinaryOperator(BinaryOperatorKind.BitwiseAnd, promotedLeft, promotedRight, promotedLeft);
                    return true;
                }
                break;

            case SyntaxKind.BarToken:
                if (IsBitwiseIntegralType(leftSpecialType))
                {
                    op = new BoundBinaryOperator(BinaryOperatorKind.BitwiseOr, promotedLeft, promotedRight, promotedLeft);
                    return true;
                }
                break;

            case SyntaxKind.CaretToken:
                if (IsBitwiseIntegralType(leftSpecialType))
                {
                    op = new BoundBinaryOperator(BinaryOperatorKind.BitwiseXor, promotedLeft, promotedRight, promotedLeft);
                    return true;
                }
                break;
        }

        op = default!;
        return false;
    }

    private static bool TryLookupBooleanOperator(
        SyntaxKind kind,
        ITypeSymbol left,
        ITypeSymbol right,
        ITypeSymbol boolType,
        out BoundBinaryOperator op)
    {
        if (left.SpecialType != SpecialType.System_Boolean || right.SpecialType != SpecialType.System_Boolean)
        {
            op = default!;
            return false;
        }

        switch (kind)
        {
            case SyntaxKind.AmpersandToken:
                op = new BoundBinaryOperator(BinaryOperatorKind.BitwiseAnd, boolType, boolType, boolType);
                return true;

            case SyntaxKind.BarToken:
                op = new BoundBinaryOperator(BinaryOperatorKind.BitwiseOr, boolType, boolType, boolType);
                return true;

            case SyntaxKind.CaretToken:
                op = new BoundBinaryOperator(BinaryOperatorKind.BitwiseXor, boolType, boolType, boolType);
                return true;

            case SyntaxKind.AmpersandAmpersandToken:
                op = new BoundBinaryOperator(BinaryOperatorKind.LogicalAnd, boolType, boolType, boolType);
                return true;

            case SyntaxKind.BarBarToken:
                op = new BoundBinaryOperator(BinaryOperatorKind.LogicalOr, boolType, boolType, boolType);
                return true;

            case SyntaxKind.EqualsEqualsToken:
                op = new BoundBinaryOperator(BinaryOperatorKind.Equality, boolType, boolType, boolType);
                return true;

            case SyntaxKind.NotEqualsToken:
                op = new BoundBinaryOperator(BinaryOperatorKind.Inequality, boolType, boolType, boolType);
                return true;
        }

        op = default!;
        return false;
    }

    private static bool TryLookupConstrainedInterfaceOperator(
        SyntaxKind kind,
        ITypeSymbol left,
        ITypeSymbol right,
        out BoundBinaryOperator op)
    {
        foreach (var candidate in EnumerateOperatorInterfaceCandidates(left))
        {
            if (candidate.TypeArguments.IsDefaultOrEmpty || candidate.TypeArguments.Length != 3)
                continue;

            if (!MatchesConstrainedOperatorOperand(candidate.TypeArguments[0], left) ||
                !MatchesConstrainedOperatorOperand(candidate.TypeArguments[1], right))
            {
                continue;
            }

            if (!TryMatchConstrainedOperator(kind, candidate, out var operatorKind))
                continue;

            op = new BoundBinaryOperator(
                operatorKind,
                left,
                right,
                GetConstrainedOperatorResultType(operatorKind, candidate.TypeArguments[2], left, right));
            return true;
        }

        op = default!;
        return false;
    }

    private static bool TryLookupTypeParameterConstraintOperator(
        SyntaxKind kind,
        ITypeSymbol left,
        ITypeSymbol right,
        ITypeSymbol boolType,
        out BoundBinaryOperator op)
    {
        if (left is not ITypeParameterSymbol leftTypeParameter ||
            right is not ITypeParameterSymbol rightTypeParameter ||
            !AreEquivalentTypeParameterOperands(leftTypeParameter, rightTypeParameter))
        {
            op = default!;
            return false;
        }

        if (!TryMatchTypeParameterConstraintOperator(kind, leftTypeParameter, out var operatorKind))
        {
            op = default!;
            return false;
        }

        var resultType = operatorKind is BinaryOperatorKind.Equality or
            BinaryOperatorKind.Inequality or
            BinaryOperatorKind.GreaterThan or
            BinaryOperatorKind.LessThan or
            BinaryOperatorKind.GreaterThanOrEqual or
            BinaryOperatorKind.LessThanOrEqual
            ? boolType
            : left;

        op = new BoundBinaryOperator(operatorKind, left, right, resultType);
        return true;
    }

    private static bool TryMatchTypeParameterConstraintOperator(
        SyntaxKind kind,
        ITypeParameterSymbol typeParameter,
        out BinaryOperatorKind operatorKind)
    {
        operatorKind = BinaryOperatorKind.None;

        return kind switch
        {
            SyntaxKind.PlusToken when HasTypeParameterOperatorConstraint(typeParameter, "INumber", "IAdditionOperators") => Set(BinaryOperatorKind.Addition, out operatorKind),
            SyntaxKind.MinusToken when HasTypeParameterOperatorConstraint(typeParameter, "INumber", "ISubtractionOperators") => Set(BinaryOperatorKind.Subtraction, out operatorKind),
            SyntaxKind.StarToken when HasTypeParameterOperatorConstraint(typeParameter, "INumber", "IMultiplyOperators") => Set(BinaryOperatorKind.Multiplication, out operatorKind),
            SyntaxKind.SlashToken when HasTypeParameterOperatorConstraint(typeParameter, "INumber", "IDivisionOperators") => Set(BinaryOperatorKind.Division, out operatorKind),
            SyntaxKind.PercentToken when HasTypeParameterOperatorConstraint(typeParameter, "INumber", "IModulusOperators") => Set(BinaryOperatorKind.Modulo, out operatorKind),
            SyntaxKind.EqualsEqualsToken when HasTypeParameterOperatorConstraint(typeParameter, "INumber", "IEqualityOperators") => Set(BinaryOperatorKind.Equality, out operatorKind),
            SyntaxKind.NotEqualsToken when HasTypeParameterOperatorConstraint(typeParameter, "INumber", "IEqualityOperators") => Set(BinaryOperatorKind.Inequality, out operatorKind),
            SyntaxKind.GreaterThanToken when HasTypeParameterOperatorConstraint(typeParameter, "INumber", "IComparisonOperators") => Set(BinaryOperatorKind.GreaterThan, out operatorKind),
            SyntaxKind.LessThanToken when HasTypeParameterOperatorConstraint(typeParameter, "INumber", "IComparisonOperators") => Set(BinaryOperatorKind.LessThan, out operatorKind),
            SyntaxKind.GreaterThanOrEqualsToken when HasTypeParameterOperatorConstraint(typeParameter, "INumber", "IComparisonOperators") => Set(BinaryOperatorKind.GreaterThanOrEqual, out operatorKind),
            SyntaxKind.LessThanOrEqualsToken when HasTypeParameterOperatorConstraint(typeParameter, "INumber", "IComparisonOperators") => Set(BinaryOperatorKind.LessThanOrEqual, out operatorKind),
            SyntaxKind.AmpersandToken when HasTypeParameterOperatorConstraint(typeParameter, "IBitwiseOperators") => Set(BinaryOperatorKind.BitwiseAnd, out operatorKind),
            SyntaxKind.BarToken when HasTypeParameterOperatorConstraint(typeParameter, "IBitwiseOperators") => Set(BinaryOperatorKind.BitwiseOr, out operatorKind),
            SyntaxKind.CaretToken when HasTypeParameterOperatorConstraint(typeParameter, "IBitwiseOperators") => Set(BinaryOperatorKind.BitwiseXor, out operatorKind),
            SyntaxKind.LessThanLessThanToken when HasTypeParameterOperatorConstraint(typeParameter, "IShiftOperators") => Set(BinaryOperatorKind.ShiftLeft, out operatorKind),
            SyntaxKind.GreaterThanGreaterThanToken when HasTypeParameterOperatorConstraint(typeParameter, "IShiftOperators") => Set(BinaryOperatorKind.ShiftRight, out operatorKind),
            _ => false,
        };

        static bool Set(BinaryOperatorKind value, out BinaryOperatorKind destination)
        {
            destination = value;
            return true;
        }
    }

    private static bool HasTypeParameterOperatorConstraint(
        ITypeParameterSymbol typeParameter,
        params string[] interfaceNames)
    {
        foreach (var constraintType in typeParameter.ConstraintTypes)
        {
            if (constraintType is not INamedTypeSymbol namedConstraint)
                continue;

            if (IsMatchingSystemNumericsConstraint(namedConstraint, interfaceNames))
                return true;

            foreach (var interfaceType in namedConstraint.AllInterfaces)
            {
                if (IsMatchingSystemNumericsConstraint(interfaceType, interfaceNames))
                    return true;
            }
        }

        return false;
    }

    private static bool AreEquivalentTypeParameterOperands(
        ITypeParameterSymbol left,
        ITypeParameterSymbol right)
    {
        if (SymbolEqualityComparer.Default.Equals(left, right))
            return true;

        if (left.MetadataIdentityEquals(right))
            return true;

        return string.Equals(left.Name, right.Name, StringComparison.Ordinal) &&
               left.OwnerKind == right.OwnerKind;
    }

    private static bool IsMatchingSystemNumericsConstraint(INamedTypeSymbol type, string[] interfaceNames)
    {
        var definition = (type.OriginalDefinition as INamedTypeSymbol) ?? type;
        if (!IsNamespace(definition.ContainingNamespace, "System", "Numerics"))
            return false;

        foreach (var interfaceName in interfaceNames)
        {
            if (string.Equals(definition.Name, interfaceName, StringComparison.Ordinal))
                return true;
        }

        return false;
    }

    private static bool TryMatchConstrainedOperator(
        SyntaxKind kind,
        INamedTypeSymbol candidate,
        out BinaryOperatorKind operatorKind)
    {
        operatorKind = BinaryOperatorKind.None;

        switch (kind)
        {
            case SyntaxKind.PlusToken when IsSystemNumericsOperatorInterface(candidate, "IAdditionOperators", 3):
                operatorKind = BinaryOperatorKind.Addition;
                return true;

            case SyntaxKind.MinusToken when IsSystemNumericsOperatorInterface(candidate, "ISubtractionOperators", 3):
                operatorKind = BinaryOperatorKind.Subtraction;
                return true;

            case SyntaxKind.StarToken when IsSystemNumericsOperatorInterface(candidate, "IMultiplyOperators", 3):
                operatorKind = BinaryOperatorKind.Multiplication;
                return true;

            case SyntaxKind.SlashToken when IsSystemNumericsOperatorInterface(candidate, "IDivisionOperators", 3):
                operatorKind = BinaryOperatorKind.Division;
                return true;

            case SyntaxKind.PercentToken when IsSystemNumericsOperatorInterface(candidate, "IModulusOperators", 3):
                operatorKind = BinaryOperatorKind.Modulo;
                return true;

            case SyntaxKind.EqualsEqualsToken when IsSystemNumericsOperatorInterface(candidate, "IEqualityOperators", 3):
                operatorKind = BinaryOperatorKind.Equality;
                return true;

            case SyntaxKind.NotEqualsToken when IsSystemNumericsOperatorInterface(candidate, "IEqualityOperators", 3):
                operatorKind = BinaryOperatorKind.Inequality;
                return true;

            case SyntaxKind.GreaterThanToken when IsSystemNumericsOperatorInterface(candidate, "IComparisonOperators", 3):
                operatorKind = BinaryOperatorKind.GreaterThan;
                return true;

            case SyntaxKind.LessThanToken when IsSystemNumericsOperatorInterface(candidate, "IComparisonOperators", 3):
                operatorKind = BinaryOperatorKind.LessThan;
                return true;

            case SyntaxKind.GreaterThanOrEqualsToken when IsSystemNumericsOperatorInterface(candidate, "IComparisonOperators", 3):
                operatorKind = BinaryOperatorKind.GreaterThanOrEqual;
                return true;

            case SyntaxKind.LessThanOrEqualsToken when IsSystemNumericsOperatorInterface(candidate, "IComparisonOperators", 3):
                operatorKind = BinaryOperatorKind.LessThanOrEqual;
                return true;

            case SyntaxKind.AmpersandToken when IsSystemNumericsOperatorInterface(candidate, "IBitwiseOperators", 3):
                operatorKind = BinaryOperatorKind.BitwiseAnd;
                return true;

            case SyntaxKind.BarToken when IsSystemNumericsOperatorInterface(candidate, "IBitwiseOperators", 3):
                operatorKind = BinaryOperatorKind.BitwiseOr;
                return true;

            case SyntaxKind.CaretToken when IsSystemNumericsOperatorInterface(candidate, "IBitwiseOperators", 3):
                operatorKind = BinaryOperatorKind.BitwiseXor;
                return true;

            case SyntaxKind.LessThanLessThanToken when IsSystemNumericsOperatorInterface(candidate, "IShiftOperators", 3):
                operatorKind = BinaryOperatorKind.ShiftLeft;
                return true;

            case SyntaxKind.GreaterThanGreaterThanToken when IsSystemNumericsOperatorInterface(candidate, "IShiftOperators", 3):
                operatorKind = BinaryOperatorKind.ShiftRight;
                return true;

            default:
                return false;
        }
    }

    private static bool IsSystemNumericsOperatorInterface(INamedTypeSymbol type, string name, int arity)
    {
        var definition = (type.OriginalDefinition as INamedTypeSymbol) ?? type;
        if (!string.Equals(definition.Name, name, StringComparison.Ordinal) || definition.Arity != arity)
            return false;

        return IsNamespace(definition.ContainingNamespace, "System", "Numerics");
    }

    private static bool IsNamespace(INamespaceSymbol? ns, params string[] segments)
    {
        if (ns is null)
            return false;

        for (int i = segments.Length - 1; i >= 0; i--)
        {
            if (ns.IsGlobalNamespace || !string.Equals(ns.Name, segments[i], StringComparison.Ordinal))
                return false;

            ns = ns.ContainingNamespace;
        }

        return ns is { IsGlobalNamespace: true };
    }

    private static bool MatchesConstrainedOperatorOperand(ITypeSymbol candidateOperand, ITypeSymbol actualOperand)
    {
        if (SymbolEqualityComparer.Default.Equals(candidateOperand, actualOperand))
            return true;

        if (candidateOperand.MetadataIdentityEquals(actualOperand))
            return true;

        return candidateOperand is ITypeParameterSymbol && actualOperand is ITypeParameterSymbol;
    }

    private static ITypeSymbol GetConstrainedOperatorResultType(
        BinaryOperatorKind operatorKind,
        ITypeSymbol candidateResult,
        ITypeSymbol left,
        ITypeSymbol right)
    {
        if (operatorKind is BinaryOperatorKind.Equality or
            BinaryOperatorKind.Inequality or
            BinaryOperatorKind.GreaterThan or
            BinaryOperatorKind.LessThan or
            BinaryOperatorKind.GreaterThanOrEqual or
            BinaryOperatorKind.LessThanOrEqual)
        {
            return candidateResult;
        }

        if (MatchesConstrainedOperatorOperand(candidateResult, left))
            return left;

        if (MatchesConstrainedOperatorOperand(candidateResult, right))
            return right;

        return candidateResult;
    }

    private static System.Collections.Generic.IEnumerable<INamedTypeSymbol> EnumerateOperatorInterfaceCandidates(ITypeSymbol type)
    {
        if (type is INamedTypeSymbol namedType)
        {
            yield return namedType;

            foreach (var interfaceType in namedType.AllInterfaces)
                yield return interfaceType;

            yield break;
        }

        if (type is ITypeParameterSymbol typeParameter)
        {
            foreach (var interfaceType in typeParameter.AllInterfaces)
                yield return interfaceType;
        }
    }

    private static bool TryLookupShiftOperator(
        Compilation compilation,
        SyntaxKind kind,
        ITypeSymbol left,
        ITypeSymbol right,
        out BoundBinaryOperator op)
    {
        if (kind is not (SyntaxKind.LessThanLessThanToken or SyntaxKind.GreaterThanGreaterThanToken))
        {
            op = default!;
            return false;
        }

        if (!TryPromoteShiftLeftOperand(compilation, left, out var promotedLeft) ||
            !TryPromoteShiftRightOperand(compilation, right, out var promotedRight))
        {
            op = default!;
            return false;
        }

        op = new BoundBinaryOperator(
            kind == SyntaxKind.LessThanLessThanToken ? BinaryOperatorKind.ShiftLeft : BinaryOperatorKind.ShiftRight,
            promotedLeft,
            promotedRight,
            promotedLeft);
        return true;
    }

    private static bool TryCreatePointerArithmetic(
        SyntaxKind kind,
        ITypeSymbol left,
        ITypeSymbol right,
        ITypeSymbol nativeIntType,
        out BoundBinaryOperator op)
    {
        var leftPointer = left as IPointerTypeSymbol;
        var rightPointer = right as IPointerTypeSymbol;

        if (kind == SyntaxKind.PlusToken)
        {
            if (leftPointer is not null && IsIntegralType(right))
            {
                op = new BoundBinaryOperator(BinaryOperatorKind.Addition, left, right, left);
                return true;
            }

            if (rightPointer is not null && IsIntegralType(left))
            {
                op = new BoundBinaryOperator(BinaryOperatorKind.Addition, left, right, right);
                return true;
            }
        }

        if (kind == SyntaxKind.MinusToken)
        {
            if (leftPointer is not null && IsIntegralType(right))
            {
                op = new BoundBinaryOperator(BinaryOperatorKind.Subtraction, left, right, left);
                return true;
            }

            if (leftPointer is not null &&
                rightPointer is not null &&
                SymbolEqualityComparer.Default.Equals(leftPointer.PointedAtType, rightPointer.PointedAtType))
            {
                op = new BoundBinaryOperator(BinaryOperatorKind.Subtraction, left, right, nativeIntType);
                return true;
            }
        }

        op = new BoundBinaryOperator(BinaryOperatorKind.None, left, right, left);
        return false;
    }

    private static bool IsIntegralType(ITypeSymbol type)
    {
        if (type is LiteralTypeSymbol literal)
            type = literal.UnderlyingType;

        return type.SpecialType switch
        {
            SpecialType.System_SByte => true,
            SpecialType.System_Byte => true,
            SpecialType.System_Int16 => true,
            SpecialType.System_UInt16 => true,
            SpecialType.System_Int32 => true,
            SpecialType.System_UInt32 => true,
            SpecialType.System_Int64 => true,
            SpecialType.System_UInt64 => true,
            SpecialType.System_Char => true,
            SpecialType.System_IntPtr => true,
            SpecialType.System_UIntPtr => true,
            _ => false,
        };
    }

    private static bool TryApplyBinaryNumericPromotion(
        Compilation compilation,
        SyntaxKind kind,
        ITypeSymbol left,
        ITypeSymbol right,
        out ITypeSymbol promotedLeft,
        out ITypeSymbol promotedRight)
    {
        promotedLeft = left;
        promotedRight = right;

        if (!IsNumericPromotionOperator(kind))
            return false;

        left = left.UnwrapLiteralType() ?? left;
        right = right.UnwrapLiteralType() ?? right;

        if (!IsSupportedPredefinedNumericType(left.SpecialType) || !IsSupportedPredefinedNumericType(right.SpecialType))
            return false;

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var longType = compilation.GetSpecialType(SpecialType.System_Int64);
        var uintType = compilation.GetSpecialType(SpecialType.System_UInt32);
        var ulongType = compilation.GetSpecialType(SpecialType.System_UInt64);
        var floatType = compilation.GetSpecialType(SpecialType.System_Single);
        var doubleType = compilation.GetSpecialType(SpecialType.System_Double);
        var decimalType = compilation.GetSpecialType(SpecialType.System_Decimal);

        var leftSpecialType = left.SpecialType;
        var rightSpecialType = right.SpecialType;

        if (leftSpecialType == SpecialType.System_Decimal || rightSpecialType == SpecialType.System_Decimal)
        {
            if (leftSpecialType is SpecialType.System_Single or SpecialType.System_Double ||
                rightSpecialType is SpecialType.System_Single or SpecialType.System_Double)
            {
                return false;
            }

            promotedLeft = decimalType;
            promotedRight = decimalType;
            return true;
        }

        if (leftSpecialType == SpecialType.System_Double || rightSpecialType == SpecialType.System_Double)
        {
            promotedLeft = doubleType;
            promotedRight = doubleType;
            return true;
        }

        if (leftSpecialType == SpecialType.System_Single || rightSpecialType == SpecialType.System_Single)
        {
            promotedLeft = floatType;
            promotedRight = floatType;
            return true;
        }

        if (leftSpecialType == SpecialType.System_UInt64 || rightSpecialType == SpecialType.System_UInt64)
        {
            if (IsSignedIntegralType(leftSpecialType) || IsSignedIntegralType(rightSpecialType))
                return false;

            promotedLeft = ulongType;
            promotedRight = ulongType;
            return true;
        }

        if (leftSpecialType == SpecialType.System_Int64 || rightSpecialType == SpecialType.System_Int64)
        {
            promotedLeft = longType;
            promotedRight = longType;
            return true;
        }

        if (leftSpecialType == SpecialType.System_UInt32 || rightSpecialType == SpecialType.System_UInt32)
        {
            if (leftSpecialType is SpecialType.System_Int32 or SpecialType.System_Int16 or SpecialType.System_SByte ||
                rightSpecialType is SpecialType.System_Int32 or SpecialType.System_Int16 or SpecialType.System_SByte)
            {
                promotedLeft = longType;
                promotedRight = longType;
                return true;
            }

            promotedLeft = uintType;
            promotedRight = uintType;
            return true;
        }

        promotedLeft = intType;
        promotedRight = intType;
        return true;
    }

    private static bool IsNumericPromotionOperator(SyntaxKind kind)
    {
        return kind switch
        {
            SyntaxKind.PlusToken => true,
            SyntaxKind.MinusToken => true,
            SyntaxKind.StarToken => true,
            SyntaxKind.SlashToken => true,
            SyntaxKind.PercentToken => true,

            SyntaxKind.EqualsEqualsToken => true,
            SyntaxKind.NotEqualsToken => true,

            SyntaxKind.GreaterThanToken => true,
            SyntaxKind.LessThanToken => true,
            SyntaxKind.GreaterThanOrEqualsToken => true,
            SyntaxKind.LessThanOrEqualsToken => true,

            _ => false,
        };
    }

    private static bool TryPromoteShiftLeftOperand(Compilation compilation, ITypeSymbol type, out ITypeSymbol promotedType)
    {
        type = type.UnwrapLiteralType() ?? type;
        switch (type.SpecialType)
        {
            case SpecialType.System_SByte:
            case SpecialType.System_Byte:
            case SpecialType.System_Int16:
            case SpecialType.System_UInt16:
            case SpecialType.System_Int32:
            case SpecialType.System_Char:
                promotedType = compilation.GetSpecialType(SpecialType.System_Int32);
                return true;

            case SpecialType.System_UInt32:
            case SpecialType.System_Int64:
            case SpecialType.System_UInt64:
                promotedType = type;
                return true;

            default:
                promotedType = default!;
                return false;
        }
    }

    private static bool TryPromoteShiftRightOperand(Compilation compilation, ITypeSymbol type, out ITypeSymbol promotedType)
    {
        type = type.UnwrapLiteralType() ?? type;
        switch (type.SpecialType)
        {
            case SpecialType.System_SByte:
            case SpecialType.System_Byte:
            case SpecialType.System_Int16:
            case SpecialType.System_UInt16:
            case SpecialType.System_Int32:
            case SpecialType.System_Char:
                promotedType = compilation.GetSpecialType(SpecialType.System_Int32);
                return true;

            default:
                promotedType = default!;
                return false;
        }
    }

    private static bool IsSupportedPredefinedNumericType(SpecialType specialType)
    {
        return specialType switch
        {
            SpecialType.System_SByte => true,
            SpecialType.System_Byte => true,
            SpecialType.System_Int16 => true,
            SpecialType.System_UInt16 => true,
            SpecialType.System_Int32 => true,
            SpecialType.System_UInt32 => true,
            SpecialType.System_Int64 => true,
            SpecialType.System_UInt64 => true,
            SpecialType.System_Char => true,
            SpecialType.System_Single => true,
            SpecialType.System_Double => true,
            SpecialType.System_Decimal => true,
            _ => false,
        };
    }

    private static bool IsSignedIntegralType(SpecialType specialType)
    {
        return specialType switch
        {
            SpecialType.System_SByte => true,
            SpecialType.System_Int16 => true,
            SpecialType.System_Int32 => true,
            SpecialType.System_Int64 => true,
            _ => false,
        };
    }

    private static bool IsArithmeticNumericType(SpecialType specialType)
    {
        return IsBitwiseIntegralType(specialType) ||
               specialType is SpecialType.System_Single or SpecialType.System_Double or SpecialType.System_Decimal;
    }

    private static bool IsOrderedNumericType(SpecialType specialType)
    {
        return IsArithmeticNumericType(specialType);
    }

    private static bool IsEqualityComparablePredefinedType(SpecialType specialType)
    {
        return specialType == SpecialType.System_Boolean ||
               IsArithmeticNumericType(specialType);
    }

    private static bool IsBitwiseIntegralType(SpecialType specialType)
    {
        return specialType switch
        {
            SpecialType.System_Int32 => true,
            SpecialType.System_UInt32 => true,
            SpecialType.System_Int64 => true,
            SpecialType.System_UInt64 => true,
            _ => false,
        };
    }

    private static bool MatchesSyntaxKind(SyntaxKind kind, BinaryOperatorKind operatorKind)
    {
        return kind switch
        {
            SyntaxKind.PlusToken => operatorKind == BinaryOperatorKind.Addition,
            SyntaxKind.MinusToken => operatorKind == BinaryOperatorKind.Subtraction,
            SyntaxKind.StarToken => operatorKind == BinaryOperatorKind.Multiplication,
            SyntaxKind.SlashToken => operatorKind == BinaryOperatorKind.Division,
            SyntaxKind.PercentToken => operatorKind == BinaryOperatorKind.Modulo,
            SyntaxKind.EqualsEqualsToken => operatorKind == BinaryOperatorKind.Equality,
            SyntaxKind.NotEqualsToken => operatorKind == BinaryOperatorKind.Inequality,
            SyntaxKind.GreaterThanToken => operatorKind == BinaryOperatorKind.GreaterThan,
            SyntaxKind.LessThanToken => operatorKind == BinaryOperatorKind.LessThan,
            SyntaxKind.GreaterThanOrEqualsToken => operatorKind == BinaryOperatorKind.GreaterThanOrEqual,
            SyntaxKind.LessThanOrEqualsToken => operatorKind == BinaryOperatorKind.LessThanOrEqual,
            SyntaxKind.AmpersandToken => operatorKind == BinaryOperatorKind.BitwiseAnd,
            SyntaxKind.BarToken => operatorKind == BinaryOperatorKind.BitwiseOr,
            SyntaxKind.CaretToken => operatorKind == BinaryOperatorKind.BitwiseXor,
            SyntaxKind.AmpersandAmpersandToken => operatorKind == BinaryOperatorKind.LogicalAnd,
            SyntaxKind.BarBarToken => operatorKind == BinaryOperatorKind.LogicalOr,
            SyntaxKind.LessThanLessThanToken => operatorKind == BinaryOperatorKind.ShiftLeft,
            SyntaxKind.GreaterThanGreaterThanToken => operatorKind == BinaryOperatorKind.ShiftRight,
            _ => false,
        };
    }

    private static bool TypesMatchForBuiltInOperator(ITypeSymbol expected, ITypeSymbol actual)
    {
        if (SymbolEqualityComparer.Default.Equals(expected, actual))
            return true;

        return expected.SpecialType != SpecialType.None &&
            actual.SpecialType == expected.SpecialType;
    }

    private static bool AreEquivalentEnumTypes(ITypeSymbol left, ITypeSymbol right)
    {
        if (SymbolEqualityComparer.Default.Equals(left, right) || left.MetadataIdentityEquals(right))
            return true;

        if (left is not INamedTypeSymbol leftNamed || right is not INamedTypeSymbol rightNamed)
            return false;

        var leftEnumLike = leftNamed.TypeKind == TypeKind.Enum || leftNamed.EnumUnderlyingType is not null;
        var rightEnumLike = rightNamed.TypeKind == TypeKind.Enum || rightNamed.EnumUnderlyingType is not null;

        return leftEnumLike &&
            rightEnumLike &&
            string.Equals(leftNamed.MetadataName, rightNamed.MetadataName, StringComparison.Ordinal) &&
            string.Equals(leftNamed.ContainingNamespace?.ToDisplayString(), rightNamed.ContainingNamespace?.ToDisplayString(), StringComparison.Ordinal) &&
            leftNamed.EnumUnderlyingType?.SpecialType == rightNamed.EnumUnderlyingType?.SpecialType;
    }

    internal static bool TryCreateEnumLikeOperator(
        Compilation compilation,
        SyntaxKind kind,
        ITypeSymbol left,
        ITypeSymbol right,
        out BoundBinaryOperator op)
    {
        op = null!;

        if (!AreEquivalentEnumTypes(left, right))
            return false;

        var boolType = compilation.GetSpecialType(SpecialType.System_Boolean);
        switch (kind)
        {
            case SyntaxKind.EqualsEqualsToken:
                op = new BoundBinaryOperator(BinaryOperatorKind.Equality, left, right, boolType);
                return true;
            case SyntaxKind.NotEqualsToken:
                op = new BoundBinaryOperator(BinaryOperatorKind.Inequality, left, right, boolType);
                return true;
            case SyntaxKind.AmpersandToken:
                op = new BoundBinaryOperator(BinaryOperatorKind.BitwiseAnd, left, right, left);
                return true;
            case SyntaxKind.BarToken:
                op = new BoundBinaryOperator(BinaryOperatorKind.BitwiseOr, left, right, left);
                return true;
            case SyntaxKind.CaretToken:
                op = new BoundBinaryOperator(BinaryOperatorKind.BitwiseXor, left, right, left);
                return true;
            default:
                return false;
        }
    }
}

[Flags]
internal enum BinaryOperatorKind
{
    None = 0,
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Equality,
    Inequality,
    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
    Modulo,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LogicalAnd,
    LogicalOr,
    StringConcatenation,

    ShiftLeft,
    ShiftRight,


    Lifted = 1 << 8,
    Checked = 1 << 9,
}
