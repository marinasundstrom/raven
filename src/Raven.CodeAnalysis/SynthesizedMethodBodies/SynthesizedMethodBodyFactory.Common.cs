using System;
using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal static partial class SynthesizedMethodBodyFactory
{
    private static void AppendFormattedMemberList(
        Compilation compilation,
        List<BoundExpression> parts,
        IReadOnlyList<(string? Name, BoundExpression Value)> members)
    {
        for (var index = 0; index < members.Count; index++)
        {
            if (index > 0)
                parts.Add(CreateStringLiteral(compilation, ", "));

            var (name, value) = members[index];
            if (!string.IsNullOrEmpty(name))
            {
                parts.Add(CreateStringLiteral(compilation, name!));
                parts.Add(CreateStringLiteral(compilation, " = "));
            }

            parts.Add(value);
        }
    }

    private static BoundBlockStatement CreateAutoPropertyAccessorBody(
        Compilation compilation,
        IMethodSymbol method,
        SourcePropertySymbol property)
    {
        if (property.BackingField is not SourceFieldSymbol backingField)
            return CreateImplicitUnitReturnBody();

        var receiver = property.IsStatic ? null : new BoundSelfExpression(method.ContainingType!);
        if (method.MethodKind == MethodKind.PropertyGet)
            return new BoundBlockStatement([new BoundReturnStatement(new BoundFieldAccess(receiver, backingField))]);

        var unitType = compilation.GetSpecialType(SpecialType.System_Unit)
            ?? throw new InvalidOperationException("Failed to resolve System.Unit.");
        var valueParameter = method.Parameters.FirstOrDefault();
        if (valueParameter is null)
            return CreateImplicitUnitReturnBody();

        var assignment = new BoundFieldAssignmentExpression(
            receiver,
            backingField,
            CreateConversion(compilation, new BoundParameterAccess(valueParameter), backingField.Type),
            unitType);
        return new BoundBlockStatement([
            new BoundAssignmentStatement(assignment),
            CreateImplicitUnitReturn()
        ]);
    }

    private static BoundBlockStatement CreateDeconstructBody(
        Compilation compilation,
        IMethodSymbol method,
        IReadOnlyList<BoundExpression> sourceValues)
    {
        var statements = new List<BoundStatement>();
        var factory = new BoundNodeFactory(compilation);
        var parameterCount = Math.Min(sourceValues.Count, method.Parameters.Length);

        for (var index = 0; index < parameterCount; index++)
        {
            var targetParameter = method.Parameters[index];
            var targetType = targetParameter.GetByRefElementType();
            var convertedValue = CreateConversion(compilation, sourceValues[index], targetType);

            statements.Add(new BoundAssignmentStatement(
                factory.CreateByRefAssignmentExpression(
                    new BoundParameterAccess(targetParameter),
                    targetType,
                    convertedValue)));
        }

        return WithImplicitUnitReturn(statements);
    }

    private static BoundExpression CreateSelfPropertyGetterAccess(
        IMethodSymbol method,
        IPropertySymbol property)
    {
        return CreatePropertyGetterAccess(new BoundSelfExpression(method.ContainingType!), property);
    }

    private static BoundBlockStatement CreateImplicitUnitReturnBody()
    {
        return new BoundBlockStatement([CreateImplicitUnitReturn()]);
    }

    private static BoundBlockStatement WithImplicitUnitReturn(List<BoundStatement> statements)
    {
        statements.Add(CreateImplicitUnitReturn());
        return new BoundBlockStatement(statements);
    }

    private static BoundReturnStatement CreateImplicitUnitReturn()
    {
        return new BoundReturnStatement(null);
    }

    private static BoundExpression InvokeUnionFormatValueHelper(Compilation compilation, IMethodSymbol method, BoundExpression value)
    {
        var containingType = GetInvocationContainingType(method);
        var helper = containingType
            .GetMembers(SynthesizedUnionMethodNames.FormatValueHelper)
            .OfType<IMethodSymbol>()
            .First(m => m.IsStatic &&
                        m.Parameters.Length == 2 &&
                        m.Parameters[0].Type.SpecialType == SpecialType.System_Object &&
                        m.Parameters[1].Type.SpecialType == SpecialType.System_Type &&
                        m.ReturnType.SpecialType == SpecialType.System_String);
        var objectType = compilation.GetSpecialType(SpecialType.System_Object)!;

        return new BoundInvocationExpression(
            helper,
            [
                CreateConversion(compilation, value, objectType),
                new BoundTypeOfExpression(value.Type, compilation.GetSpecialType(SpecialType.System_Type)!)
            ]);
    }

    private static BoundExpression CreateObjectToStringInvocation(Compilation compilation, BoundExpression receiver)
    {
        var objectToString = compilation.GetSpecialType(SpecialType.System_Object)!
            .GetMembers(nameof(object.ToString))
            .OfType<IMethodSymbol>()
            .First(m => m.Parameters.Length == 0);

        return new BoundInvocationExpression(objectToString, Array.Empty<BoundExpression>(), receiver);
    }

    private static BoundExpression CreatePropertyGetterAccess(BoundExpression receiver, IPropertySymbol property)
    {
        if (property.GetMethod is null)
            throw new InvalidOperationException($"Property '{property.Name}' does not have a getter.");

        return new BoundInvocationExpression(property.GetMethod, Array.Empty<BoundExpression>(), receiver);
    }

    private static BoundExpression CreateQuotedStringValue(
        Compilation compilation,
        BoundExpression value,
        string quote)
    {
        return ConcatSequence(
            compilation,
            [
                CreateStringLiteral(compilation, quote),
                value,
                CreateStringLiteral(compilation, quote)
            ]);
    }

    private static IMethodSymbol ResolveMethod(INamedTypeSymbol type, string name, IReadOnlyList<ITypeSymbol> parameterTypes)
    {
        var methods = type.GetMembers(name).OfType<IMethodSymbol>().ToArray();
        var match = methods.FirstOrDefault(method =>
            method.Parameters.Length == parameterTypes.Count &&
            method.Parameters.Select(parameter => parameter.Type)
                .Zip(parameterTypes, ParameterTypesMatch)
                .All(static matched => matched));

        if (match is not null)
            return match;

        var expectedParameters = string.Join(", ", parameterTypes.Select(typeSymbol => typeSymbol.ToDisplayString(SymbolDisplayFormat.RavenSymbolKeyFormat)));
        var candidates = string.Join(", ", methods.Select(method => method.ToDisplayString(SymbolDisplayFormat.RavenSymbolKeyFormat)));
        throw new InvalidOperationException(
            $"Failed to resolve method '{type.ToDisplayString(SymbolDisplayFormat.RavenSymbolKeyFormat)}.{name}({expectedParameters})'. Candidates: {candidates}");
    }

    private static bool ParameterTypesMatch(ITypeSymbol left, ITypeSymbol right)
    {
        if (SymbolEqualityComparer.Default.Equals(left, right))
            return true;

        if (left.SpecialType != SpecialType.None &&
            left.SpecialType == right.SpecialType)
        {
            return true;
        }

        if (left is IArrayTypeSymbol leftArray &&
            right is IArrayTypeSymbol rightArray &&
            leftArray.Rank == rightArray.Rank)
        {
            return ParameterTypesMatch(leftArray.ElementType, rightArray.ElementType);
        }

        return string.Equals(
            left.ToDisplayString(SymbolDisplayFormat.RavenSymbolKeyFormat),
            right.ToDisplayString(SymbolDisplayFormat.RavenSymbolKeyFormat),
            StringComparison.Ordinal);
    }

    private static IMethodSymbol ResolvePropertyGetter(INamedTypeSymbol type, string propertyName)
    {
        for (INamedTypeSymbol? current = type; current is not null; current = current.BaseType)
        {
            var getter = current.GetMembers(propertyName)
                .OfType<IPropertySymbol>()
                .Select(property => property.GetMethod)
                .FirstOrDefault(static candidate => candidate is not null);

            if (getter is not null)
                return getter;
        }

        throw new InvalidOperationException($"Failed to resolve property getter '{type.ToDisplayString()}.{propertyName}'.");
    }

    private static SourceLocalSymbol CreateSynthesizedLocal(IMethodSymbol method, ITypeSymbol type, string name)
    {
        return new SourceLocalSymbol(
            name,
            type,
            isMutable: true,
            method,
            method.ContainingType,
            method.ContainingNamespace,
            method.Locations.ToArray(),
            method.DeclaringSyntaxReferences.ToArray());
    }

    private static BoundExpressionStatement CreateExpressionStatement(BoundExpression expression)
        => new(expression);

    private static BoundAssignmentStatement CreateAssignmentStatement(
        Compilation compilation,
        ILocalSymbol local,
        BoundExpression right,
        ITypeSymbol unitType)
    {
        return new BoundAssignmentStatement(
            new BoundLocalAssignmentExpression(local, new BoundLocalAccess(local), right, unitType));
    }

    private static BoundAssignmentStatement CreateAppendAssignment(
        Compilation compilation,
        ILocalSymbol local,
        BoundExpression value,
        ITypeSymbol unitType)
        => CreateAssignmentStatement(
            compilation,
            local,
            CreateStringConcat(compilation, new BoundLocalAccess(local), value),
            unitType);

    private static BoundExpression CreateBinaryExpression(
        Compilation compilation,
        SyntaxKind operatorKind,
        BoundExpression left,
        BoundExpression right)
    {
        if (!BoundBinaryOperator.TryLookup(compilation, operatorKind, left.Type, right.Type, out var op))
            throw new InvalidOperationException($"Failed to bind synthesized binary operator '{operatorKind}'.");

        return new BoundBinaryExpression(left, op, right);
    }

    private static BoundExpression CreateLogicalNotExpression(Compilation compilation, BoundExpression operand)
    {
        if (!BoundUnaryOperator.TryLookup(compilation, SyntaxKind.ExclamationToken, operand.Type, out var op))
            throw new InvalidOperationException("Failed to bind synthesized unary operator '!'.");

        return new BoundUnaryExpression(op, operand);
    }

    private static BoundLiteralExpression CreateIntLiteral(Compilation compilation, int value)
        => new(BoundLiteralExpressionKind.NumericLiteral, value, compilation.GetSpecialType(SpecialType.System_Int32)!);

    private static BoundLiteralExpression CreateBoolLiteral(Compilation compilation, bool value)
        => new(
            value ? BoundLiteralExpressionKind.TrueLiteral : BoundLiteralExpressionKind.FalseLiteral,
            value,
            compilation.GetSpecialType(SpecialType.System_Boolean)!);

    private static BoundLiteralExpression CreateByteLiteral(Compilation compilation, byte value)
        => new(BoundLiteralExpressionKind.NumericLiteral, value, compilation.GetSpecialType(SpecialType.System_Byte)!);

    private static BoundExpression ConcatSequence(Compilation compilation, IEnumerable<BoundExpression> expressions)
    {
        using var enumerator = expressions.GetEnumerator();
        if (!enumerator.MoveNext())
            return CreateStringLiteral(compilation, string.Empty);

        var current = enumerator.Current;
        while (enumerator.MoveNext())
            current = CreateStringConcat(compilation, current, enumerator.Current);

        return current;
    }

    private static BoundExpression CreateStringConcat(Compilation compilation, BoundExpression left, BoundExpression right)
    {
        var stringType = compilation.GetSpecialType(SpecialType.System_String)!;
        var candidates = stringType.GetMembers(nameof(string.Concat)).OfType<IMethodSymbol>().ToArray();
        var resolution = OverloadResolver.ResolveOverload(
            candidates,
            [
                new BoundArgument(left, RefKind.None, null),
                new BoundArgument(right, RefKind.None, null)
            ],
            compilation);

        var concat = resolution.Method
            ?? candidates.First(m => m.Parameters.Length == 2);

        return new BoundInvocationExpression(concat, [left, right]);
    }

    private static BoundLiteralExpression CreateStringLiteral(Compilation compilation, string value)
        => new(BoundLiteralExpressionKind.StringLiteral, value, compilation.GetSpecialType(SpecialType.System_String)!);

    private static BoundLiteralExpression CreateNullLiteral(Compilation compilation)
        => new(BoundLiteralExpressionKind.NullLiteral, null!, compilation.NullTypeSymbol);

    private static BoundExpression CreateConversion(Compilation compilation, BoundExpression expression, ITypeSymbol targetType)
    {
        var conversion = compilation.ClassifyConversion(expression.Type, targetType, includeUserDefined: false);
        if (!conversion.Exists)
            throw new InvalidOperationException($"Failed to bind synthesized conversion from '{expression.Type}' to '{targetType}'.");

        return new BoundConversionExpression(expression, targetType, conversion);
    }
}
