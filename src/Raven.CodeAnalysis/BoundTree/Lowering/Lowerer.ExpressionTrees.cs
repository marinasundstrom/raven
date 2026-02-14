using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer
{
    private bool TryLowerExpressionTreeConversion(
        BoundConversionExpression conversion,
        BoundLambdaExpression lambda,
        out BoundExpression lowered)
    {
        lowered = conversion;
        var delegateType = default(INamedTypeSymbol);
        if (!TryGetExpressionTreeDelegateType(conversion.Type, out delegateType))
        {
            delegateType = lambda.DelegateType as INamedTypeSymbol;
            if (delegateType is null || delegateType.GetDelegateInvokeMethod() is null)
                return false;
        }

        var context = new ExpressionTreeLoweringContext();
        var compilation = GetCompilation();
        var expressionType = compilation.GetTypeByMetadataName("System.Linq.Expressions.Expression");
        var parameterExpressionType = compilation.GetTypeByMetadataName("System.Linq.Expressions.ParameterExpression");
        var systemType = compilation.GetSpecialType(SpecialType.System_Type);
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var objectType = compilation.GetSpecialType(SpecialType.System_Object);

        if (expressionType is null || parameterExpressionType is null || systemType.TypeKind == TypeKind.Error)
            return false;

        var parameterLocals = new Dictionary<IParameterSymbol, ILocalSymbol>(SymbolEqualityComparer.Default);
        var statements = new List<BoundStatement>();

        foreach (var parameter in lambda.Parameters)
        {
            var parameterLocal = CreateTempLocal($"exprParam_{parameter.Name}", parameterExpressionType, isMutable: false);
            parameterLocals[parameter] = parameterLocal;

            var parameterNameLiteral = new BoundLiteralExpression(
                BoundLiteralExpressionKind.StringLiteral,
                parameter.Name,
                stringType);
            var parameterTypeOf = new BoundTypeOfExpression(parameter.Type, systemType);

            if (!TryCreateExpressionFactoryCall(
                    "Parameter",
                    [parameterTypeOf, parameterNameLiteral],
                    parameterExpressionType,
                    out var parameterExprCall))
            {
                return false;
            }

            statements.Add(new BoundLocalDeclarationStatement(
            [
                new BoundVariableDeclarator(parameterLocal, parameterExprCall)
            ]));
        }

        if (!TryLowerLambdaBody(lambda.Body, parameterLocals, context, out var bodyExpression))
            return false;

        var parameterArrayType = compilation.CreateArrayTypeSymbol(parameterExpressionType, 1);
        var parameterArray = new BoundCollectionExpression(
            parameterArrayType,
            parameterLocals.Values.Select(local => (BoundExpression)new BoundLocalAccess(local)).ToArray());

        if (!TryCreateExpressionLambdaCall(delegateType, bodyExpression, parameterArray, conversion.Type, out var lambdaCall))
            return false;

        context.MarkLowered(lambda, lambdaCall);
        statements.Add(new BoundExpressionStatement(lambdaCall));

        lowered = new BoundBlockExpression(statements, compilation.UnitTypeSymbol);
        return true;

        bool TryCreateExpressionFactoryCall(
            string methodName,
            IReadOnlyList<BoundExpression> arguments,
            ITypeSymbol expectedReturnType,
            out BoundInvocationExpression invocation)
        {
            invocation = null!;

            var selected = expressionType.GetMembers(methodName)
                .OfType<IMethodSymbol>()
                .FirstOrDefault(candidate => !candidate.IsGenericMethod &&
                    candidate.Parameters.Length == arguments.Count &&
                    ParametersCompatible(candidate.Parameters, arguments.Select(arg => arg.Type).ToArray()));
            if (selected is null)
                return false;

            var convertedArgs = ConvertArgumentsForMethod(selected, arguments);
            invocation = new BoundInvocationExpression(selected, convertedArgs);
            return SymbolEqualityComparer.Default.Equals(expectedReturnType, selected.ReturnType) ||
                   compilation.ClassifyConversion(selected.ReturnType, expectedReturnType).Exists;
        }

        bool TryCreateExpressionLambdaCall(
            INamedTypeSymbol targetDelegateType,
            BoundExpression body,
            BoundExpression parametersArray,
            ITypeSymbol targetExpressionType,
            out BoundExpression result)
        {
            result = null!;

            var lambdaMethodDefinition = expressionType.GetMembers("Lambda")
                .OfType<IMethodSymbol>()
                .FirstOrDefault(candidate =>
                    candidate.IsGenericMethod &&
                    candidate.TypeParameters.Length == 1 &&
                    candidate.Parameters.Length == 2 &&
                    ParametersCompatible(candidate.Parameters, [body.Type, parametersArray.Type]));
            if (lambdaMethodDefinition is null)
                return false;

            var constructed = lambdaMethodDefinition.Construct(targetDelegateType);
            var convertedArgs = ConvertArgumentsForMethod(constructed, [body, parametersArray]);
            result = new BoundInvocationExpression(constructed, convertedArgs);

            if (SymbolEqualityComparer.Default.Equals(result.Type, targetExpressionType))
                return true;

            var targetConversion = compilation.ClassifyConversion(result.Type, targetExpressionType);
            if (!targetConversion.Exists)
                return false;

            if (!targetConversion.IsIdentity)
                result = new BoundConversionExpression(result, targetExpressionType, targetConversion);

            return true;
        }

        BoundExpression[] ConvertArgumentsForMethod(IMethodSymbol method, IReadOnlyList<BoundExpression> args)
        {
            var converted = new BoundExpression[args.Count];

            for (var i = 0; i < args.Count; i++)
            {
                var source = args[i];
                var target = method.Parameters[i].Type;
                var sourceType = source.Type ?? compilation.ErrorTypeSymbol;
                var conversionInfo = compilation.ClassifyConversion(sourceType, target);

                converted[i] = conversionInfo.Exists && !conversionInfo.IsIdentity
                    ? new BoundConversionExpression(source, target, conversionInfo)
                    : source;
            }

            return converted;
        }

        bool ParametersCompatible(ImmutableArray<IParameterSymbol> parameters, IReadOnlyList<ITypeSymbol?> argumentTypes)
        {
            if (parameters.Length != argumentTypes.Count)
                return false;

            for (var i = 0; i < parameters.Length; i++)
            {
                var argumentType = argumentTypes[i] ?? compilation.ErrorTypeSymbol;
                var parameterType = parameters[i].Type;
                var conversionInfo = compilation.ClassifyConversion(argumentType, parameterType);
                if (!conversionInfo.Exists)
                    return false;
            }

            return true;
        }
    }

    private bool TryLowerLambdaBody(
        BoundExpression expression,
        Dictionary<IParameterSymbol, ILocalSymbol> parameterLocals,
        ExpressionTreeLoweringContext context,
        out BoundExpression lowered)
    {
        var compilation = GetCompilation();
        var expressionType = compilation.GetTypeByMetadataName("System.Linq.Expressions.Expression");
        var systemType = compilation.GetSpecialType(SpecialType.System_Type);
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var objectType = compilation.GetSpecialType(SpecialType.System_Object);

        lowered = expression;

        if (expressionType is null || systemType.TypeKind == TypeKind.Error)
            return false;

        switch (expression)
        {
            case BoundParameterAccess parameterAccess:
                if (!parameterLocals.TryGetValue(parameterAccess.Parameter, out var parameterLocal))
                {
                    context.MarkUnsupported(expression, "parameter is not part of the expression-tree lambda signature");
                    return false;
                }

                lowered = new BoundLocalAccess(parameterLocal);
                context.MarkLowered(expression, lowered);
                return true;

            case BoundLocalAccess localAccess:
                {
                    var value = (BoundExpression)localAccess;
                    var boxedValue = localAccess.Type.IsValueType
                        ? new BoundConversionExpression(value, objectType, compilation.ClassifyConversion(localAccess.Type, objectType))
                        : value;

                    var valueTypeOf = new BoundTypeOfExpression(localAccess.Type, systemType);

                    if (!TryCreateExpressionFactoryCall("Constant", [boxedValue, valueTypeOf], expressionType, out var constant))
                        return false;

                    lowered = constant;
                    context.MarkLowered(expression, lowered);
                    return true;
                }

            case BoundLiteralExpression literal:
                {
                    BoundExpression valueExpr = literal;
                    if (literal.Type.IsValueType)
                    {
                        var boxing = compilation.ClassifyConversion(literal.Type, objectType);
                        if (boxing.Exists && !boxing.IsIdentity)
                            valueExpr = new BoundConversionExpression(literal, objectType, boxing);
                    }

                    var valueTypeOf = new BoundTypeOfExpression(literal.Type, systemType);
                    if (!TryCreateExpressionFactoryCall("Constant", [valueExpr, valueTypeOf], expressionType, out var constant))
                        return false;

                    lowered = constant;
                    context.MarkLowered(expression, lowered);
                    return true;
                }

            case BoundConversionExpression conversionExpression:
                {
                    if (!TryLowerLambdaBody(conversionExpression.Expression, parameterLocals, context, out var operand))
                        return false;

                    var targetTypeOf = new BoundTypeOfExpression(conversionExpression.Type, systemType);
                    if (!TryCreateExpressionFactoryCall("Convert", [operand, targetTypeOf], expressionType, out var convert))
                        return false;

                    lowered = convert;
                    context.MarkLowered(expression, lowered);
                    return true;
                }

            case BoundMemberAccessExpression memberAccess:
                {
                    if (memberAccess.Member is not IFieldSymbol and not IPropertySymbol)
                    {
                        context.MarkUnsupported(expression, $"member access for '{memberAccess.Member.Kind}' is not supported");
                        return false;
                    }

                    if (memberAccess.Receiver is null)
                    {
                        context.MarkUnsupported(expression, "static member access is not supported yet");
                        return false;
                    }

                    if (!TryLowerLambdaBody(memberAccess.Receiver, parameterLocals, context, out var receiverExpression))
                        return false;

                    var memberName = new BoundLiteralExpression(
                        BoundLiteralExpressionKind.StringLiteral,
                        memberAccess.Member.Name,
                        stringType);

                    if (!TryCreateExpressionFactoryCall("PropertyOrField", [receiverExpression, memberName], expressionType, out var propertyOrField))
                        return false;

                    lowered = propertyOrField;
                    context.MarkLowered(expression, lowered);
                    return true;
                }

            case BoundBinaryExpression binary:
                {
                    if (!TryLowerLambdaBody(binary.Left, parameterLocals, context, out var left))
                        return false;
                    if (!TryLowerLambdaBody(binary.Right, parameterLocals, context, out var right))
                        return false;

                    var methodName = binary.Operator.OperatorKind switch
                    {
                        BinaryOperatorKind.Addition => "Add",
                        BinaryOperatorKind.Subtraction => "Subtract",
                        BinaryOperatorKind.Multiplication => "Multiply",
                        BinaryOperatorKind.Division => "Divide",
                        BinaryOperatorKind.Equality => "Equal",
                        BinaryOperatorKind.Inequality => "NotEqual",
                        BinaryOperatorKind.GreaterThan => "GreaterThan",
                        BinaryOperatorKind.GreaterThanOrEqual => "GreaterThanOrEqual",
                        BinaryOperatorKind.LessThan => "LessThan",
                        BinaryOperatorKind.LessThanOrEqual => "LessThanOrEqual",
                        BinaryOperatorKind.LogicalAnd => "AndAlso",
                        BinaryOperatorKind.LogicalOr => "OrElse",
                        _ => null
                    };

                    if (methodName is null)
                    {
                        context.MarkUnsupported(expression, $"binary operator '{binary.Operator.OperatorKind}' is not supported");
                        return false;
                    }

                    if (!TryCreateExpressionFactoryCall(methodName, [left, right], expressionType, out var binaryCall))
                        return false;

                    lowered = binaryCall;
                    context.MarkLowered(expression, lowered);
                    return true;
                }

            case BoundUnaryExpression unary:
                {
                    if (!TryLowerLambdaBody(unary.Operand, parameterLocals, context, out var operand))
                        return false;

                    var methodName = unary.Operator.OperatorKind switch
                    {
                        BoundUnaryOperatorKind.LogicalNot => "Not",
                        BoundUnaryOperatorKind.UnaryMinus => "Negate",
                        _ => null
                    };
                    if (methodName is null)
                    {
                        context.MarkUnsupported(expression, $"unary operator '{unary.Operator.OperatorKind}' is not supported");
                        return false;
                    }

                    if (!TryCreateExpressionFactoryCall(methodName, [operand], expressionType, out var unaryCall))
                        return false;

                    lowered = unaryCall;
                    context.MarkLowered(expression, lowered);
                    return true;
                }
        }

        context.MarkUnsupported(expression, $"bound node '{expression.GetType().Name}' is not supported");
        return false;

        bool TryCreateExpressionFactoryCall(
            string methodName,
            IReadOnlyList<BoundExpression> arguments,
            ITypeSymbol expectedReturnType,
            out BoundInvocationExpression invocation)
        {
            invocation = null!;

            var method = expressionType.GetMembers(methodName)
                .OfType<IMethodSymbol>()
                .FirstOrDefault(candidate =>
                    !candidate.IsGenericMethod &&
                    candidate.Parameters.Length == arguments.Count &&
                    ParametersCompatible(candidate.Parameters, arguments.Select(arg => arg.Type).ToArray()));
            if (method is null)
                return false;

            var convertedArgs = new BoundExpression[arguments.Count];
            for (var i = 0; i < arguments.Count; i++)
            {
                var source = arguments[i];
                var targetType = method.Parameters[i].Type;
                var sourceType = source.Type ?? compilation.ErrorTypeSymbol;
                var conversionInfo = compilation.ClassifyConversion(sourceType, targetType);

                convertedArgs[i] = conversionInfo.Exists && !conversionInfo.IsIdentity
                    ? new BoundConversionExpression(source, targetType, conversionInfo)
                    : source;
            }

            invocation = new BoundInvocationExpression(method, convertedArgs);
            return SymbolEqualityComparer.Default.Equals(expectedReturnType, method.ReturnType) ||
                   compilation.ClassifyConversion(method.ReturnType, expectedReturnType).Exists;
        }

        bool ParametersCompatible(ImmutableArray<IParameterSymbol> parameters, IReadOnlyList<ITypeSymbol?> argumentTypes)
        {
            if (parameters.Length != argumentTypes.Count)
                return false;

            for (var i = 0; i < parameters.Length; i++)
            {
                var argumentType = argumentTypes[i] ?? compilation.ErrorTypeSymbol;
                var conversionInfo = compilation.ClassifyConversion(argumentType, parameters[i].Type);
                if (!conversionInfo.Exists)
                    return false;
            }

            return true;
        }
    }

    private static bool TryGetExpressionTreeDelegateType(ITypeSymbol type, out INamedTypeSymbol delegateType)
    {
        delegateType = null!;

        static ITypeSymbol Unalias(ITypeSymbol symbol)
        {
            while (symbol.IsAlias && symbol.UnderlyingSymbol is ITypeSymbol underlying)
                symbol = underlying;

            return symbol;
        }

        type = Unalias(type);

        if (type is NullableTypeSymbol nullable)
            type = nullable.UnderlyingType;

        if (type is not INamedTypeSymbol named)
            return false;

        var definition = (named.OriginalDefinition as INamedTypeSymbol) ?? named;
        if (definition.Arity != 1)
            return false;

        var isExpressionType =
            string.Equals(definition.Name, "Expression", StringComparison.Ordinal) ||
            string.Equals(definition.MetadataName, "Expression`1", StringComparison.Ordinal);
        if (!isExpressionType)
            return false;

        if (named.TypeArguments.Length != 1)
            return false;

        var candidate = Unalias(named.TypeArguments[0]);
        if (candidate is not INamedTypeSymbol candidateDelegate)
            return false;

        if (candidateDelegate.TypeKind != TypeKind.Delegate &&
            candidateDelegate.GetDelegateInvokeMethod() is null)
            return false;

        delegateType = candidateDelegate;
        return true;
    }
}

internal sealed class ExpressionTreeLoweringContext
{
    private readonly Dictionary<BoundNode, BoundExpression> _loweredNodes = new(ReferenceEqualityComparer.Instance);
    private readonly List<(BoundNode Node, string Reason)> _unsupportedNodes = [];

    public void MarkLowered(BoundNode original, BoundExpression lowered)
    {
        _loweredNodes[original] = lowered;
    }

    public void MarkUnsupported(BoundNode node, string reason)
    {
        _unsupportedNodes.Add((node, reason));
    }

    public IReadOnlyDictionary<BoundNode, BoundExpression> LoweredNodes => _loweredNodes;
    public IReadOnlyList<(BoundNode Node, string Reason)> UnsupportedNodes => _unsupportedNodes;
}
