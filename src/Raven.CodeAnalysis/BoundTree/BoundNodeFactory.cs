#nullable enable
using System;
using System.Collections.Generic;
using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

internal sealed partial class BoundNodeFactory
{
    public BoundNodeFactory(Compilation compilation)
    {
        Compilation = compilation ?? throw new ArgumentNullException(nameof(compilation));
    }

    public Compilation Compilation { get; }

    public BoundAddressOfExpression CreateAddressOfExpression(
        ISymbol? symbol,
        ITypeSymbol valueType,
        BoundExpression? receiver = null,
        BoundExpression? storage = null)
        => new(symbol, valueType, receiver, storage);

    public BoundAndPattern CreateAndPattern(BoundPattern left, BoundPattern right)
        => new(left, right);

    public BoundArrayAccessExpression CreateArrayAccessExpression(
        BoundExpression receiver,
        IEnumerable<BoundExpression> indices,
        ITypeSymbol elementType)
        => new(receiver, indices, elementType);

    public BoundArrayAssignmentExpression CreateArrayAssignmentExpression(
        BoundArrayAccessExpression left,
        BoundExpression right)
        => new(left, right, Compilation.UnitTypeSymbol);

    public BoundAsExpression CreateAsExpression(BoundExpression expression, ITypeSymbol type, Conversion conversion)
        => new(expression, type, conversion);

    public BoundAssignmentStatement CreateAssignmentStatement(BoundAssignmentExpression expression)
        => new(expression);

    public BoundAwaitExpression CreateAwaitExpression(
        BoundExpression expression,
        ITypeSymbol resultType,
        ITypeSymbol awaiterType,
        IMethodSymbol getAwaiterMethod,
        IMethodSymbol getResultMethod,
        IPropertySymbol isCompletedProperty)
        => new(expression, resultType, awaiterType, getAwaiterMethod, getResultMethod, isCompletedProperty);

    public BoundBinaryExpression CreateBinaryExpression(BoundExpression left, BoundBinaryOperator @operator, BoundExpression right)
        => new(left, @operator, right);

    public BoundBlockExpression CreateBlockExpression(
        IEnumerable<BoundStatement> statements,
        ImmutableArray<ILocalSymbol> localsToDispose = default)
        => new(statements, Compilation.UnitTypeSymbol, localsToDispose);

    public BoundBlockStatement CreateBlockStatement(
        IEnumerable<BoundStatement> statements,
        ImmutableArray<ILocalSymbol> localsToDispose = default)
        => new(statements, localsToDispose);

    public BoundBreakStatement CreateBreakStatement()
        => new();

    public BoundByRefAssignmentExpression CreateByRefAssignmentExpression(
        BoundExpression reference,
        ITypeSymbol elementType,
        BoundExpression right)
        => new(reference, elementType, right, Compilation.UnitTypeSymbol);

    public BoundConversionExpression CreateConversionExpression(BoundExpression expression, ITypeSymbol type, Conversion conversion)
        => new(expression, type, conversion);

    public BoundCatchClause CreateCatchClause(ITypeSymbol exceptionType, ILocalSymbol? local, BoundBlockStatement block)
        => new(exceptionType, local, block);

    public BoundCollectionExpression CreateCollectionExpression(
        ITypeSymbol type,
        IEnumerable<BoundExpression> elements,
        ISymbol? collectionSymbol = null,
        BoundExpressionReason reason = BoundExpressionReason.None)
        => new(type, elements, collectionSymbol, reason);

    public BoundConditionalAccessExpression CreateConditionalAccessExpression(
        BoundExpression receiver,
        BoundExpression whenNotNull,
        ITypeSymbol type)
        => new(receiver, whenNotNull, type);

    public BoundConditionalGotoStatement CreateConditionalGotoStatement(
        ILabelSymbol target,
        BoundExpression condition,
        bool jumpIfTrue)
        => new(target, condition, jumpIfTrue);

    public BoundContinueStatement CreateContinueStatement()
        => new();

    public BoundDeclarationPattern CreateDeclarationPattern(
        ITypeSymbol declaredType,
        BoundDesignator designator,
        BoundExpressionReason reason = BoundExpressionReason.None)
        => new(declaredType, designator, reason);

    public BoundDelegateCreationExpression CreateDelegateCreationExpression(
        BoundMethodGroupExpression methodGroup,
        ITypeSymbol delegateType)
        => new(methodGroup, delegateType);

    public BoundEmptyCollectionExpression CreateEmptyCollectionExpression(
        ITypeSymbol? type = null,
        BoundExpressionReason reason = BoundExpressionReason.None)
        => new(type, reason);

    public BoundErrorExpression CreateErrorExpression(
        ITypeSymbol type,
        ISymbol? symbol = null,
        BoundExpressionReason reason = BoundExpressionReason.None,
        ImmutableArray<ISymbol> candidates = default)
        => new(type, symbol, reason, candidates);

    public BoundExpressionStatement CreateExpressionStatement(BoundExpression expression)
        => new(expression);

    public BoundFieldAccess CreateFieldAccess(
        IFieldSymbol field,
        BoundExpressionReason reason = BoundExpressionReason.None)
        => new(field, reason);

    public BoundFieldAssignmentExpression CreateFieldAssignmentExpression(
        BoundExpression? receiver,
        IFieldSymbol field,
        BoundExpression right,
        bool requiresReceiverAddress = false)
        => new(receiver, field, right, Compilation.UnitTypeSymbol, requiresReceiverAddress);

    public BoundForStatement CreateForStatement(
        ILocalSymbol? local,
        ForIterationInfo iteration,
        BoundExpression collection,
        BoundStatement body)
        => new(local, iteration, collection, body);

    public BoundFunctionStatement CreateFunctionStatement(IMethodSymbol method)
        => new(method);

    public BoundGotoStatement CreateGotoStatement(ILabelSymbol target, bool isBackward = false)
        => new(target, isBackward);

    public BoundIfExpression CreateIfExpression(
        BoundExpression condition,
        BoundExpression thenBranch,
        BoundExpression? elseBranch = null)
        => new(condition, thenBranch, elseBranch);

    public BoundIfStatement CreateIfStatement(
        BoundExpression condition,
        BoundStatement thenNode,
        BoundStatement? elseNode = null)
        => new(condition, thenNode, elseNode);

    public BoundIndexerAccessExpression CreateIndexerAccessExpression(
        BoundExpression receiver,
        IEnumerable<BoundExpression> arguments,
        IPropertySymbol indexer)
        => new(receiver, arguments, indexer);

    public BoundIndexerAssignmentExpression CreateIndexerAssignmentExpression(
        BoundIndexerAccessExpression left,
        BoundExpression right)
        => new(left, right, Compilation.UnitTypeSymbol);

    public BoundInvocationExpression CreateInvocationExpression(
        IMethodSymbol method,
        IEnumerable<BoundExpression> arguments,
        BoundExpression? receiver = null,
        BoundExpression? extensionReceiver = null,
        bool requiresReceiverAddress = false)
        => new(method, arguments, receiver, extensionReceiver, requiresReceiverAddress);

    public BoundIsPatternExpression CreateIsPatternExpression(
        BoundExpression expression,
        BoundPattern pattern,
        ITypeSymbol booleanType,
        BoundExpressionReason reason = BoundExpressionReason.None)
        => new(expression, pattern, booleanType, reason);

    public BoundLabeledStatement CreateLabeledStatement(ILabelSymbol label, BoundStatement statement)
        => new(label, statement);

    public BoundLambdaExpression CreateLambdaExpression(
        IEnumerable<IParameterSymbol> parameters,
        ITypeSymbol returnType,
        BoundExpression body,
        ISymbol symbol,
        ITypeSymbol delegateType,
        IEnumerable<ISymbol> capturedVariables,
        ImmutableArray<INamedTypeSymbol> candidateDelegates)
        => new(parameters, returnType, body, symbol, delegateType, capturedVariables, candidateDelegates);

    public BoundLiteralExpression CreateLiteralExpression(
        BoundLiteralExpressionKind kind,
        object value,
        ITypeSymbol type,
        ITypeSymbol? convertedType = null)
        => new(kind, value, type, convertedType);

    public BoundLocalAccess CreateLocalAccess(
        ILocalSymbol local,
        BoundExpressionReason reason = BoundExpressionReason.None)
        => new(local, reason);

    public BoundLocalAssignmentExpression CreateLocalAssignmentExpression(ILocalSymbol local, BoundExpression right)
        => new(local, right, Compilation.UnitTypeSymbol);

    public BoundLocalDeclarationStatement CreateLocalDeclarationStatement(
        IEnumerable<BoundVariableDeclarator> declarators,
        bool isUsing = false)
        => new(declarators, isUsing);

    public BoundMatchExpression CreateMatchExpression(
        BoundExpression expression,
        ImmutableArray<BoundMatchArm> arms,
        ITypeSymbol type)
        => new(expression, arms, type);

    public BoundMemberAccessExpression CreateMemberAccessExpression(
        BoundExpression? receiver,
        ISymbol member,
        BoundExpressionReason reason = BoundExpressionReason.None)
        => new(receiver, member, reason);

    public BoundMemberAssignmentExpression CreateMemberAssignmentExpression(
        ISymbol member,
        BoundExpression receiver,
        BoundExpression right)
        => new(member, receiver, right, Compilation.UnitTypeSymbol);

    public BoundMethodGroupExpression CreateMethodGroupExpression(
        BoundExpression? receiver,
        ImmutableArray<IMethodSymbol> methods,
        ITypeSymbol methodGroupType,
        Func<ITypeSymbol?>? delegateTypeFactory = null,
        IMethodSymbol? selectedMethod = null,
        BoundExpressionReason reason = BoundExpressionReason.None)
        => new(receiver, methods, methodGroupType, delegateTypeFactory, selectedMethod, reason);

    public BoundNamespaceExpression CreateNamespaceExpression(INamespaceSymbol @namespace)
        => new(@namespace);

    public BoundNotPattern CreateNotPattern(BoundPattern pattern)
        => new(pattern);

    public BoundObjectCreationExpression CreateObjectCreationExpression(
        IMethodSymbol constructor,
        IEnumerable<BoundExpression> arguments,
        BoundExpression? receiver = null)
        => new(constructor, arguments, receiver);

    public BoundOrPattern CreateOrPattern(BoundPattern left, BoundPattern right)
        => new(left, right);

    public BoundParameterAccess CreateParameterAccess(
        IParameterSymbol parameter,
        BoundExpressionReason reason = BoundExpressionReason.None)
        => new(parameter, reason);

    public BoundParameterAssignmentExpression CreateParameterAssignmentExpression(
        IParameterSymbol parameter,
        BoundExpression right)
        => new(parameter, right, Compilation.UnitTypeSymbol);

    public BoundParenthesizedExpression CreateParenthesizedExpression(BoundExpression expression)
        => new(expression);

    public BoundPatternAssignmentExpression CreatePatternAssignmentExpression(
        ITypeSymbol type,
        BoundPattern pattern,
        BoundExpression right)
        => new(type, pattern, right, Compilation.UnitTypeSymbol);

    public BoundPropertyAccess CreatePropertyAccess(
        IPropertySymbol property,
        BoundExpressionReason reason = BoundExpressionReason.None)
        => new(property, reason);

    public BoundPropertyAssignmentExpression CreatePropertyAssignmentExpression(
        BoundExpression? receiver,
        IPropertySymbol property,
        BoundExpression right)
        => new(receiver, property, right, Compilation.UnitTypeSymbol);

    public BoundReturnStatement CreateReturnStatement(BoundExpression? expression)
        => new(expression);

    public BoundSelfExpression CreateSelfExpression(
        ITypeSymbol type,
        BoundExpressionReason reason = BoundExpressionReason.None)
        => new(type, reason);

    public BoundSingleVariableDesignator CreateSingleVariableDesignator(
        ILocalSymbol local,
        BoundExpressionReason reason = BoundExpressionReason.None)
        => new(local, reason);

    public BoundSpreadElement CreateSpreadElement(BoundExpression expression)
        => new(expression);

    public BoundThrowStatement CreateThrowStatement(BoundExpression expression)
        => new(expression);

    public BoundTryExpression CreateTryExpression(
        BoundExpression expression,
        ITypeSymbol exceptionType,
        ITypeSymbol type,
        IMethodSymbol okConstructor,
        IMethodSymbol errorConstructor)
        => new(expression, exceptionType, type, okConstructor, errorConstructor);

    public BoundTryStatement CreateTryStatement(
        BoundBlockStatement tryBlock,
        ImmutableArray<BoundCatchClause> catchClauses,
        BoundBlockStatement? finallyBlock)
        => new(tryBlock, catchClauses, finallyBlock);

    public BoundTupleExpression CreateTupleExpression(
        IEnumerable<BoundExpression> elements,
        ITypeSymbol type)
        => new(elements, type);

    public BoundTypeExpression CreateTypeExpression(ITypeSymbol typeSymbol)
        => new(typeSymbol);

    public BoundTypeOfExpression CreateTypeOfExpression(ITypeSymbol operandType, ITypeSymbol systemType)
        => new(operandType, systemType);

    public BoundUnaryExpression CreateUnaryExpression(BoundUnaryOperator @operator, BoundExpression operand)
        => new(@operator, operand);

    public BoundUnitExpression CreateUnitExpression(BoundExpressionReason reason = BoundExpressionReason.None)
        => new(Compilation.UnitTypeSymbol, reason);

    public BoundVariableDeclarator CreateVariableDeclarator(ILocalSymbol local, BoundExpression? initializer)
        => new(local, initializer);

    public BoundVariableExpression CreateVariableExpression(ILocalSymbol variable)
        => new(variable);

    public BoundWhileStatement CreateWhileStatement(BoundExpression condition, BoundStatement body)
        => new(condition, body);

    public BoundYieldBreakStatement CreateYieldBreakStatement(ITypeSymbol elementType, IteratorMethodKind iteratorKind)
        => new(elementType, iteratorKind);

    public BoundYieldReturnStatement CreateYieldReturnStatement(
        BoundExpression expression,
        ITypeSymbol elementType,
        IteratorMethodKind iteratorKind)
        => new(expression, elementType, iteratorKind);
}
