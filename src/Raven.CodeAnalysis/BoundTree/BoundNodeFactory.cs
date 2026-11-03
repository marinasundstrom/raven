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
        Symbol? symbol,
        TypeSymbol valueType,
        BoundExpression? receiver = null,
        BoundExpression? storage = null)
        => new(symbol, valueType, receiver, storage);

    public BoundAndPattern CreateAndPattern(BoundPattern left, BoundPattern right)
        => new(left, right);

    public BoundArrayAccessExpression CreateArrayAccessExpression(
        BoundExpression receiver,
        IEnumerable<BoundExpression> indices,
        TypeSymbol elementType)
        => new(receiver, indices, elementType);

    public BoundArrayAssignmentExpression CreateArrayAssignmentExpression(
        BoundArrayAccessExpression left,
        BoundExpression right)
        => new(left, right, Compilation.UnitTypeSymbol);

    public BoundAsExpression CreateAsExpression(BoundExpression expression, TypeSymbol type, Conversion conversion)
        => new(expression, type, conversion);

    public BoundAssignmentStatement CreateAssignmentStatement(BoundAssignmentExpression expression)
        => new(expression);

    public BoundAwaitExpression CreateAwaitExpression(
        BoundExpression expression,
        TypeSymbol resultType,
        TypeSymbol awaiterType,
        MethodSymbol getAwaiterMethod,
        MethodSymbol getResultMethod,
        PropertySymbol isCompletedProperty)
        => new(expression, resultType, awaiterType, getAwaiterMethod, getResultMethod, isCompletedProperty);

    public BoundBinaryExpression CreateBinaryExpression(BoundExpression left, BoundBinaryOperator @operator, BoundExpression right)
        => new(left, @operator, right);

    public BoundBlockExpression CreateBlockExpression(
        IEnumerable<BoundStatement> statements,
        ImmutableArray<LocalSymbol> localsToDispose = default)
        => new(statements, Compilation.UnitTypeSymbol, localsToDispose);

    public BoundBlockStatement CreateBlockStatement(
        IEnumerable<BoundStatement> statements,
        ImmutableArray<LocalSymbol> localsToDispose = default)
        => new(statements, localsToDispose);

    public BoundBreakStatement CreateBreakStatement()
        => new();

    public BoundByRefAssignmentExpression CreateByRefAssignmentExpression(
        BoundExpression reference,
        TypeSymbol elementType,
        BoundExpression right)
        => new(reference, elementType, right, Compilation.UnitTypeSymbol);

    public BoundConversionExpression CreateConversionExpression(BoundExpression expression, TypeSymbol type, Conversion conversion)
        => new(expression, type, conversion);

    public BoundCatchClause CreateCatchClause(TypeSymbol exceptionType, LocalSymbol? local, BoundBlockStatement block)
        => new(exceptionType, local, pattern: null, guard: null, block);

    public BoundCollectionExpression CreateCollectionExpression(
        TypeSymbol type,
        IEnumerable<BoundExpression> elements,
        Symbol? collectionSymbol = null,
        BoundExpressionReason reason = BoundExpressionReason.None)
        => new(type, elements, collectionSymbol, reason);

    public BoundConditionalAccessExpression CreateConditionalAccessExpression(
        BoundExpression receiver,
        BoundExpression whenNotNull,
        TypeSymbol type)
        => new(receiver, whenNotNull, type);

    public BoundConditionalGotoStatement CreateConditionalGotoStatement(
        LabelSymbol target,
        BoundExpression condition,
        bool jumpIfTrue)
        => new(target, condition, jumpIfTrue);

    public BoundContinueStatement CreateContinueStatement()
        => new();

    public BoundDeclarationPattern CreateDeclarationPattern(
        TypeSymbol declaredType,
        BoundDesignator designator,
        BoundExpressionReason reason = BoundExpressionReason.None)
        => new(declaredType, designator, reason);

    public BoundDelegateCreationExpression CreateDelegateCreationExpression(
        BoundMethodGroupExpression methodGroup,
        TypeSymbol delegateType)
        => new(methodGroup, delegateType);

    public BoundEmptyCollectionExpression CreateEmptyCollectionExpression(
        TypeSymbol? type = null,
        BoundExpressionReason reason = BoundExpressionReason.None)
        => new(type, reason);

    public BoundErrorExpression CreateErrorExpression(
        TypeSymbol type,
        Symbol? symbol = null,
        BoundExpressionReason reason = BoundExpressionReason.None,
        ImmutableArray<Symbol> candidates = default)
        => new(type, symbol, reason, candidates);

    public BoundExpressionStatement CreateExpressionStatement(BoundExpression expression)
        => new(expression);

    public BoundFieldAccess CreateFieldAccess(
        FieldSymbol field,
        BoundExpressionReason reason = BoundExpressionReason.None)
        => new(field, reason);

    public BoundFieldAssignmentExpression CreateFieldAssignmentExpression(
        BoundExpression? receiver,
        FieldSymbol field,
        BoundExpression right,
        bool requiresReceiverAddress = false)
        => new(receiver, field, right, Compilation.UnitTypeSymbol, requiresReceiverAddress);

    public BoundForStatement CreateForStatement(
        LocalSymbol? local,
        ForIterationInfo iteration,
        BoundExpression collection,
        BoundStatement body)
        => new(local, iteration, collection, body);

    public BoundFunctionStatement CreateFunctionStatement(MethodSymbol method)
        => new(method);

    public BoundGotoStatement CreateGotoStatement(LabelSymbol target, bool isBackward = false)
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
        PropertySymbol indexer)
        => new(receiver, arguments, indexer);

    public BoundIndexerAssignmentExpression CreateIndexerAssignmentExpression(
        BoundIndexerAccessExpression left,
        BoundExpression right)
        => new(left, right, Compilation.UnitTypeSymbol);

    public BoundInvocationExpression CreateInvocationExpression(
        MethodSymbol method,
        IEnumerable<BoundExpression> arguments,
        BoundExpression? receiver = null,
        BoundExpression? extensionReceiver = null,
        bool requiresReceiverAddress = false)
        => new(method, arguments, receiver, extensionReceiver, requiresReceiverAddress);

    public BoundIsPatternExpression CreateIsPatternExpression(
        BoundExpression expression,
        BoundPattern pattern,
        TypeSymbol booleanType,
        BoundExpressionReason reason = BoundExpressionReason.None)
        => new(expression, pattern, booleanType, reason);

    public BoundLabeledStatement CreateLabeledStatement(LabelSymbol label, BoundStatement statement)
        => new(label, statement);

    public BoundFunctionExpression CreateFunctionExpression(
        IEnumerable<ParameterSymbol> parameters,
        TypeSymbol returnType,
        BoundExpression body,
        Symbol symbol,
        TypeSymbol delegateType,
        IEnumerable<Symbol> capturedVariables,
        ImmutableArray<NamedTypeSymbol> candidateDelegates)
        => new(parameters, returnType, body, symbol, delegateType, capturedVariables, candidateDelegates);

    public BoundLiteralExpression CreateLiteralExpression(
        BoundLiteralExpressionKind kind,
        object value,
        TypeSymbol type,
        TypeSymbol? convertedType = null)
        => new(kind, value, type, convertedType);

    public BoundLocalAccess CreateLocalAccess(
        LocalSymbol local,
        BoundExpressionReason reason = BoundExpressionReason.None)
        => new(local, reason);

    public BoundLocalAssignmentExpression CreateLocalAssignmentExpression(LocalSymbol local, BoundLocalAccess left, BoundExpression right)
        => new(local, left, right, Compilation.UnitTypeSymbol);

    public BoundLocalDeclarationStatement CreateLocalDeclarationStatement(
        IEnumerable<BoundVariableDeclarator> declarators,
        bool isUsing = false)
        => new(declarators, isUsing);

    public BoundMatchExpression CreateMatchExpression(
        BoundExpression expression,
        ImmutableArray<BoundMatchArm> arms,
        TypeSymbol type)
        => new(expression, arms, type);

    public BoundMemberAccessExpression CreateMemberAccessExpression(
        BoundExpression? receiver,
        Symbol member,
        BoundExpressionReason reason = BoundExpressionReason.None)
        => new(receiver, member, reason);

    public BoundMemberAssignmentExpression CreateMemberAssignmentExpression(
        Symbol member,
        BoundExpression receiver,
        BoundExpression right)
        => new(member, receiver, right, Compilation.UnitTypeSymbol);

    public BoundMethodGroupExpression CreateMethodGroupExpression(
        BoundExpression? receiver,
        ImmutableArray<MethodSymbol> methods,
        TypeSymbol methodGroupType,
        Func<TypeSymbol?>? delegateTypeFactory = null,
        MethodSymbol? selectedMethod = null,
        BoundExpressionReason reason = BoundExpressionReason.None)
        => new(receiver, methods, methodGroupType, delegateTypeFactory, selectedMethod, reason);

    public BoundNamespaceExpression CreateNamespaceExpression(NamespaceSymbol @namespace)
        => new(@namespace);

    public BoundNotPattern CreateNotPattern(BoundPattern pattern)
        => new(pattern);

    public BoundObjectCreationExpression CreateObjectCreationExpression(
        MethodSymbol constructor,
        IEnumerable<BoundExpression> arguments,
        BoundExpression? receiver = null)
        => new(constructor, arguments, receiver);

    public BoundOrPattern CreateOrPattern(BoundPattern left, BoundPattern right)
        => new(left, right);

    public BoundParameterAccess CreateParameterAccess(
        ParameterSymbol parameter,
        BoundExpressionReason reason = BoundExpressionReason.None)
        => new(parameter, reason);

    public BoundParameterAssignmentExpression CreateParameterAssignmentExpression(
        ParameterSymbol parameter,
        BoundExpression left,
        BoundExpression right)
        => new(parameter, left, right, Compilation.UnitTypeSymbol);

    public BoundParenthesizedExpression CreateParenthesizedExpression(BoundExpression expression)
        => new(expression);

    public BoundPatternAssignmentExpression CreatePatternAssignmentExpression(
        TypeSymbol type,
        BoundPattern pattern,
        BoundExpression right)
        => new(type, pattern, right, Compilation.UnitTypeSymbol);

    public BoundPropertyAccess CreatePropertyAccess(
        PropertySymbol property,
        BoundExpressionReason reason = BoundExpressionReason.None)
        => new(property, reason);

    public BoundPropertyAssignmentExpression CreatePropertyAssignmentExpression(
        BoundExpression? receiver,
        PropertySymbol property,
        BoundExpression right)
        => new(receiver, property, receiver, right, Compilation.UnitTypeSymbol);

    public BoundReturnStatement CreateReturnStatement(BoundExpression? expression)
        => new(expression);

    public BoundSelfExpression CreateSelfExpression(
        TypeSymbol type,
        BoundExpressionReason reason = BoundExpressionReason.None)
        => new(type, reason);

    public BoundSingleVariableDesignator CreateSingleVariableDesignator(
        LocalSymbol local,
        BoundExpressionReason reason = BoundExpressionReason.None)
        => new(local, reason);

    public BoundSpreadElement CreateSpreadElement(BoundExpression expression)
        => new(expression);

    public BoundThrowStatement CreateThrowStatement(BoundExpression expression)
        => new(expression);

    public BoundTryExpression CreateTryExpression(
        BoundExpression expression,
        TypeSymbol exceptionType,
        TypeSymbol type,
        MethodSymbol okConstructor,
        MethodSymbol errorConstructor)
        => new(expression, exceptionType, type, okConstructor, errorConstructor);

    public BoundTryStatement CreateTryStatement(
        BoundBlockStatement tryBlock,
        ImmutableArray<BoundCatchClause> catchClauses,
        BoundBlockStatement? finallyBlock)
        => new(tryBlock, catchClauses, finallyBlock);

    public BoundTupleExpression CreateTupleExpression(
        IEnumerable<BoundExpression> elements,
        TypeSymbol type)
        => new(elements, type);

    public BoundTypeExpression CreateTypeExpression(TypeSymbol typeSymbol)
        => new(typeSymbol);

    public BoundTypeOfExpression CreateTypeOfExpression(TypeSymbol operandType, TypeSymbol systemType)
        => new(operandType, systemType);

    public BoundUnaryExpression CreateUnaryExpression(BoundUnaryOperator @operator, BoundExpression operand)
        => new(@operator, operand);

    public BoundUnitExpression CreateUnitExpression(BoundExpressionReason reason = BoundExpressionReason.None)
        => new(Compilation.UnitTypeSymbol, reason);

    public BoundVariableDeclarator CreateVariableDeclarator(LocalSymbol local, BoundExpression? initializer)
        => new(local, initializer);

    public BoundVariableExpression CreateVariableExpression(LocalSymbol variable)
        => new(variable);

    public BoundWhileStatement CreateWhileStatement(BoundExpression condition, BoundStatement body)
        => new(condition, body);

    public BoundYieldStatement CreateYieldStatement(
        BoundExpression expression,
        TypeSymbol elementType,
        IteratorMethodKind iteratorKind)
        => new(expression, elementType, iteratorKind);
}
