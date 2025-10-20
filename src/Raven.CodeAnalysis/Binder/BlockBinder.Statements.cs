using System;
using System.Collections.Generic;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

partial class BlockBinder
{
    private BoundStatement BindExpressionStatement(ExpressionStatementSyntax expressionStmt)
    {
        var expr = BindExpression(expressionStmt.Expression, allowReturn: true);

        if (expr is BoundMethodGroupExpression methodGroup && methodGroup.GetConvertedType() is null)
        {
            expr = ReportMethodGroupRequiresDelegate(methodGroup, expressionStmt.Expression);
            CacheBoundNode(expressionStmt.Expression, expr);
        }

        return ExpressionToStatement(expr);
    }

    private BoundStatement BindIfStatement(IfStatementSyntax ifStmt)
    {
        var condition = BindExpression(ifStmt.Condition);
        var thenBound = BindStatement(ifStmt.ThenStatement);
        BoundStatement? elseBound = null;
        if (ifStmt.ElseStatement is not null)
            elseBound = BindStatement(ifStmt.ElseStatement);
        return new BoundIfStatement(condition, thenBound, elseBound);
    }

    private BoundStatement BindWhileStatement(WhileStatementSyntax whileStmt)
    {
        var condition = BindExpression(whileStmt.Condition);

        var body = BindStatementInLoop(whileStmt.Statement);

        return new BoundWhileStatement(condition, body);
    }

    private BoundStatement BindTryStatement(TryStatementSyntax tryStmt)
    {
        var tryBlock = BindBlockStatement(tryStmt.Block);

        var catchBuilder = ImmutableArray.CreateBuilder<BoundCatchClause>();
        foreach (var catchClause in tryStmt.CatchClauses)
        {
            catchBuilder.Add(BindCatchClause(catchClause));
        }

        BoundBlockStatement? finallyBlock = null;
        if (tryStmt.FinallyClause is { } finallyClause)
            finallyBlock = BindBlockStatement(finallyClause.Block);

        if (catchBuilder.Count == 0 && finallyBlock is null)
            return tryBlock;

        return new BoundTryStatement(tryBlock, catchBuilder.ToImmutable(), finallyBlock);
    }

    public Dictionary<string, IMethodSymbol> _functions = new();

    protected static bool HaveSameSignature(IMethodSymbol first, IMethodSymbol second)
    {
        if (first.Parameters.Length != second.Parameters.Length)
            return false;

        if (first.TypeParameters.Length != second.TypeParameters.Length)
            return false;

        for (int i = 0; i < first.Parameters.Length; i++)
        {
            var firstType = first.Parameters[i].Type;
            var secondType = second.Parameters[i].Type;

            if (!AreTypesEquivalent(firstType, secondType, first, second))
                return false;
        }

        return true;

        static bool AreTypesEquivalent(ITypeSymbol firstType, ITypeSymbol secondType, IMethodSymbol firstMethod, IMethodSymbol secondMethod)
        {
            if (SymbolEqualityComparer.Default.Equals(firstType, secondType))
                return true;

            if (firstType is ITypeParameterSymbol firstParam && secondType is ITypeParameterSymbol secondParam)
            {
                if (!SymbolEqualityComparer.Default.Equals(firstParam.ContainingSymbol, firstMethod) ||
                    !SymbolEqualityComparer.Default.Equals(secondParam.ContainingSymbol, secondMethod))
                {
                    return false;
                }

                if (firstParam is SourceTypeParameterSymbol firstSource && secondParam is SourceTypeParameterSymbol secondSource)
                    return firstSource.Ordinal == secondSource.Ordinal;

                return SymbolEqualityComparer.Default.Equals(firstParam, secondParam);
            }

            if (firstType is INamedTypeSymbol firstNamed && secondType is INamedTypeSymbol secondNamed)
            {
                if (!SymbolEqualityComparer.Default.Equals(firstNamed.ConstructedFrom, secondNamed.ConstructedFrom))
                    return false;

                var firstArguments = firstNamed.TypeArguments;
                var secondArguments = secondNamed.TypeArguments;

                if (firstArguments.Length != secondArguments.Length)
                    return false;

                for (int i = 0; i < firstArguments.Length; i++)
                {
                    if (!AreTypesEquivalent(firstArguments[i], secondArguments[i], firstMethod, secondMethod))
                        return false;
                }

                return true;
            }

            if (firstType is IArrayTypeSymbol firstArray && secondType is IArrayTypeSymbol secondArray)
            {
                if (firstArray.Rank != secondArray.Rank)
                    return false;

                return AreTypesEquivalent(firstArray.ElementType, secondArray.ElementType, firstMethod, secondMethod);
            }

            return false;
        }
    }

    public virtual BoundBlockStatement BindBlockStatement(BlockStatementSyntax block)
    {
        if (TryGetCachedBoundNode(block) is BoundStatement cached)
            return (BoundBlockStatement)cached;

        _scopeDepth++;
        var depth = _scopeDepth;

        EnsureLabelsDeclared(block);

        foreach (var stmt in block.Statements)
        {
            if (stmt is FunctionStatementSyntax func)
            {
                var functionBinder = SemanticModel.GetBinder(func, this);
                if (functionBinder is FunctionBinder lfBinder)
                {
                    var symbol = lfBinder.GetMethodSymbol();
                    if (_functions.TryGetValue(symbol.Name, out var existing) && HaveSameSignature(existing, symbol))
                        _diagnostics.ReportFunctionAlreadyDefined(symbol.Name, func.Identifier.GetLocation());
                    else
                        _functions[symbol.Name] = symbol;
                }
            }
        }

        var boundStatements = new List<BoundStatement>(block.Statements.Count);
        foreach (var stmt in block.Statements)
        {
            var bound = BindStatement(stmt);
            boundStatements.Add(bound);
        }

        var localsAtDepth = _localsToDispose
            .Where(l => l.Depth == depth)
            .Select(l => l.Local)
            .ToList();

        if (localsAtDepth.Count > 0)
            _localsToDispose.RemoveAll(l => l.Depth == depth);

        var blockStmt = new BoundBlockStatement(boundStatements.ToArray(), localsAtDepth.ToImmutableArray());
        CacheBoundNode(block, blockStmt);

        foreach (var name in _locals.Where(kvp => kvp.Value.Depth == depth).Select(kvp => kvp.Key).ToList())
            _locals.Remove(name);

        _scopeDepth--;
        return blockStmt;
    }

    public virtual BoundBlockExpression BindBlock(BlockSyntax block, bool allowReturn = true)
    {
        if (TryGetCachedBoundNode(block) is BoundExpression cached)
            return (BoundBlockExpression)cached;

        _scopeDepth++;
        var depth = _scopeDepth;

        if (!allowReturn)
            _expressionContextDepth++;

        EnsureLabelsDeclared(block);

        foreach (var stmt in block.Statements)
        {
            if (stmt is FunctionStatementSyntax func)
            {
                var functionBinder = SemanticModel.GetBinder(func, this);
                if (functionBinder is FunctionBinder lfBinder)
                {
                    var symbol = lfBinder.GetMethodSymbol();
                    if (_functions.TryGetValue(symbol.Name, out var existing) && HaveSameSignature(existing, symbol))
                        _diagnostics.ReportFunctionAlreadyDefined(symbol.Name, func.Identifier.GetLocation());
                    else
                        _functions[symbol.Name] = symbol;
                }
            }
        }

        var boundStatements = new List<BoundStatement>(block.Statements.Count);
        foreach (var stmt in block.Statements)
        {
            BoundStatement bound;
            if (!allowReturn && stmt is ReturnStatementSyntax ret)
            {
                _diagnostics.ReportReturnStatementInExpression(stmt.GetLocation());
                var expr = ret.Expression is null
                    ? BoundFactory.UnitExpression()
                    : BindExpression(ret.Expression);
                bound = new BoundExpressionStatement(expr);
            }
            else
            {
                bound = BindStatement(stmt);
                if (!allowReturn && bound is BoundReturnStatement br)
                {
                    _diagnostics.ReportReturnStatementInExpression(stmt.GetLocation());
                    var expr = br.Expression ?? BoundFactory.UnitExpression();
                    bound = new BoundExpressionStatement(expr);
                }
            }
            boundStatements.Add(bound);
        }

        var unitType = Compilation.GetSpecialType(SpecialType.System_Unit);
        var localsAtDepth = _localsToDispose
            .Where(l => l.Depth == depth)
            .Select(l => l.Local)
            .ToList();

        if (localsAtDepth.Count > 0)
            _localsToDispose.RemoveAll(l => l.Depth == depth);

        var blockExpr = new BoundBlockExpression(boundStatements.ToArray(), unitType, localsAtDepth.ToImmutableArray());
        CacheBoundNode(block, blockExpr);

        foreach (var name in _locals.Where(kvp => kvp.Value.Depth == depth).Select(kvp => kvp.Key).ToList())
            _locals.Remove(name);

        _scopeDepth--;
        if (!allowReturn)
            _expressionContextDepth--;
        return blockExpr;
    }

    private BoundStatement BindForStatement(ForStatementSyntax forStmt)
    {
        var loopBinder = (BlockBinder)SemanticModel.GetBinder(forStmt, this)!;

        var collection = BindExpression(forStmt.Expression);

        var iteration = ClassifyForIteration(collection);

        ILocalSymbol? local = null;
        if (forStmt.Identifier.Kind is not SyntaxKind.None and not SyntaxKind.UnderscoreToken)
        {
            local = loopBinder.CreateLocalSymbol(forStmt, forStmt.Identifier.ValueText, isMutable: false, iteration.ElementType);
        }

        var body = loopBinder.BindStatementInLoop(forStmt.Body);

        return new BoundForStatement(local, iteration, collection, body);
    }

    private ForIterationInfo ClassifyForIteration(BoundExpression collection)
    {
        var collectionType = collection.Type;
        var elementType = InferForElementType(collectionType, out var enumerableInterface);

        if (collectionType is IArrayTypeSymbol arrayType)
            return ForIterationInfo.ForArray(arrayType);

        if (enumerableInterface is { } &&
            enumerableInterface.TypeArguments.Length == 1 &&
            enumerableInterface.TypeArguments[0].TypeKind != TypeKind.Error)
        {
            var enumeratorDefinition = (INamedTypeSymbol)Compilation.GetSpecialType(
                SpecialType.System_Collections_Generic_IEnumerator_T);

            if (enumeratorDefinition.TypeKind != TypeKind.Error)
            {
                var enumeratorInterface = (INamedTypeSymbol)enumeratorDefinition.Construct(
                    enumerableInterface.TypeArguments[0]);
                return ForIterationInfo.ForGeneric(enumerableInterface, enumeratorInterface);
            }
        }

        return ForIterationInfo.ForNonGeneric(elementType);
    }

    private ITypeSymbol InferForElementType(ITypeSymbol? collectionType, out INamedTypeSymbol? enumerableInterface)
    {
        var objectType = Compilation.GetSpecialType(SpecialType.System_Object);
        enumerableInterface = null;

        if (collectionType is null)
            return objectType;

        if (collectionType is IArrayTypeSymbol array)
            return array.ElementType;

        if (collectionType is INamedTypeSymbol named)
        {
            if (TryGetGenericEnumerableElement(named, out var elementType, out enumerableInterface))
                return elementType;

            foreach (var iface in named.AllInterfaces)
            {
                if (TryGetGenericEnumerableElement(iface, out elementType, out enumerableInterface))
                    return elementType;
            }

            if (TryGetEnumeratorElementType(named, out elementType))
                return elementType;
        }

        if (collectionType is ITypeParameterSymbol typeParameter)
        {
            foreach (var constraint in typeParameter.ConstraintTypes)
            {
                var inferred = InferForElementType(constraint, out var constraintEnumerable);
                if (!SymbolEqualityComparer.Default.Equals(inferred, objectType))
                {
                    enumerableInterface ??= constraintEnumerable;
                    return inferred;
                }
            }
        }

        return objectType;
    }

    private bool TryGetGenericEnumerableElement(
        ITypeSymbol type,
        out ITypeSymbol elementType,
        out INamedTypeSymbol? enumerableInterface)
    {
        if (type is INamedTypeSymbol named &&
            named.TypeArguments.Length == 1)
        {
            var enumerableDefinition = (INamedTypeSymbol)Compilation.GetSpecialType(
                SpecialType.System_Collections_Generic_IEnumerable_T);

            if (enumerableDefinition.TypeKind != TypeKind.Error &&
                SymbolEqualityComparer.Default.Equals(GetEnumerableDefinition(named), enumerableDefinition))
            {
                elementType = named.TypeArguments[0];
                enumerableInterface = (INamedTypeSymbol)enumerableDefinition.Construct(elementType);
                return true;
            }
        }

        elementType = null!;
        enumerableInterface = null;
        return false;
    }

    private static INamedTypeSymbol GetEnumerableDefinition(INamedTypeSymbol symbol)
    {
        if (symbol.OriginalDefinition is INamedTypeSymbol original)
            return original;

        if (symbol.ConstructedFrom is INamedTypeSymbol constructed)
            return constructed;

        return symbol;
    }

    private bool TryGetEnumeratorElementType(INamedTypeSymbol type, out ITypeSymbol elementType)
    {
        ITypeSymbol? nonGenericElementType = null;
        var genericEnumeratorDefinition = GetGenericEnumeratorDefinition();
        var nonGenericEnumeratorDefinition = GetNonGenericEnumeratorDefinition();

        foreach (var member in type.GetMembers("GetEnumerator"))
        {
            if (member is not IMethodSymbol { Parameters.Length: 0 } getEnumerator)
                continue;

            var returnType = getEnumerator.ReturnType;

            if (returnType is IArrayTypeSymbol array)
            {
                elementType = array.ElementType;
                return true;
            }

            if (returnType is INamedTypeSymbol named)
            {
                if (named.TypeArguments.Length == 1 &&
                    genericEnumeratorDefinition.TypeKind != TypeKind.Error &&
                    SymbolEqualityComparer.Default.Equals(
                        GetEnumerableDefinition(named),
                        genericEnumeratorDefinition))
                {
                    elementType = named.TypeArguments[0];
                    return true;
                }

                foreach (var iface in named.AllInterfaces)
                {
                    if (iface is INamedTypeSymbol { TypeArguments.Length: 1 } genericEnumerator &&
                        genericEnumeratorDefinition.TypeKind != TypeKind.Error &&
                        SymbolEqualityComparer.Default.Equals(
                            GetEnumerableDefinition(genericEnumerator),
                            genericEnumeratorDefinition))
                    {
                        elementType = genericEnumerator.TypeArguments[0];
                        return true;
                    }
                }

                if (nonGenericElementType is null &&
                    nonGenericEnumeratorDefinition.TypeKind != TypeKind.Error &&
                    SymbolEqualityComparer.Default.Equals(
                        GetEnumerableDefinition(named),
                        nonGenericEnumeratorDefinition))
                {
                    nonGenericElementType = Compilation.GetSpecialType(SpecialType.System_Object);
                }
            }
        }

        if (nonGenericElementType is not null)
        {
            elementType = nonGenericElementType;
            return true;
        }

        elementType = null!;
        return false;
    }

    private BoundCatchClause BindCatchClause(CatchClauseSyntax catchClause)
    {
        var exceptionBase = Compilation.GetTypeByMetadataName("System.Exception") ?? Compilation.ErrorTypeSymbol;
        var exceptionType = exceptionBase;

        SourceLocalSymbol? localSymbol = null;

        if (catchClause.Declaration is { } declaration)
        {
            var declaredType = ResolveType(declaration.Type);
            exceptionType = declaredType;

            if (exceptionBase.TypeKind != TypeKind.Error &&
                declaredType.TypeKind != TypeKind.Error &&
                !IsAssignable(exceptionBase, declaredType, out _))
            {
                _diagnostics.ReportCatchTypeMustDeriveFromSystemException(
                    declaredType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    declaration.Type.GetLocation());
            }

            if (declaration.Identifier is { } identifier &&
                !identifier.IsMissing &&
                identifier.Kind == SyntaxKind.IdentifierToken)
            {
                var name = identifier.Text;
                if (_locals.TryGetValue(name, out var existing) && existing.Depth == _scopeDepth)
                {
                    _diagnostics.ReportVariableAlreadyDefined(name, identifier.GetLocation());
                    localSymbol = existing.Symbol as SourceLocalSymbol;
                }
                else
                {
                    localSymbol = CreateLocalSymbol(declaration, name, isMutable: false, declaredType);
                }
            }
        }

        var block = BindBlockStatement(catchClause.Block);

        if (localSymbol is not null)
            _locals.Remove(localSymbol.Name);

        return new BoundCatchClause(exceptionType, localSymbol, block);
    }

    private BoundStatement ExpressionToStatement(BoundExpression expression)
    {
        return expression switch
        {
            BoundIfExpression ifExpr => new BoundIfStatement(
                ifExpr.Condition,
                ExpressionToStatement(ifExpr.ThenBranch),
                ifExpr.ElseBranch is not null ? ExpressionToStatement(ifExpr.ElseBranch) : null),
            BoundBlockExpression blockExpr => new BoundBlockStatement(blockExpr.Statements, blockExpr.LocalsToDispose),
            BoundAssignmentExpression assignmentExpr => new BoundAssignmentStatement(assignmentExpr),
            _ => new BoundExpressionStatement(expression),
        };
    }

    private BoundStatement BindReturnStatement(ReturnStatementSyntax returnStatement)
    {
        BoundExpression? expr = null;

        if (returnStatement.Expression is not null)
            expr = BindExpression(returnStatement.Expression);

        if (_containingSymbol is IMethodSymbol method)
        {
            var skipReturnConversions = method switch
            {
                SourceMethodSymbol { HasAsyncReturnTypeError: true } => true,
                SourceMethodSymbol { ShouldDeferAsyncReturnDiagnostics: true } => true,
                SourceLambdaSymbol { HasAsyncReturnTypeError: true } => true,
                _ => false,
            };

            if (!skipReturnConversions)
            {
                if (expr is null)
                {
                    var unit = Compilation.GetSpecialType(SpecialType.System_Unit);
                    if (!IsAssignable(method.ReturnType, unit, out _))
                        _diagnostics.ReportCannotConvertFromTypeToType(
                            unit.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                            method.ReturnType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                            returnStatement.GetLocation());
                }
                else if (method.IsAsync &&
                    method.ReturnType.SpecialType == SpecialType.System_Threading_Tasks_Task)
                {
                    var methodDisplay = method.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat);
                    _diagnostics.ReportAsyncTaskReturnCannotHaveExpression(
                        methodDisplay,
                        returnStatement.Expression!.GetLocation());
                }
                else
                {
                    var targetType = method.ReturnType;

                    if (method.IsAsync &&
                        method.ReturnType is INamedTypeSymbol namedReturn &&
                        namedReturn.OriginalDefinition.SpecialType == SpecialType.System_Threading_Tasks_Task_T &&
                        namedReturn.TypeArguments.Length == 1 &&
                        namedReturn.TypeArguments[0] is { } resultType)
                    {
                        targetType = resultType;
                    }

                    if (ShouldAttemptConversion(expr) && targetType.TypeKind != TypeKind.Error)
                    {
                        if (!IsAssignable(targetType, expr.Type, out var conversion))
                        {
                            _diagnostics.ReportCannotConvertFromTypeToType(
                                expr.Type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                                targetType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                                returnStatement.Expression!.GetLocation());
                        }
                        else
                        {
                            expr = ApplyConversion(expr, targetType, conversion, returnStatement.Expression!);
                        }
                    }
                }
            }
        }

        return new BoundReturnStatement(expr);
    }

    private BoundStatement BindYieldReturnStatement(YieldReturnStatementSyntax yieldReturn)
    {
        if (_expressionContextDepth > 0)
        {
            var exprInExpressionContext = BindExpression(yieldReturn.Expression);
            return new BoundExpressionStatement(exprInExpressionContext);
        }

        var expression = BindExpression(yieldReturn.Expression);
        var (kind, elementType) = ResolveIteratorInfoForCurrentMethod();

        if (elementType.TypeKind == TypeKind.Error)
            elementType = Compilation.ErrorTypeSymbol;

        if (ShouldAttemptConversion(expression) &&
            expression.Type is { TypeKind: not TypeKind.Error } expressionType &&
            elementType.TypeKind != TypeKind.Error &&
            IsAssignable(elementType, expressionType, out var conversion))
        {
            expression = ApplyConversion(expression, elementType, conversion, yieldReturn.Expression);
        }

        return new BoundYieldReturnStatement(expression, elementType, kind);
    }

    private BoundStatement BindYieldBreakStatement(YieldBreakStatementSyntax yieldBreak)
    {
        if (_expressionContextDepth > 0)
        {
            var unit = BoundFactory.UnitExpression();
            return new BoundExpressionStatement(unit);
        }

        var (kind, elementType) = ResolveIteratorInfoForCurrentMethod();
        if (elementType.TypeKind == TypeKind.Error)
            elementType = Compilation.ErrorTypeSymbol;

        return new BoundYieldBreakStatement(elementType, kind);
    }

    private BoundStatement BindThrowStatement(ThrowStatementSyntax throwStatement)
    {
        var location = throwStatement.ThrowKeyword.GetLocation();

        if (_expressionContextDepth > 0)
            _diagnostics.ReportThrowStatementInExpression(location);

        var expression = BindExpression(throwStatement.Expression);

        var exceptionBase = Compilation.GetTypeByMetadataName("System.Exception")
            ?? Compilation.ErrorTypeSymbol;

        if (ShouldAttemptConversion(expression) &&
            expression.Type is { TypeKind: not TypeKind.Error } expressionType &&
            exceptionBase.TypeKind != TypeKind.Error)
        {
            if (!IsAssignable(exceptionBase, expressionType, out var conversion))
            {
                _diagnostics.ReportThrowExpressionMustBeException(
                    expressionType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    throwStatement.Expression.GetLocation());
            }
            else
            {
                expression = ApplyConversion(expression, exceptionBase, conversion, throwStatement.Expression);
            }
        }

        return new BoundThrowStatement(expression);
    }

    private BoundStatement BindLabeledStatement(LabeledStatementSyntax labeledStatement)
    {
        if (_expressionContextDepth > 0)
            _diagnostics.ReportLabelInExpression(labeledStatement.Identifier.GetLocation());

        var labelSymbol = DeclareLabelSymbol(labeledStatement);
        var boundStatement = BindStatement(labeledStatement.Statement);
        var bound = new BoundLabeledStatement(labelSymbol, boundStatement);
        CacheBoundNode(labeledStatement, bound);
        return bound;
    }

    private BoundStatement BindGotoStatement(GotoStatementSyntax gotoStatement)
    {
        if (_expressionContextDepth > 0)
            _diagnostics.ReportGotoStatementInExpression(gotoStatement.GotoKeyword.GetLocation());

        var identifier = gotoStatement.Identifier;
        if (identifier.IsMissing)
        {
            var errorSymbol = CreateLabelSymbol(string.Empty, identifier.GetLocation(), gotoStatement.GetReference());
            var boundError = new BoundGotoStatement(errorSymbol);
            CacheBoundNode(gotoStatement, boundError);
            return boundError;
        }

        if (SyntaxFacts.IsReservedWordKind(identifier.Kind))
        {
            var identifierName = identifier.ValueText;
            _diagnostics.ReportReservedWordCannotBeLabel(identifierName, identifier.GetLocation());
            var errorSymbol = CreateLabelSymbol(string.Empty, identifier.GetLocation(), gotoStatement.GetReference());
            var boundError = new BoundGotoStatement(errorSymbol);
            CacheBoundNode(gotoStatement, boundError);
            return boundError;
        }

        var name = identifier.ValueText;
        if (!_labelsByName.TryGetValue(name, out var label))
        {
            _diagnostics.ReportLabelNotFound(name, identifier.GetLocation());
            label = CreateLabelSymbol(name, identifier.GetLocation(), gotoStatement.GetReference());
        }

        var isBackward = false;
        if (_syntaxByLabel.TryGetValue(label, out var labeledSyntax))
            isBackward = labeledSyntax.Span.Start < gotoStatement.Span.Start;

        SemanticModel?.RegisterGoto(gotoStatement, label);

        var bound = new BoundGotoStatement(label, isBackward);
        CacheBoundNode(gotoStatement, bound);
        return bound;
    }

    private BoundStatement BindBreakStatement(BreakStatementSyntax breakStatement)
    {
        var location = breakStatement.BreakKeyword.GetLocation();

        if (_expressionContextDepth > 0)
        {
            _diagnostics.ReportBreakStatementInExpression(location);
        }
        else if (_loopDepth == 0)
        {
            _diagnostics.ReportBreakStatementNotWithinLoop(location);
        }

        var bound = new BoundBreakStatement();
        CacheBoundNode(breakStatement, bound);
        return bound;
    }

    private BoundStatement BindContinueStatement(ContinueStatementSyntax continueStatement)
    {
        var location = continueStatement.ContinueKeyword.GetLocation();

        if (_expressionContextDepth > 0)
        {
            _diagnostics.ReportContinueStatementInExpression(location);
        }
        else if (_loopDepth == 0)
        {
            _diagnostics.ReportContinueStatementNotWithinLoop(location);
        }

        var bound = new BoundContinueStatement();
        CacheBoundNode(continueStatement, bound);
        return bound;
    }

    public BoundStatement BindStatementInLoop(StatementSyntax syntax)
    {
        var previous = EnterLoop();
        try
        {
            return BindStatement(syntax);
        }
        finally
        {
            ExitLoop(previous);
        }
    }

    private int EnterLoop()
    {
        var previous = _loopDepth;
        _loopDepth++;
        return previous;
    }

    private void ExitLoop(int previous)
    {
        _loopDepth = previous;
    }

    private void EnsureLabelsDeclared(SyntaxNode node)
    {
        if (!_labelDeclarationNodes.Add(node))
            return;

        var stack = new Stack<SyntaxNode>();
        stack.Push(node);

        while (stack.Count > 0)
        {
            var current = stack.Pop();

            if (current is FunctionStatementSyntax)
                continue;

            if (current is LabeledStatementSyntax labeled)
            {
                _ = DeclareLabelSymbol(labeled);
                stack.Push(labeled.Statement);
                continue;
            }

            foreach (var child in current.ChildNodes())
                stack.Push(child);
        }
    }

    private ILabelSymbol DeclareLabelSymbol(LabeledStatementSyntax labeledStatement)
    {
        if (_labelsBySyntax.TryGetValue(labeledStatement, out var existing))
            return existing;

        var identifier = labeledStatement.Identifier;
        if (identifier.IsMissing)
            return CreateLabelSymbol(string.Empty, identifier.GetLocation(), labeledStatement.GetReference());

        if (SyntaxFacts.IsReservedWordKind(identifier.Kind))
        {
            var identifierName = identifier.ValueText;
            _diagnostics.ReportReservedWordCannotBeLabel(identifierName, identifier.GetLocation());
            return CreateLabelSymbol(string.Empty, identifier.GetLocation(), labeledStatement.GetReference());
        }

        var name = identifier.ValueText;

        if (_labelsByName.TryGetValue(name, out var conflict))
        {
            _diagnostics.ReportLabelAlreadyDefined(name, identifier.GetLocation());
            return conflict;
        }

        var symbol = CreateLabelSymbol(name, identifier.GetLocation(), labeledStatement.GetReference());

        _labelsByName[name] = symbol;
        _labelsBySyntax[labeledStatement] = symbol;
        _syntaxByLabel[symbol] = labeledStatement;
        SemanticModel?.RegisterLabel(labeledStatement, symbol);

        return symbol;
    }

    private INamedTypeSymbol GetGenericEnumeratorDefinition()
        => (INamedTypeSymbol)Compilation.GetSpecialType(
            SpecialType.System_Collections_Generic_IEnumerator_T);

    private INamedTypeSymbol GetNonGenericEnumeratorDefinition()
        => (INamedTypeSymbol)Compilation.GetSpecialType(
            SpecialType.System_Collections_IEnumerator);

    private (IteratorMethodKind Kind, ITypeSymbol ElementType) ResolveIteratorInfoForCurrentMethod()
    {
        if (_containingSymbol is not IMethodSymbol method)
            return (IteratorMethodKind.None, Compilation.ErrorTypeSymbol);

        var result = ResolveIteratorInfo(method);

        if (result.Kind != IteratorMethodKind.None && _containingSymbol is SourceMethodSymbol sourceMethod)
            sourceMethod.MarkIterator(result.Kind, result.ElementType);

        return result;
    }

    private (IteratorMethodKind Kind, ITypeSymbol ElementType) ResolveIteratorInfo(IMethodSymbol method)
    {
        var errorType = Compilation.ErrorTypeSymbol;
        var returnType = method.ReturnType;

        if (returnType.SpecialType == SpecialType.System_Collections_IEnumerable)
        {
            var objectType = Compilation.GetSpecialType(SpecialType.System_Object);
            return (IteratorMethodKind.Enumerable, objectType);
        }

        if (returnType.SpecialType == SpecialType.System_Collections_IEnumerator)
        {
            var objectType = Compilation.GetSpecialType(SpecialType.System_Object);
            return (IteratorMethodKind.Enumerator, objectType);
        }

        if (returnType is INamedTypeSymbol named)
        {
            var definition = named;
            if (named.ConstructedFrom is INamedTypeSymbol constructedFrom)
                definition = constructedFrom;

            if (definition.SpecialType == SpecialType.System_Collections_Generic_IEnumerable_T)
            {
                var elementType = named.TypeArguments.Length > 0 ? named.TypeArguments[0] : errorType;
                return (IteratorMethodKind.Enumerable, elementType);
            }

            if (definition.SpecialType == SpecialType.System_Collections_Generic_IEnumerator_T)
            {
                var elementType = named.TypeArguments.Length > 0 ? named.TypeArguments[0] : errorType;
                return (IteratorMethodKind.Enumerator, elementType);
            }
        }

        return (IteratorMethodKind.None, errorType);
    }

    private ILabelSymbol CreateLabelSymbol(string name, Location location, SyntaxReference reference)
    {
        return new LabelSymbol(
            name,
            _containingSymbol,
            _containingSymbol.ContainingType as INamedTypeSymbol,
            _containingSymbol?.ContainingNamespace,
            [location],
            [reference]);
    }
}
