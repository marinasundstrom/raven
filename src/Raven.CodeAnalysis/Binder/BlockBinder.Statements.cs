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

        for (int i = 0; i < first.Parameters.Length; i++)
        {
            if (!SymbolEqualityComparer.Default.Equals(first.Parameters[i].Type, second.Parameters[i].Type))
                return false;
        }

        return true;
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
                    ? new BoundUnitExpression(Compilation.GetSpecialType(SpecialType.System_Unit))
                    : BindExpression(ret.Expression);
                bound = new BoundExpressionStatement(expr);
            }
            else
            {
                bound = BindStatement(stmt);
                if (!allowReturn && bound is BoundReturnStatement br)
                {
                    _diagnostics.ReportReturnStatementInExpression(stmt.GetLocation());
                    var expr = br.Expression ?? new BoundUnitExpression(Compilation.GetSpecialType(SpecialType.System_Unit));
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

        ITypeSymbol elementType = collection.Type is IArrayTypeSymbol array
            ? array.ElementType
            : Compilation.GetSpecialType(SpecialType.System_Object);

        var local = loopBinder.CreateLocalSymbol(forStmt, forStmt.Identifier.ValueText, isMutable: false, elementType);

        var body = loopBinder.BindStatementInLoop(forStmt.Body);

        return new BoundForStatement(local, collection, body);
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
            if (expr is null)
            {
                var unit = Compilation.GetSpecialType(SpecialType.System_Unit);
                if (!IsAssignable(method.ReturnType, unit, out _))
                    _diagnostics.ReportCannotConvertFromTypeToType(
                        unit.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        method.ReturnType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        returnStatement.GetLocation());
            }
            else if (expr is not null &&
                     ShouldAttemptConversion(expr) &&
                     method.ReturnType.TypeKind != TypeKind.Error)
            {
                if (!IsAssignable(method.ReturnType, expr.Type, out var conversion))
                {
                    _diagnostics.ReportCannotConvertFromTypeToType(
                        expr.Type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        method.ReturnType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        returnStatement.Expression!.GetLocation());
                }
                else
                {
                    expr = ApplyConversion(expr, method.ReturnType, conversion, returnStatement.Expression!);
                }
            }
        }

        return new BoundReturnStatement(expr);
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
