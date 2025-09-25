using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

partial class BlockBinder : Binder
{
    private readonly ISymbol _containingSymbol;
    protected readonly Dictionary<string, (ILocalSymbol Symbol, int Depth)> _locals = new();
    private readonly List<(ILocalSymbol Local, int Depth)> _localsToDispose = new();
    private int _scopeDepth;
    private bool _allowReturnsInExpression;

    public BlockBinder(ISymbol containingSymbol, Binder parent) : base(parent)
    {
        _containingSymbol = containingSymbol;
    }

    public override ISymbol ContainingSymbol => _containingSymbol;

    public override ISymbol? BindDeclaredSymbol(SyntaxNode node)
    {
        return node switch
        {
            VariableDeclaratorSyntax v => BindLocalDeclaration(v).Symbol,
            CompilationUnitSyntax unit => BindCompilationUnit(unit).Symbol,
            SingleVariableDesignationSyntax singleVariableDesignation => BindSingleVariableDesignation(singleVariableDesignation).Local,
            FunctionStatementSyntax functionStatement => BindFunction(functionStatement).Method,
            _ => base.BindDeclaredSymbol(node)
        };
    }

    public override SymbolInfo BindReferencedSymbol(SyntaxNode node)
    {
        return node switch
        {
            ExpressionSyntax expr => BindExpression(expr).GetSymbolInfo(),
            ExpressionStatementSyntax stmt => BindStatement(stmt).GetSymbolInfo(),
            _ => base.BindReferencedSymbol(node)
        };
    }

    internal override SymbolInfo BindIdentifierReference(IdentifierNameSyntax node)
    {
        return BindIdentifierName(node).GetSymbolInfo();
    }

    internal override SymbolInfo BindInvocationReference(InvocationExpressionSyntax node)
    {
        return BindInvocationExpression(node).GetSymbolInfo();
    }

    internal override SymbolInfo BindMemberAccessReference(MemberAccessExpressionSyntax node)
    {
        return BindMemberAccessExpression(node).GetSymbolInfo();
    }

    internal override SymbolInfo BindMemberBindingReference(MemberBindingExpressionSyntax node)
    {
        return BindMemberBindingExpression(node).GetSymbolInfo();
    }

    public override ISymbol? LookupSymbol(string name)
    {
        // Forward to LookupSymbols so import directives and alias mappings are considered
        // when binding identifier references. Previously this method bypassed the import
        // binder, causing namespace or type aliases to be ignored and resulting in missing
        // completions for alias-qualified names.
        return LookupSymbols(name).FirstOrDefault();
    }

    private SymbolInfo BindCompilationUnit(CompilationUnitSyntax compilationUnit)
    {
        var entryPoint = Compilation.GetEntryPoint();
        if (entryPoint is not null && entryPoint.IsImplicitlyDeclared)
        {
            if (entryPoint.DeclaringSyntaxReferences.FirstOrDefault()!.GetSyntax() == compilationUnit)
            {
                return new SymbolInfo(entryPoint);
            }
        }
        return new SymbolInfo(Compilation.SourceGlobalNamespace);
    }

    private BoundLocalDeclarationStatement BindLocalDeclaration(VariableDeclaratorSyntax variableDeclarator)
    {
        var name = variableDeclarator.Identifier.Text;

        if (_locals.TryGetValue(name, out var existing) && existing.Depth == _scopeDepth)
        {
            _diagnostics.ReportVariableAlreadyDefined(name, variableDeclarator.Identifier.GetLocation());

            BoundExpression? existingInitializer = null;
            if (variableDeclarator.Initializer is { } init)
                existingInitializer = BindExpression(init.Value, allowReturn: false);

            return new BoundLocalDeclarationStatement([new BoundVariableDeclarator(existing.Symbol, existingInitializer)]);
        }

        if (LookupSymbol(name) is ILocalSymbol or IParameterSymbol or IFieldSymbol)
            _diagnostics.ReportVariableShadowsOuterScope(name, variableDeclarator.Identifier.GetLocation());

        var decl = variableDeclarator.Parent as VariableDeclarationSyntax;
        var isMutable = decl!.LetOrVarKeyword.IsKind(SyntaxKind.VarKeyword);
        var isUsingDeclaration = decl.Parent is UsingDeclarationStatementSyntax;
        var shouldDispose = isUsingDeclaration;

        ITypeSymbol type = Compilation.ErrorTypeSymbol;
        BoundExpression? boundInitializer = null;
        ITypeSymbol? initializerValueType = null;

        var initializer = variableDeclarator.Initializer;
        var typeLocation = variableDeclarator.TypeAnnotation?.Type.GetLocation()
            ?? decl.LetOrVarKeyword.GetLocation();
        if (initializer is not null)
        {
            // Initializers are always evaluated for their value; return statements
            // are not permitted within them since they would escape the enclosing
            // context. Bind the initializer with returns disallowed so explicit
            // `return` keywords trigger diagnostics rather than method return
            // validation.
            boundInitializer = BindExpression(initializer.Value, allowReturn: false);
            initializerValueType = boundInitializer?.Type;
        }

        if (initializer is null)
        {
            if (variableDeclarator.TypeAnnotation is not null)
                type = ResolveType(variableDeclarator.TypeAnnotation.Type);

            _diagnostics.ReportLocalVariableMustBeInitialized(name, variableDeclarator.Identifier.GetLocation());
            boundInitializer = new BoundErrorExpression(type, null, BoundExpressionReason.OtherError);
        }
        else if (variableDeclarator.TypeAnnotation is null)
        {
            if (boundInitializer is BoundMethodGroupExpression methodGroup)
            {
                var inferredDelegate = methodGroup.DelegateType ?? methodGroup.DelegateTypeFactory?.Invoke();
                if (inferredDelegate is INamedTypeSymbol delegateType && delegateType.TypeKind == TypeKind.Delegate)
                {
                    boundInitializer = ConvertMethodGroupToDelegate(methodGroup, delegateType, initializer.Value);
                    CacheBoundNode(initializer.Value, boundInitializer);
                    type = delegateType;
                }
                else
                {
                    boundInitializer = ReportMethodGroupRequiresDelegate(methodGroup, initializer.Value);
                    CacheBoundNode(initializer.Value, boundInitializer);
                    type = Compilation.ErrorTypeSymbol;
                }
            }
            else
            {
                type = TypeSymbolNormalization.NormalizeForInference(boundInitializer!.Type!);
            }
        }
        else
        {
            type = ResolveType(variableDeclarator.TypeAnnotation.Type);

            if (type.TypeKind != TypeKind.Error &&
                boundInitializer is not null &&
                ShouldAttemptConversion(boundInitializer))
            {
                if (!IsAssignable(type, boundInitializer.Type!, out var conversion))
                {
                    _diagnostics.ReportCannotAssignFromTypeToType(
                        boundInitializer.Type!.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        type.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        initializer.Value.GetLocation());
                    boundInitializer = new BoundErrorExpression(type, null, BoundExpressionReason.TypeMismatch);
                }
                else
                {
                    boundInitializer = ApplyConversion(boundInitializer, type, conversion, initializer.Value);
                    CacheBoundNode(initializer.Value, boundInitializer);
                }
            }
        }

        type = EnsureTypeAccessible(type, typeLocation);

        if (initializer is not null && boundInitializer is not null)
            CacheBoundNode(initializer.Value, boundInitializer);

        if (isUsingDeclaration)
        {
            var disposableType = Compilation.GetSpecialType(SpecialType.System_IDisposable);
            if (disposableType.TypeKind != TypeKind.Error)
            {
                var initializerSupportsDispose = initializerValueType is not null &&
                    initializerValueType.TypeKind != TypeKind.Error &&
                    IsAssignable(disposableType, initializerValueType, out _);

                if (!initializerSupportsDispose && initializerValueType is not null && initializerValueType.TypeKind != TypeKind.Error)
                {
                    _diagnostics.ReportCannotConvertFromTypeToType(
                        initializerValueType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        disposableType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        initializer?.Value.GetLocation() ?? variableDeclarator.Identifier.GetLocation());
                    shouldDispose = false;
                }
                else if (type.TypeKind != TypeKind.Error && !IsAssignable(disposableType, type, out _))
                {
                    _diagnostics.ReportCannotConvertFromTypeToType(
                        type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        disposableType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        variableDeclarator.Identifier.GetLocation());
                    shouldDispose = false;
                }
            }
        }

        var declarator = new BoundVariableDeclarator(CreateLocalSymbol(variableDeclarator, name, isMutable, type), boundInitializer);

        if (shouldDispose)
            _localsToDispose.Add((declarator.Local, _scopeDepth));

        return new BoundLocalDeclarationStatement([declarator], isUsingDeclaration);
    }

    private ITypeSymbol EnsureTypeAccessible(ITypeSymbol type, Location location)
    {
        if (type.TypeKind == TypeKind.Error)
            return type;

        if (EnsureMemberAccessible(type, location, "type"))
            return type;

        return Compilation.ErrorTypeSymbol;
    }

    private bool EnsureMemberAccessible(ISymbol symbol, Location location, string symbolKind)
    {
        if (symbol is null)
            return true;

        if (symbol.DeclaredAccessibility == Accessibility.NotApplicable)
            return true;

        if (IsSymbolAccessible(symbol))
            return true;

        var display = symbol is ITypeSymbol typeSymbol
            ? typeSymbol.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat)
            : symbol.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat);

        _diagnostics.ReportSymbolIsInaccessible(symbolKind, display, location);
        return false;
    }

    private ImmutableArray<IMethodSymbol> GetAccessibleMethods(
        ImmutableArray<IMethodSymbol> methods,
        Location location,
        bool reportIfInaccessible = true)
    {
        if (methods.IsDefaultOrEmpty)
            return methods;

        var builder = ImmutableArray.CreateBuilder<IMethodSymbol>();

        foreach (var method in methods)
        {
            if (IsSymbolAccessible(method))
                builder.Add(method);
        }

        if (builder.Count > 0)
            return builder.ToImmutable();

        if (reportIfInaccessible)
            EnsureMemberAccessible(methods[0], location, "method");
        return ImmutableArray<IMethodSymbol>.Empty;
    }

    private BoundExpression BindMethodGroup(BoundExpression? receiver, ImmutableArray<IMethodSymbol> methods, Location location)
    {
        var accessibleMethods = GetAccessibleMethods(methods, location);

        if (accessibleMethods.IsDefaultOrEmpty)
            return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.Inaccessible);

        return CreateMethodGroup(receiver, accessibleMethods);
    }

    private ImmutableArray<ITypeSymbol>? TryBindTypeArguments(GenericNameSyntax generic)
    {
        if (generic.TypeArgumentList.Arguments.Count == 0)
            return ImmutableArray<ITypeSymbol>.Empty;

        var builder = ImmutableArray.CreateBuilder<ITypeSymbol>(generic.TypeArgumentList.Arguments.Count);

        foreach (var argument in generic.TypeArgumentList.Arguments)
        {
            var bound = BindTypeSyntax(argument.Type);
            if (bound is not BoundTypeExpression bt)
                return null;

            builder.Add(bt.Type);
        }

        return builder.MoveToImmutable();
    }

    private ImmutableArray<IMethodSymbol> InstantiateMethodCandidates(
        ImmutableArray<IMethodSymbol> methods,
        ImmutableArray<ITypeSymbol> typeArguments,
        GenericNameSyntax? typeArgumentSyntax = null,
        Location? fallbackLocation = null)
    {
        if (methods.IsDefaultOrEmpty)
            return methods;

        var builder = ImmutableArray.CreateBuilder<IMethodSymbol>();

        Location GetArgumentLocation(int index)
        {
            if (typeArgumentSyntax is not null)
            {
                var arguments = typeArgumentSyntax.TypeArgumentList.Arguments;
                if (index >= 0 && index < arguments.Count)
                    return arguments[index].GetLocation();
                return typeArgumentSyntax.GetLocation();
            }

            return fallbackLocation ?? Location.None;
        }

        foreach (var method in methods)
        {
            if (method.TypeParameters.Length != typeArguments.Length)
                continue;

            if (!ValidateMethodTypeArgumentConstraints(method, typeArguments, GetArgumentLocation))
                continue;

            try
            {
                var constructed = method.Construct(typeArguments.ToArray());
                builder.Add(constructed);
            }
            catch
            {
                // Ignore methods that cannot be constructed with the provided type arguments.
            }
        }

        return builder.ToImmutable();
    }

    private static string GetSymbolKindForDiagnostic(ISymbol symbol)
    {
        return symbol switch
        {
            IMethodSymbol method when method.MethodKind == MethodKind.Constructor => "constructor",
            IMethodSymbol => "method",
            IPropertySymbol => "property",
            IFieldSymbol => "field",
            INamedTypeSymbol => "type",
            _ => "member"
        };
    }

    private SourceLocalSymbol CreateLocalSymbol(SyntaxNode declaringSyntax, string name, bool isMutable, ITypeSymbol type)
    {
        var symbol = new SourceLocalSymbol(
            name,
            type,
            isMutable,
            _containingSymbol,
            _containingSymbol.ContainingType as INamedTypeSymbol,
            _containingSymbol?.ContainingNamespace,
            [declaringSyntax.GetLocation()],
            [declaringSyntax.GetReference()]);

        _locals[name] = (symbol, _scopeDepth);
        return symbol;
    }

    public override BoundStatement BindStatement(StatementSyntax statement)
    {
        if (TryGetCachedBoundNode(statement) is BoundStatement cached)
            return cached;

        BoundStatement boundNode = statement switch
        {
            LocalDeclarationStatementSyntax localDeclaration => BindLocalDeclaration(localDeclaration.Declaration.Declarators[0]),
            UsingDeclarationStatementSyntax usingDeclaration => BindLocalDeclaration(usingDeclaration.Declaration.Declarators[0]),
            AssignmentStatementSyntax assignmentStatement => BindAssignmentStatement(assignmentStatement),
            ExpressionStatementSyntax expressionStmt => BindExpressionStatement(expressionStmt),
            IfStatementSyntax ifStmt => BindIfStatement(ifStmt),
            TryStatementSyntax tryStmt => BindTryStatement(tryStmt),
            FunctionStatementSyntax function => BindFunction(function),
            ReturnStatementSyntax returnStatement => BindReturnStatement(returnStatement),
            BlockStatementSyntax blockStmt => BindBlockStatement(blockStmt),
            EmptyStatementSyntax emptyStatement => new BoundExpressionStatement(new BoundUnitExpression(Compilation.GetSpecialType(SpecialType.System_Unit))),
            _ => throw new NotSupportedException($"Unsupported statement: {statement.Kind}")
        };

        CacheBoundNode(statement, boundNode);

        return boundNode;
    }

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

                if (LookupSymbol(name) is ILocalSymbol or IParameterSymbol or IFieldSymbol)
                    _diagnostics.ReportVariableShadowsOuterScope(name, identifier.GetLocation());

                _scopeDepth++;
                localSymbol = CreateLocalSymbol(declaration, name, isMutable: false, declaredType);
                _scopeDepth--;
            }
        }

        var block = BindBlockStatement(catchClause.Block);

        return new BoundCatchClause(exceptionType, localSymbol, block);
    }

    private BoundStatement ExpressionToStatement(BoundExpression expression)
    {
        return expression switch
        {
            BoundIfExpression ifExpr => new BoundIfStatement(ifExpr.Condition,
                ExpressionToStatement(ifExpr.ThenBranch),
                ifExpr.ElseBranch is not null ? ExpressionToStatement(ifExpr.ElseBranch) : null),
            BoundWhileExpression whileExpr => new BoundWhileStatement(whileExpr.Condition, ExpressionToStatement(whileExpr.Body)),
            BoundForExpression forExpr => new BoundForStatement(forExpr.Local, forExpr.Collection, ExpressionToStatement(forExpr.Body)),
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

        // Step 1: Pre-declare all functions
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

        // Step 2: Bind and cache all statements
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

        // Step 3: Create and cache the block
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
        return blockExpr;
    }

    public BoundExpression BindExpression(ExpressionSyntax syntax, bool allowReturn)
    {
        var previous = _allowReturnsInExpression;
        _allowReturnsInExpression = allowReturn;
        try
        {
            return BindExpression(syntax);
        }
        finally
        {
            _allowReturnsInExpression = previous;
        }
    }

    public override BoundExpression BindExpression(ExpressionSyntax syntax)
    {
        if (TryGetCachedBoundNode(syntax) is BoundExpression cached)
            return cached;

        var boundNode = syntax switch
        {
            LiteralExpressionSyntax literal => BindLiteralExpression(literal),
            IdentifierNameSyntax identifier => BindIdentifierName(identifier),
            TypeSyntax type => BindTypeSyntax(type),
            BinaryExpressionSyntax binary => BindBinaryExpression(binary),
            InvocationExpressionSyntax invocation => BindInvocationExpression(invocation),
            ObjectCreationExpressionSyntax invocation => BindObjectCreationExpression(invocation),
            MemberAccessExpressionSyntax memberAccess => BindMemberAccessExpression(memberAccess),
            MemberBindingExpressionSyntax memberBinding => BindMemberBindingExpression(memberBinding),
            ConditionalAccessExpressionSyntax conditionalAccess => BindConditionalAccessExpression(conditionalAccess),
            ElementAccessExpressionSyntax elementAccess => BindElementAccessExpression(elementAccess),
            AssignmentExpressionSyntax assignment => BindAssignmentExpression(assignment),
            CollectionExpressionSyntax collection => BindCollectionExpression(collection),
            ParenthesizedExpressionSyntax parenthesizedExpression => BindParenthesizedExpression(parenthesizedExpression),
            CastExpressionSyntax castExpression => BindCastExpression(castExpression),
            AsExpressionSyntax asExpression => BindAsExpression(asExpression),
            TupleExpressionSyntax tupleExpression => BindTupleExpression(tupleExpression),
            IfExpressionSyntax ifExpression => BindIfExpression(ifExpression),
            WhileExpressionSyntax whileExpression => BindWhileExpression(whileExpression),
            ForExpressionSyntax forExpression => BindForExpression(forExpression),
            BlockSyntax block => BindBlock(block, allowReturn: _allowReturnsInExpression),
            IsPatternExpressionSyntax isPatternExpression => BindIsPatternExpression(isPatternExpression),
            MatchExpressionSyntax matchExpression => BindMatchExpression(matchExpression),
            LambdaExpressionSyntax lambdaExpression => BindLambdaExpression(lambdaExpression),
            InterpolatedStringExpressionSyntax interpolated => BindInterpolatedStringExpression(interpolated),
            UnaryExpressionSyntax unaryExpression => BindUnaryExpression(unaryExpression),
            SelfExpressionSyntax selfExpression => BindSelfExpression(selfExpression),
            UnitExpressionSyntax unitExpression => BindUnitExpression(unitExpression),
            ExpressionSyntax.Missing missing => BindMissingExpression(missing),
            _ => throw new NotSupportedException($"Unsupported expression: {syntax.Kind}")
        };

        //CacheBoundNode(syntax, boundNode);

        return boundNode;
    }

    private BoundExpression BindSelfExpression(SelfExpressionSyntax selfExpression)
    {
        if (_containingSymbol is IMethodSymbol method && (!method.IsStatic || method.IsNamedConstructor))
        {
            var containingType = method.ContainingType;
            return new BoundSelfExpression(containingType);
        }

        //_diagnostics.ReportSelfNotAllowed(selfExpression.GetLocation());
        return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
    }

    private BoundExpression BindTupleExpression(TupleExpressionSyntax tupleExpression)
    {
        var elements = new List<BoundExpression>(tupleExpression.Arguments.Count);

        if (GetTargetType(tupleExpression) is INamedTypeSymbol target &&
            target.TypeKind == TypeKind.Tuple &&
            target.TupleElements.Length == tupleExpression.Arguments.Count)
        {
            for (int i = 0; i < tupleExpression.Arguments.Count; i++)
            {
                var arg = tupleExpression.Arguments[i];
                var boundExpr = BindExpression(arg.Expression);
                var expected = target.TupleElements[i].Type;
                if (expected.TypeKind != TypeKind.Error &&
                    ShouldAttemptConversion(boundExpr))
                {
                    if (!IsAssignable(expected, boundExpr.Type!, out var conversion))
                    {
                        _diagnostics.ReportCannotConvertFromTypeToType(
                            boundExpr.Type!.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                            expected.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                            arg.GetLocation());
                    }
                    else
                    {
                        boundExpr = ApplyConversion(boundExpr, expected, conversion, arg.Expression);
                    }
                }

                elements.Add(boundExpr);
            }

            return new BoundTupleExpression(elements.ToImmutableArray(), target);
        }

        var tupleElements = new List<(string? name, ITypeSymbol type)>();

        foreach (var node in tupleExpression.Arguments)
        {
            if (node is ArgumentSyntax arg)
            {
                var boundExpr = BindExpression(arg.Expression);
                elements.Add(boundExpr);

                var type = boundExpr.Type ?? Compilation.ErrorTypeSymbol;
                string? name = arg.NameColon?.Name.ToString();
                tupleElements.Add((name, type));
            }
        }

        var tupleType = Compilation.CreateTupleTypeSymbol(tupleElements);

        return new BoundTupleExpression(
            elements.ToImmutableArray(),
            tupleType
        );
    }

    private BoundExpression BindUnaryExpression(UnaryExpressionSyntax unaryExpression)
    {
        var operand = BindExpression(unaryExpression.Expression);

        return unaryExpression.Kind switch
        {
            SyntaxKind.LogicalNotExpression => operand,
            SyntaxKind.UnaryMinusExpression => operand,
            SyntaxKind.UnaryPlusExpression => operand,

            SyntaxKind.AddressOfExpression => BindAddressOfExpression(operand, unaryExpression),

            _ => throw new NotSupportedException("Unsupported unary expression")
        };
    }

    private BoundExpression BindAddressOfExpression(BoundExpression operand, UnaryExpressionSyntax syntax)
    {
        if (operand is BoundLocalAccess or BoundParameterAccess)
        {
            return new BoundAddressOfExpression(operand.Symbol!, operand.Type!);
        }

        //_diagnostics.ReportInvalidAddressOf(syntax.Expression.GetLocation());
        return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.ArgumentBindingFailed /*,.InvalidAddressOfTarget */);
    }

    private BoundExpression BindLambdaExpression(LambdaExpressionSyntax syntax)
    {
        // 1. Extract parameter syntax
        var parameterSyntaxes = syntax switch
        {
            SimpleLambdaExpressionSyntax s => new[] { s.Parameter },
            ParenthesizedLambdaExpressionSyntax p => p.ParameterList.Parameters.ToArray(),
            _ => throw new NotSupportedException("Unknown lambda syntax")
        };

        var targetType = GetTargetType(syntax);
        INamedTypeSymbol? targetDelegate = targetType as INamedTypeSymbol;
        if (targetDelegate?.TypeKind != TypeKind.Delegate)
            targetDelegate = null;
        var targetSignature = targetDelegate?.GetDelegateInvokeMethod();

        // 2. Create parameter symbols
        var parameterSymbols = new List<IParameterSymbol>();
        for (int index = 0; index < parameterSyntaxes.Length; index++)
        {
            var parameterSyntax = parameterSyntaxes[index];
            var annotation = parameterSyntax.TypeAnnotation;
            var typeSyntax = annotation?.Type;
            var refKind = RefKind.None;

            if (typeSyntax is ByRefTypeSyntax byRefSyntax)
            {
                refKind = parameterSyntax.Modifiers.Any(m => m.Kind == SyntaxKind.OutKeyword) ? RefKind.Out : RefKind.Ref;
                typeSyntax = byRefSyntax.ElementType;
            }

            var targetParam = targetSignature is { } invoke && invoke.Parameters.Length > index
                ? invoke.Parameters[index]
                : null;

            ITypeSymbol parameterType;
            if (typeSyntax is not null)
            {
                parameterType = ResolveType(typeSyntax);
            }
            else if (targetParam is not null)
            {
                parameterType = targetParam.Type;
                if (refKind == RefKind.None)
                    refKind = targetParam.RefKind;
            }
            else
            {
                parameterType = Compilation.ErrorTypeSymbol;
                _diagnostics.ReportLambdaParameterTypeCannotBeInferred(
                    parameterSyntax.Identifier.Text,
                    parameterSyntax.Identifier.GetLocation());
            }

            var symbol = new SourceParameterSymbol(
                parameterSyntax.Identifier.Text,
                parameterType,
                _containingSymbol,
                _containingSymbol.ContainingType as INamedTypeSymbol,
                _containingSymbol.ContainingNamespace,
                [parameterSyntax.GetLocation()],
                [parameterSyntax.GetReference()],
                refKind
            );

            parameterSymbols.Add(symbol);
        }

        // 3. Resolve explicit return type (if any)
        TypeSyntax? returnTypeSyntax = syntax switch
        {
            SimpleLambdaExpressionSyntax s => s.ReturnType.Type,
            ParenthesizedLambdaExpressionSyntax p => p.ReturnType.Type,
            _ => null
        };

        // 4. Create symbol for the lambda
        var inferredReturnType = returnTypeSyntax is not null
            ? ResolveType(returnTypeSyntax)
            : Compilation.ErrorTypeSymbol; // Temporary fallback; real type inferred below

        var lambdaSymbol = new SourceLambdaSymbol(
            parameterSymbols,
            inferredReturnType,
            _containingSymbol,
            _containingSymbol.ContainingType as INamedTypeSymbol,
            _containingSymbol.ContainingNamespace,
            [syntax.GetLocation()],
            [syntax.GetReference()]);

        // 5. Bind the body using a new binder scope
        var lambdaBinder = new LambdaBinder(lambdaSymbol, this);

        foreach (var param in parameterSymbols)
            lambdaBinder.DeclareParameter(param);

        var bodyExpr = lambdaBinder.BindExpression(syntax.ExpressionBody, allowReturn: true);

        var inferred = bodyExpr.Type ?? ReturnTypeCollector.Infer(bodyExpr);

        ITypeSymbol returnType;
        if (returnTypeSyntax is not null)
        {
            returnType = inferredReturnType;
            if (inferred is not null &&
                inferred.TypeKind != TypeKind.Error &&
                returnType.TypeKind != TypeKind.Error)
            {
                if (!IsAssignable(returnType, inferred, out var conversion))
                {
                    _diagnostics.ReportCannotConvertFromTypeToType(
                        inferred.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        returnType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        syntax.ExpressionBody.GetLocation());
                }
                else
                {
                    bodyExpr = ApplyConversion(bodyExpr, returnType, conversion, syntax.ExpressionBody);
                }
            }
        }
        else if (targetSignature is { ReturnType: { } targetReturn } && targetReturn.TypeKind != TypeKind.Error)
        {
            returnType = targetReturn;
            if (inferred is not null &&
                inferred.TypeKind != TypeKind.Error &&
                returnType.TypeKind != TypeKind.Error)
            {
                if (!IsAssignable(returnType, inferred, out var conversion))
                {
                    _diagnostics.ReportCannotConvertFromTypeToType(
                        inferred.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        returnType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        syntax.ExpressionBody.GetLocation());
                }
                else
                {
                    bodyExpr = ApplyConversion(bodyExpr, returnType, conversion, syntax.ExpressionBody);
                }
            }
        }
        else
        {
            returnType = inferred ?? Compilation.ErrorTypeSymbol;
        }

        lambdaBinder.SetLambdaBody(bodyExpr);

        var capturedVariables = lambdaBinder.AnalyzeCapturedVariables();

        if (lambdaSymbol is SourceLambdaSymbol sourceLambdaSymbol)
            sourceLambdaSymbol.SetCapturedVariables(capturedVariables);

        // 7. Construct delegate type (e.g., Func<...> or custom delegate)
        var delegateType = targetDelegate is not null &&
            targetSignature is not null &&
            targetSignature.Parameters.Length == parameterSymbols.Count &&
            SymbolEqualityComparer.Default.Equals(returnType, targetSignature.ReturnType) &&
            parameterSymbols
                .Zip(targetSignature.Parameters, (parameter, target) =>
                    SymbolEqualityComparer.Default.Equals(parameter.Type, target.Type) && parameter.RefKind == target.RefKind)
                .All(match => match)
            ? targetDelegate
            : Compilation.CreateFunctionTypeSymbol(
                parameterSymbols.Select(p => p.Type).ToArray(),
                returnType
            );

        // 7. Update lambda symbol if needed
        if (lambdaSymbol is SourceLambdaSymbol mutable)
        {
            mutable.SetReturnType(returnType);
            mutable.SetDelegateType(delegateType);
        }

        // 8. Return a fully bound lambda expression
        return new BoundLambdaExpression(parameterSymbols, returnType, bodyExpr, lambdaSymbol, delegateType, capturedVariables);
    }

    private BoundExpression BindMissingExpression(ExpressionSyntax.Missing missing)
    {
        return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
    }

    private BoundExpression BindParenthesizedExpression(ParenthesizedExpressionSyntax parenthesizedExpression)
    {
        var expression = BindExpression(parenthesizedExpression.Expression);

        if (expression is BoundErrorExpression)
            return expression;

        return new BoundParenthesizedExpression(expression);
    }

    private BoundExpression BindCastExpression(CastExpressionSyntax castExpression)
    {
        var expression = BindExpression(castExpression.Expression);
        var targetType = ResolveType(castExpression.Type);

        if (expression is BoundErrorExpression)
            return expression;

        var conversion = Compilation.ClassifyConversion(expression.Type!, targetType);
        if (!conversion.Exists)
        {
            _diagnostics.ReportCannotConvertFromTypeToType(
                expression.Type!.ToDisplayStringForDiagnostics(SymbolDisplayFormat.MinimallyQualifiedFormat),
                targetType.ToDisplayStringForDiagnostics(SymbolDisplayFormat.MinimallyQualifiedFormat),
                castExpression.GetLocation());
            return new BoundErrorExpression(targetType, null, BoundExpressionReason.TypeMismatch);
        }

        return new BoundCastExpression(expression, targetType, conversion);
    }

    private BoundExpression ConvertMethodGroupToDelegate(BoundMethodGroupExpression methodGroup, ITypeSymbol targetType, SyntaxNode? syntax)
    {
        if (targetType is not INamedTypeSymbol delegateType || delegateType.TypeKind != TypeKind.Delegate)
            return methodGroup;

        var invoke = delegateType.GetDelegateInvokeMethod();
        if (invoke is null)
        {
            var fallbackGroup = new BoundMethodGroupExpression(
                methodGroup.Receiver,
                methodGroup.Methods,
                methodGroup.MethodGroupType,
                delegateTypeFactory: () => targetType,
                selectedMethod: methodGroup.SelectedMethod,
                reason: methodGroup.Reason != BoundExpressionReason.None ? methodGroup.Reason : BoundExpressionReason.OverloadResolutionFailed);

            return new BoundDelegateCreationExpression(fallbackGroup, delegateType);
        }

        var compatibleMethods = methodGroup.Methods
            .Where(candidate => IsCompatibleWithDelegate(candidate, invoke, methodGroup.Receiver is not null))
            .ToImmutableArray();

        var selectedMethod = methodGroup.SelectedMethod;
        var computedReason = BoundExpressionReason.None;

        if (compatibleMethods.Length == 1)
        {
            selectedMethod = compatibleMethods[0];
        }
        else if (compatibleMethods.IsDefaultOrEmpty)
        {
            selectedMethod = null;
            computedReason = BoundExpressionReason.OverloadResolutionFailed;
        }
        else
        {
            selectedMethod = null;
            computedReason = BoundExpressionReason.Ambiguous;
        }

        var reason = methodGroup.Reason != BoundExpressionReason.None ? methodGroup.Reason : computedReason;
        var reportReason = methodGroup.Reason == BoundExpressionReason.None ? computedReason : BoundExpressionReason.None;

        var resolvedGroup = new BoundMethodGroupExpression(
            methodGroup.Receiver,
            methodGroup.Methods,
            methodGroup.MethodGroupType,
            delegateTypeFactory: () => targetType,
            selectedMethod: selectedMethod,
            reason: reason);

        if (syntax is not null)
        {
            if (reportReason is BoundExpressionReason.Ambiguous)
            {
                _diagnostics.ReportMethodGroupConversionIsAmbiguous(GetMethodGroupDisplay(methodGroup), syntax.GetLocation());
            }
            else if (reportReason is BoundExpressionReason.OverloadResolutionFailed)
            {
                _diagnostics.ReportNoOverloadMatchesDelegate(
                    GetMethodGroupDisplay(methodGroup),
                    delegateType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    syntax.GetLocation());
            }
        }

        return new BoundDelegateCreationExpression(resolvedGroup, delegateType);
    }

    private bool IsCompatibleWithDelegate(IMethodSymbol method, IMethodSymbol invoke, bool hasReceiver)
    {
        if (!hasReceiver && !method.IsStatic)
            return false;

        if (method.IsGenericMethod)
            return false;

        if (method.Parameters.Length != invoke.Parameters.Length)
            return false;

        for (var i = 0; i < method.Parameters.Length; i++)
        {
            var methodParameter = method.Parameters[i];
            var delegateParameter = invoke.Parameters[i];

            if (methodParameter.RefKind != delegateParameter.RefKind)
                return false;

            var conversion = Compilation.ClassifyConversion(delegateParameter.Type, methodParameter.Type);
            if (!conversion.Exists || !conversion.IsImplicit)
                return false;
        }

        var returnConversion = Compilation.ClassifyConversion(method.ReturnType, invoke.ReturnType);
        return returnConversion.Exists && returnConversion.IsImplicit;
    }

    private BoundExpression BindAsExpression(AsExpressionSyntax asExpression)
    {
        var expression = BindExpression(asExpression.Expression);
        var targetType = ResolveType(asExpression.Type);

        if (expression is BoundErrorExpression)
            return expression;

        if (expression.Type!.IsValueType || targetType.IsValueType)
        {
            _diagnostics.ReportCannotConvertFromTypeToType(
                expression.Type!.ToDisplayStringForDiagnostics(SymbolDisplayFormat.MinimallyQualifiedFormat),
                targetType.ToDisplayStringForDiagnostics(SymbolDisplayFormat.MinimallyQualifiedFormat),
                asExpression.GetLocation());
            var errorType = new NullableTypeSymbol(targetType, null, null, null, []);
            return new BoundErrorExpression(errorType, null, BoundExpressionReason.TypeMismatch);
        }

        var conversion = Compilation.ClassifyConversion(expression.Type!, targetType);
        if (!conversion.Exists || conversion.IsNumeric || conversion.IsUserDefined)
        {
            _diagnostics.ReportCannotConvertFromTypeToType(
                expression.Type!.ToDisplayStringForDiagnostics(SymbolDisplayFormat.MinimallyQualifiedFormat),
                targetType.ToDisplayStringForDiagnostics(SymbolDisplayFormat.MinimallyQualifiedFormat),
                asExpression.GetLocation());
            var errorType = new NullableTypeSymbol(targetType, null, null, null, []);
            return new BoundErrorExpression(errorType, null, BoundExpressionReason.TypeMismatch);
        }

        var resultType = new NullableTypeSymbol(targetType, null, null, null, []);
        return new BoundAsExpression(expression, resultType, conversion);
    }

    private BoundExpression BindMatchExpression(MatchExpressionSyntax matchExpression)
    {
        var scrutinee = BindExpression(matchExpression.Expression);

        var armBuilder = ImmutableArray.CreateBuilder<BoundMatchArm>();

        foreach (var arm in matchExpression.Arms)
        {
            _scopeDepth++;
            var depth = _scopeDepth;

            var pattern = BindPattern(arm.Pattern);

            BoundExpression? guard = null;
            if (arm.WhenClause is { } whenClause)
                guard = BindExpression(whenClause.Condition);

            var expression = BindExpression(arm.Expression, allowReturn: _allowReturnsInExpression);

            foreach (var name in _locals.Where(kvp => kvp.Value.Depth == depth).Select(kvp => kvp.Key).ToList())
                _locals.Remove(name);

            _scopeDepth--;

            armBuilder.Add(new BoundMatchArm(pattern, guard, expression));
        }

        var arms = armBuilder.ToImmutable();

        EnsureMatchArmPatternsValid(scrutinee, matchExpression, arms);
        EnsureMatchArmOrder(matchExpression, scrutinee, arms);
        EnsureMatchExhaustive(matchExpression, scrutinee, arms);

        var resultType = TypeSymbolNormalization.NormalizeUnion(
            arms.Select(arm => arm.Expression.Type ?? Compilation.ErrorTypeSymbol));

        return new BoundMatchExpression(scrutinee, arms, resultType);
    }

    private void EnsureMatchArmPatternsValid(
        BoundExpression scrutinee,
        MatchExpressionSyntax matchExpression,
        ImmutableArray<BoundMatchArm> arms)
    {
        if (scrutinee.Type is not ITypeSymbol scrutineeType)
            return;

        scrutineeType = UnwrapAlias(scrutineeType);

        if (scrutineeType.TypeKind == TypeKind.Error)
            return;

        for (var i = 0; i < arms.Length; i++)
        {
            var arm = arms[i];
            var patternSyntax = matchExpression.Arms[i].Pattern;
            EnsureMatchArmPatternValid(scrutineeType, patternSyntax, arm.Pattern);
        }
    }

    private void EnsureMatchArmPatternValid(
        ITypeSymbol scrutineeType,
        PatternSyntax patternSyntax,
        BoundPattern pattern)
    {
        switch (pattern)
        {
            case BoundDiscardPattern:
                return;
            case BoundDeclarationPattern declaration:
                {
                    var patternType = UnwrapAlias(declaration.DeclaredType);

                    if (patternType.TypeKind == TypeKind.Error)
                        return;

                    if (PatternCanMatch(scrutineeType, patternType))
                        return;

                    var patternDisplay = GetMatchPatternDisplay(patternType);
                    var location = patternSyntax switch
                    {
                        DeclarationPatternSyntax declarationSyntax => declarationSyntax.Type.GetLocation(),
                        _ => patternSyntax.GetLocation(),
                    };

                    _diagnostics.ReportMatchExpressionArmPatternInvalid(
                        patternDisplay,
                        scrutineeType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        location);
                    return;
                }
            case BoundConstantPattern constant:
                {
                    var underlyingType = UnwrapAlias(constant.LiteralType.UnderlyingType);

                    if (underlyingType.TypeKind == TypeKind.Error)
                        return;

                    if (PatternCanMatch(scrutineeType, underlyingType))
                        return;

                    var patternDisplay = GetMatchPatternDisplay(constant.LiteralType);
                    var location = patternSyntax switch
                    {
                        DeclarationPatternSyntax declarationSyntax => declarationSyntax.Type.GetLocation(),
                        _ => patternSyntax.GetLocation(),
                    };

                    _diagnostics.ReportMatchExpressionArmPatternInvalid(
                        patternDisplay,
                        scrutineeType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        location);
                    return;
                }
            case BoundOrPattern orPattern:
                {
                    if (patternSyntax is BinaryPatternSyntax binarySyntax)
                    {
                        EnsureMatchArmPatternValid(scrutineeType, binarySyntax.Left, orPattern.Left);
                        EnsureMatchArmPatternValid(scrutineeType, binarySyntax.Right, orPattern.Right);
                    }

                    return;
                }
            case BoundAndPattern andPattern:
                {
                    if (patternSyntax is BinaryPatternSyntax binarySyntax)
                    {
                        EnsureMatchArmPatternValid(scrutineeType, binarySyntax.Left, andPattern.Left);
                        EnsureMatchArmPatternValid(scrutineeType, binarySyntax.Right, andPattern.Right);
                    }

                    return;
                }
            case BoundNotPattern notPattern:
                {
                    if (patternSyntax is UnaryPatternSyntax unarySyntax)
                        EnsureMatchArmPatternValid(scrutineeType, unarySyntax.Pattern, notPattern.Pattern);

                    return;
                }
        }
    }

    private static string GetMatchPatternDisplay(ITypeSymbol patternType)
        => patternType switch
        {
            LiteralTypeSymbol literal => literal.Name,
            _ => $"for type '{patternType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat)}'"
        };

    private bool PatternCanMatch(ITypeSymbol scrutineeType, ITypeSymbol patternType)
    {
        scrutineeType = UnwrapAlias(scrutineeType);
        patternType = UnwrapAlias(patternType);

        if (scrutineeType.TypeKind == TypeKind.Error || patternType.TypeKind == TypeKind.Error)
            return true;

        if (scrutineeType is IUnionTypeSymbol scrutineeUnion)
        {
            foreach (var member in scrutineeUnion.Types)
            {
                if (PatternCanMatch(member, patternType))
                    return true;
            }

            return false;
        }

        if (patternType is IUnionTypeSymbol patternUnion)
        {
            foreach (var member in patternUnion.Types)
            {
                if (PatternCanMatch(scrutineeType, member))
                    return true;
            }

            return false;
        }

        return IsAssignable(patternType, scrutineeType, out _) ||
               IsAssignable(scrutineeType, patternType, out _);
    }

    private void EnsureMatchArmOrder(
        MatchExpressionSyntax matchExpression,
        BoundExpression scrutinee,
        ImmutableArray<BoundMatchArm> arms)
    {
        if (scrutinee.Type is not ITypeSymbol scrutineeType)
            return;

        scrutineeType = UnwrapAlias(scrutineeType);

        if (scrutineeType.TypeKind == TypeKind.Error)
            return;

        var seenCatchAll = false;

        for (var i = 0; i < arms.Length; i++)
        {
            var arm = arms[i];

            if (seenCatchAll)
            {
                _diagnostics.ReportMatchExpressionArmUnreachable(
                    matchExpression.Arms[i].Pattern.GetLocation());
                continue;
            }

            if (arm.Guard is not null)
                continue;

            if (IsCatchAllPattern(scrutineeType, arm.Pattern))
                seenCatchAll = true;
        }
    }

    private void EnsureMatchExhaustive(
        MatchExpressionSyntax matchExpression,
        BoundExpression scrutinee,
        ImmutableArray<BoundMatchArm> arms)
    {
        if (scrutinee.Type is not ITypeSymbol scrutineeType)
            return;

        scrutineeType = UnwrapAlias(scrutineeType);

        if (scrutineeType.TypeKind == TypeKind.Error)
            return;

        if (HasDefaultArm(scrutineeType, arms))
            return;

        if (scrutineeType is not IUnionTypeSymbol union)
        {
            _diagnostics.ReportMatchExpressionNotExhaustive(
                "_",
                matchExpression.GetLocation());
            return;
        }

        var remaining = new HashSet<ITypeSymbol>(
            GetUnionMembers(union),
            SymbolEqualityComparer.Default);

        foreach (var arm in arms)
        {
            RemoveCoveredUnionMembers(remaining, arm.Pattern);

            if (remaining.Count == 0)
                return;
        }

        if (remaining.Count == 0)
            return;

        foreach (var missing in remaining
            .Select(member => member.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat))
            .OrderBy(name => name, StringComparer.Ordinal))
        {
            _diagnostics.ReportMatchExpressionNotExhaustive(
                missing,
                matchExpression.GetLocation());
        }
    }

    private bool HasDefaultArm(ITypeSymbol scrutineeType, ImmutableArray<BoundMatchArm> arms)
    {
        foreach (var arm in arms)
        {
            if (arm.Guard is not null)
                continue;

            if (IsCatchAllPattern(scrutineeType, arm.Pattern))
                return true;
        }

        return false;
    }

    private bool IsCatchAllPattern(ITypeSymbol scrutineeType, BoundPattern pattern)
    {
        switch (pattern)
        {
            case BoundDiscardPattern:
                return true;
            case BoundDeclarationPattern { Designator: BoundDiscardDesignator } declaration:
                {
                    var declaredType = UnwrapAlias(declaration.DeclaredType);

                    if (SymbolEqualityComparer.Default.Equals(declaredType, scrutineeType))
                        return true;

                    return declaredType.SpecialType == SpecialType.System_Object;
                }
            case BoundOrPattern orPattern:
                return IsCatchAllPattern(scrutineeType, orPattern.Left) ||
                       IsCatchAllPattern(scrutineeType, orPattern.Right);
        }

        return false;
    }

    private void RemoveCoveredUnionMembers(HashSet<ITypeSymbol> remaining, BoundPattern pattern)
    {
        switch (pattern)
        {
            case BoundDiscardPattern:
                remaining.Clear();
                break;
            case BoundDeclarationPattern declaration:
                RemoveMembersAssignableToPattern(remaining, declaration.DeclaredType);
                break;
            case BoundConstantPattern constant:
                {
                    var literalType = UnwrapAlias(constant.LiteralType);

                    foreach (var candidate in remaining.ToArray())
                    {
                        var candidateType = UnwrapAlias(candidate);

                        if (SymbolEqualityComparer.Default.Equals(candidateType, literalType))
                            remaining.Remove(candidate);
                    }

                    break;
                }
            case BoundOrPattern orPattern:
                RemoveCoveredUnionMembers(remaining, orPattern.Left);
                RemoveCoveredUnionMembers(remaining, orPattern.Right);
                break;
        }
    }

    private void RemoveMembersAssignableToPattern(HashSet<ITypeSymbol> remaining, ITypeSymbol patternType)
    {
        patternType = UnwrapAlias(patternType);

        if (patternType.TypeKind == TypeKind.Error)
            return;

        foreach (var candidate in remaining.ToArray())
        {
            var candidateType = UnwrapAlias(candidate);

            if (candidateType.TypeKind == TypeKind.Error)
            {
                remaining.Remove(candidate);
                continue;
            }

            if (IsAssignable(patternType, candidateType, out _))
                remaining.Remove(candidate);
        }
    }

    private static IEnumerable<ITypeSymbol> GetUnionMembers(IUnionTypeSymbol union)
    {
        foreach (var member in union.Types)
        {
            if (member is IUnionTypeSymbol nested)
            {
                foreach (var nestedMember in GetUnionMembers(nested))
                    yield return UnwrapAlias(nestedMember);
            }
            else
            {
                yield return UnwrapAlias(member);
            }
        }
    }

    private static ITypeSymbol UnwrapAlias(ITypeSymbol type)
    {
        while (type.IsAlias && type.UnderlyingSymbol is ITypeSymbol alias)
            type = alias;

        return type;
    }

    private BoundExpression BindIfExpression(IfExpressionSyntax ifExpression)
    {
        var condition = BindExpression(ifExpression.Condition);

        var thenBinder = SemanticModel.GetBinder(ifExpression, this);
        var thenExpr = thenBinder is BlockBinder bb
            ? bb.BindExpression(ifExpression.Expression, _allowReturnsInExpression)
            : thenBinder.BindExpression(ifExpression.Expression);

        if (ifExpression.ElseClause is null)
        {
            _diagnostics.ReportIfExpressionRequiresElse(ifExpression.GetLocation());
            return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.OtherError);
        }

        var elseBinder = SemanticModel.GetBinder(ifExpression.ElseClause, this);
        var elseExpr = elseBinder is BlockBinder ebb
            ? ebb.BindExpression(ifExpression.ElseClause.Expression, _allowReturnsInExpression)
            : elseBinder.BindExpression(ifExpression.ElseClause.Expression);

        return new BoundIfExpression(condition, thenExpr, elseExpr);
    }

    private BoundExpression BindWhileExpression(WhileExpressionSyntax whileExpression)
    {
        var condition = BindExpression(whileExpression.Condition);

        var expressionBinder = SemanticModel.GetBinder(whileExpression, this);
        var expression = expressionBinder is BlockBinder wb
            ? wb.BindExpression(whileExpression.Expression, _allowReturnsInExpression) as BoundExpression
            : expressionBinder.BindExpression(whileExpression.Expression) as BoundExpression;

        return new BoundWhileExpression(condition, expression!);
    }

    private BoundExpression BindForExpression(ForExpressionSyntax forExpression)
    {
        var loopBinder = (BlockBinder)SemanticModel.GetBinder(forExpression, this)!;

        var collection = BindExpression(forExpression.Expression);

        ITypeSymbol elementType = collection.Type is IArrayTypeSymbol array
            ? array.ElementType
            : Compilation.GetSpecialType(SpecialType.System_Object);

        var local = loopBinder.CreateLocalSymbol(forExpression, forExpression.Identifier.Text, isMutable: false, elementType);

        var body = loopBinder.BindExpression(forExpression.Body, _allowReturnsInExpression) as BoundExpression;

        return new BoundForExpression(local, collection, body!);
    }

    private BoundExpression BindMemberAccessExpression(MemberAccessExpressionSyntax memberAccess)
    {
        // Binding for explicit receiver
        var receiver = BindExpression(memberAccess.Expression);

        if (receiver is BoundErrorExpression)
            return receiver;

        var simpleName = memberAccess.Name;
        if (simpleName.Identifier.IsMissing)
        {
            _diagnostics.ReportIdentifierExpected(simpleName.Identifier.GetLocation());
            _diagnostics.ReportTheNameDoesNotExistInTheCurrentContext(string.Empty, simpleName.GetLocation());
            return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
        }

        var name = simpleName.Identifier.Text;
        ImmutableArray<ITypeSymbol>? explicitTypeArguments = null;
        GenericNameSyntax? genericTypeSyntax = null;

        if (simpleName is GenericNameSyntax genericName)
        {
            var boundTypeArguments = TryBindTypeArguments(genericName);
            if (boundTypeArguments is null)
                return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.TypeMismatch);

            explicitTypeArguments = boundTypeArguments;
            genericTypeSyntax = genericName;
        }

        var nameLocation = simpleName.GetLocation();

        if (receiver is BoundNamespaceExpression nsExpr)
        {
            var member = nsExpr.Namespace.GetMembers(name).FirstOrDefault();

            if (member is INamespaceSymbol ns2)
                return new BoundNamespaceExpression(ns2);

            if (member is ITypeSymbol type)
                return new BoundTypeExpression(type);

            _diagnostics.ReportTypeOrNamespaceNameDoesNotExistInTheNamespace(name, nsExpr.Namespace.Name, memberAccess.Name.GetLocation());
            return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
        }

        if (receiver is BoundTypeExpression typeExpr)
        {
            var methodCandidates = new SymbolQuery(name, typeExpr.Type, IsStatic: true)
                .LookupMethods(this)
                .ToImmutableArray();

            if (!methodCandidates.IsDefaultOrEmpty)
            {
                if (explicitTypeArguments is { } typeArgs && genericTypeSyntax is not null)
                {
                    var instantiated = InstantiateMethodCandidates(methodCandidates, typeArgs, genericTypeSyntax, memberAccess.Name.GetLocation());
                    if (!instantiated.IsDefaultOrEmpty)
                        return BindMethodGroup(typeExpr, instantiated, nameLocation);
                }
                else
                {
                    return BindMethodGroup(typeExpr, methodCandidates, nameLocation);
                }
            }

            var member = new SymbolQuery(name, typeExpr.Type, IsStatic: true)
                .Lookup(this)
                .FirstOrDefault();

            if (member is null)
            {
                var typeName = typeExpr.Symbol!.Name;
                _diagnostics.ReportMemberDoesNotContainDefinition(typeName, memberAccess.Name.ToString(), memberAccess.Name.GetLocation());
                return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
            }

            if (!EnsureMemberAccessible(member, memberAccess.Name.GetLocation(), GetSymbolKindForDiagnostic(member)))
                return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.Inaccessible);

            return new BoundMemberAccessExpression(typeExpr, member);
        }

        if (receiver.Type is not null)
        {
            var instanceMethods = new SymbolQuery(name, receiver.Type, IsStatic: false)
                .LookupMethods(this)
                .ToImmutableArray();

            if (!instanceMethods.IsDefaultOrEmpty)
            {
                if (explicitTypeArguments is { } typeArgs && genericTypeSyntax is not null)
                {
                    var instantiated = InstantiateMethodCandidates(instanceMethods, typeArgs, genericTypeSyntax, memberAccess.Name.GetLocation());
                    if (!instantiated.IsDefaultOrEmpty)
                        return BindMethodGroup(receiver, instantiated, nameLocation);
                }
                else
                {
                    return BindMethodGroup(receiver, instanceMethods, nameLocation);
                }
            }

            var instanceMember = new SymbolQuery(name, receiver.Type, IsStatic: false)
                .Lookup(this)
                .FirstOrDefault();

            if (instanceMember is not null)
            {
                if (!EnsureMemberAccessible(instanceMember, nameLocation, GetSymbolKindForDiagnostic(instanceMember)))
                    return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.Inaccessible);

                return new BoundMemberAccessExpression(receiver, instanceMember);
            }
        }

        _diagnostics.ReportTheNameDoesNotExistInTheCurrentContext(name, nameLocation);
        return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
    }

    private BoundExpression BindMemberBindingExpression(MemberBindingExpressionSyntax memberBinding)
    {
        var simpleName = memberBinding.Name;
        var memberName = simpleName.Identifier.Text;
        ImmutableArray<ITypeSymbol>? explicitTypeArguments = null;
        GenericNameSyntax? genericTypeSyntax = null;

        if (simpleName is GenericNameSyntax genericName)
        {
            var typeArgs = TryBindTypeArguments(genericName);
            if (typeArgs is null)
                return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.TypeMismatch);

            explicitTypeArguments = typeArgs;
            genericTypeSyntax = genericName;
        }

        var nameLocation = simpleName.GetLocation();

        var expectedType = GetTargetType(memberBinding);

        if (expectedType is not null)
        {
            var methodCandidates = new SymbolQuery(memberName, expectedType, IsStatic: true)
                .LookupMethods(this)
                .ToImmutableArray();

            if (!methodCandidates.IsDefaultOrEmpty)
            {
                if (explicitTypeArguments is { } typeArgs && genericTypeSyntax is not null)
                {
                    var instantiated = InstantiateMethodCandidates(methodCandidates, typeArgs, genericTypeSyntax, simpleName.GetLocation());
                    if (!instantiated.IsDefaultOrEmpty)
                        return BindMethodGroup(new BoundTypeExpression(expectedType), instantiated, nameLocation);
                }
                else
                {
                    return BindMethodGroup(new BoundTypeExpression(expectedType), methodCandidates, nameLocation);
                }
            }

            var member = new SymbolQuery(memberName, expectedType, IsStatic: true)
                .Lookup(this)
                .FirstOrDefault();

            if (member is null)
            {
                _diagnostics.ReportTheNameDoesNotExistInTheCurrentContext(memberName, nameLocation);
                return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
            }

            if (!EnsureMemberAccessible(member, nameLocation, GetSymbolKindForDiagnostic(member)))
                return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.Inaccessible);

            return new BoundMemberAccessExpression(new BoundTypeExpression(expectedType), member);
        }

        _diagnostics.ReportMemberAccessRequiresTargetType(memberName, nameLocation);
        return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
    }

    private ITypeSymbol? GetTargetType(SyntaxNode node)
    {
        var current = node.Parent;

        while (current != null)
        {
            switch (current)
            {
                case VariableDeclaratorSyntax vds:
                    if (vds.TypeAnnotation != null)
                        return ResolveType(vds.TypeAnnotation.Type);
                    break;

                case AssignmentExpressionSyntax assign when assign.Right == node:
                    var left = BindExpression(assign.Left);
                    return left.Type;

                case AssignmentStatementSyntax assign when assign.Right.Contains(node):
                    var leftStmt = BindExpression(assign.Left);
                    return leftStmt.Type;

                case ReturnStatementSyntax returnStmt:
                    return _containingSymbol is IMethodSymbol method ? method.ReturnType : null;

                case BinaryExpressionSyntax binary when binary.Left == node:
                    return BindExpression(binary.Right).Type;

                case BinaryExpressionSyntax binary when binary.Right == node:
                    return BindExpression(binary.Left).Type;

                case ArgumentSyntax arg:
                    {
                        if (arg.Parent is ArgumentListSyntax argList &&
                            argList.Parent is InvocationExpressionSyntax invocation)
                        {
                            var index = 0;
                            foreach (var a in argList.Arguments)
                            {
                                if (a == arg)
                                    break;
                                index++;
                            }

                            IMethodSymbol? targetMethod = null;
                            if (invocation.Expression is MemberAccessExpressionSyntax memberAccess)
                            {
                                var boundMember = BindMemberAccessExpression(memberAccess);
                                if (boundMember is BoundMethodGroupExpression methodGroup)
                                {
                                    if (methodGroup.Methods.Length == 1)
                                        targetMethod = methodGroup.Methods[0];
                                    else if (TryGetCommonParameterType(methodGroup.Methods, index) is ITypeSymbol common)
                                        return common;
                                }
                                else if (boundMember is BoundMemberAccessExpression { Member: IMethodSymbol m })
                                {
                                    targetMethod = m;
                                }
                            }
                            else if (invocation.Expression is MemberBindingExpressionSyntax memberBinding)
                            {
                                var boundMember = BindMemberBindingExpression(memberBinding);
                                if (boundMember is BoundMethodGroupExpression methodGroup)
                                {
                                    if (methodGroup.Methods.Length == 1)
                                        targetMethod = methodGroup.Methods[0];
                                    else if (TryGetCommonParameterType(methodGroup.Methods, index) is ITypeSymbol common)
                                        return common;
                                }
                                else if (boundMember is BoundMemberAccessExpression { Member: IMethodSymbol m })
                                {
                                    targetMethod = m;
                                }
                            }
                            else if (invocation.Expression is IdentifierNameSyntax id)
                            {
                                var candidates = new SymbolQuery(id.Identifier.Text)
                                    .LookupMethods(this)
                                    .ToImmutableArray();
                                var accessible = GetAccessibleMethods(candidates, id.Identifier.GetLocation(), reportIfInaccessible: false);
                                if (accessible.Length == 1)
                                {
                                    targetMethod = accessible[0];
                                }
                                else if (!accessible.IsDefaultOrEmpty)
                                {
                                    if (TryGetCommonParameterType(accessible, index) is ITypeSymbol common)
                                        return common;
                                }
                            }

                            if (targetMethod is not null && targetMethod.Parameters.Length > index)
                                return targetMethod.Parameters[index].Type;
                        }

                        return null;
                    }

                case ExpressionStatementSyntax:
                    return null;

                default:
                    current = current.Parent;
                    continue;
            }

            break;
        }

        return null;
    }

    private BoundExpression BindTypeSyntax(TypeSyntax syntax)
    {
        if (syntax is LiteralTypeSyntax literalType)
        {
            var token = literalType.Token;
            var value = token.Value ?? token.Text!;
            ITypeSymbol underlying = value switch
            {
                int => Compilation.GetSpecialType(SpecialType.System_Int32),
                long => Compilation.GetSpecialType(SpecialType.System_Int64),
                float => Compilation.GetSpecialType(SpecialType.System_Single),
                double => Compilation.GetSpecialType(SpecialType.System_Double),
                bool => Compilation.GetSpecialType(SpecialType.System_Boolean),
                char => Compilation.GetSpecialType(SpecialType.System_Char),
                string => Compilation.GetSpecialType(SpecialType.System_String),
                _ => Compilation.ErrorTypeSymbol
            };

            var litSymbol = new LiteralTypeSymbol(underlying, value, Compilation);
            return new BoundTypeExpression(litSymbol);
        }

        if (syntax is PredefinedTypeSyntax predefinedType)
        {
            var type = Compilation.ResolvePredefinedType(predefinedType);
            return new BoundTypeExpression(type);
        }

        if (syntax is UnitTypeSyntax)
        {
            var type = Compilation.GetSpecialType(SpecialType.System_Unit);
            return new BoundTypeExpression(type);
        }

        if (syntax is TupleTypeSyntax tupleTypeSyntax)
        {
            var boundElements = new List<(string? name, ITypeSymbol type)>();

            foreach (var element in tupleTypeSyntax.Elements)
            {
                if (BindTypeSyntax(element.Type) is not BoundTypeExpression bt)
                    return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.TypeMismatch);

                boundElements.Add((element.NameColon?.Name.ToString(), bt.Type));
            }

            var tupleType = Compilation.CreateTupleTypeSymbol(boundElements);

            return new BoundTypeExpression(tupleType);
        }

        if (syntax is IdentifierNameSyntax id)
        {
            return BindTypeName(id.Identifier.Text, id.GetLocation(), []);
        }

        if (syntax is GenericNameSyntax generic)
        {
            var typeArgs = generic.TypeArgumentList.Arguments
                .Select(arg => BindTypeSyntax(arg.Type))
                .OfType<BoundTypeExpression>()
                .Select(b => b.Type)
                .ToImmutableArray();

            if (typeArgs.Length != generic.TypeArgumentList.Arguments.Count)
                return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.TypeMismatch);

            return BindTypeName(generic.Identifier.Text, generic.GetLocation(), typeArgs, generic.TypeArgumentList.Arguments);
        }

        if (syntax is QualifiedNameSyntax qualified)
        {
            var left = BindTypeSyntax(qualified.Left);

            if (left is not BoundNamespaceExpression ns && left is not BoundTypeExpression leftType)
                return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);

            string name;
            ImmutableArray<ITypeSymbol> typeArgs = [];

            GenericNameSyntax? rightGeneric = null;

            if (qualified.Right is IdentifierNameSyntax id2)
            {
                name = id2.Identifier.Text;
            }
            else if (qualified.Right is GenericNameSyntax generic2)
            {
                name = generic2.Identifier.Text;
                rightGeneric = generic2;
                typeArgs = generic2.TypeArgumentList.Arguments
                    .Select(arg => BindTypeSyntax(arg.Type))
                    .OfType<BoundTypeExpression>()
                    .Select(b => b.Type)
                    .ToImmutableArray();

                if (typeArgs.Length != generic2.TypeArgumentList.Arguments.Count)
                    return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.TypeMismatch);

                // Why does this fall down to:
                // ISymbol? member = left switch
                // The type is resolved, not?
            }
            else
            {
                return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
            }

            ISymbol? member = left switch
            {
                BoundNamespaceExpression nsExpr => nsExpr.Namespace.GetMembers(name)
                    .FirstOrDefault(s => s is INamespaceSymbol || s is INamedTypeSymbol),
                BoundTypeExpression typeExpr => typeExpr.Type.GetMembers(name)
                    .OfType<INamedTypeSymbol>()
                    .FirstOrDefault(m => m.Arity == typeArgs.Length),
                _ => null
            };

            if (member is INamespaceSymbol nsResult)
                return new BoundNamespaceExpression(nsResult);

            if (member is INamedTypeSymbol namedType)
            {
                if (namedType.Arity != typeArgs.Length)
                    return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.TypeMismatch);

                if (!typeArgs.IsEmpty && rightGeneric is not null && !ValidateTypeArgumentConstraints(namedType, typeArgs, i => GetTypeArgumentLocation(rightGeneric.TypeArgumentList.Arguments, rightGeneric.GetLocation(), i), namedType.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)))
                    return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.TypeMismatch);

                var constructed = typeArgs.IsEmpty
                    ? namedType
                    : TryConstructGeneric(namedType, typeArgs, namedType.Arity) ?? namedType;

                return new BoundTypeExpression(constructed);
            }

            return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
        }

        return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
    }

    private BoundExpression BindTypeName(string name, Location location, ImmutableArray<ITypeSymbol> typeArguments, SeparatedSyntaxList<TypeArgumentSyntax> typeArgumentSyntax = default)
    {
        var symbol = LookupType(name);

        if (symbol is ITypeSymbol type && type is INamedTypeSymbol named)
        {
            // If the resolved symbol is already a constructed generic type (e.g., from an alias
            // like `alias StringList = System.Collections.Generic.List<string>`), then we don't
            // expect any additional type arguments when the alias is used. Return the constructed
            // type directly.
            if (named.ConstructedFrom is not null && !SymbolEqualityComparer.Default.Equals(named.ConstructedFrom, named))
            {
                if (!typeArguments.IsEmpty)
                {
                    //_diagnostics.ReportTypeArityMismatch(name, named.Arity, typeArguments.Length, location);
                    return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.TypeMismatch);
                }

                return new BoundTypeExpression(named);
            }

            var definition = NormalizeDefinition(named);

            if (definition.Arity != typeArguments.Length)
            {
                var match = FindAccessibleNamedType(name, typeArguments.Length);
                if (match is null)
                    return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.TypeMismatch);

                definition = match;
            }

            if (typeArguments.IsEmpty)
                return new BoundTypeExpression(definition);

            if (!ValidateTypeArgumentConstraints(definition, typeArguments, i => GetTypeArgumentLocation(typeArgumentSyntax, location, i), definition.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)))
                return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.TypeMismatch);

            var constructed = TryConstructGeneric(definition, typeArguments, definition.Arity) ?? definition;
            return new BoundTypeExpression(constructed);
        }

        if (symbol is INamespaceSymbol ns)
            return new BoundNamespaceExpression(ns);

        var alternate = FindAccessibleNamedType(name, typeArguments.Length);
        if (alternate is not null)
        {
            if (typeArguments.IsEmpty)
                return new BoundTypeExpression(alternate);

            if (!ValidateTypeArgumentConstraints(alternate, typeArguments, i => GetTypeArgumentLocation(typeArgumentSyntax, location, i), alternate.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)))
                return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.TypeMismatch);

            var constructed = TryConstructGeneric(alternate, typeArguments, alternate.Arity) ?? alternate;
            return new BoundTypeExpression(constructed);
        }

        _diagnostics.ReportTheNameDoesNotExistInTheCurrentContext(name, location);
        return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
    }

    private BoundExpression BindLiteralExpression(LiteralExpressionSyntax syntax)
    {
        if (syntax.Kind == SyntaxKind.NullLiteralExpression)
        {
            ITypeSymbol? convertedType = null;
            var targetType = GetTargetType(syntax);

            if (targetType is not null)
            {
                var conversion = Compilation.ClassifyConversion(Compilation.NullTypeSymbol, targetType);
                if (conversion.Exists)
                    convertedType = targetType;
            }

            return new BoundLiteralExpression(BoundLiteralExpressionKind.NullLiteral, null!, Compilation.NullTypeSymbol, convertedType);
        }

        var value = syntax.Token.Value ?? syntax.Token.Text!;
        var underlying = value switch
        {
            int => Compilation.GetSpecialType(SpecialType.System_Int32),
            long => Compilation.GetSpecialType(SpecialType.System_Int64),
            float => Compilation.GetSpecialType(SpecialType.System_Single),
            double => Compilation.GetSpecialType(SpecialType.System_Double),
            bool => Compilation.GetSpecialType(SpecialType.System_Boolean),
            char => Compilation.GetSpecialType(SpecialType.System_Char),
            string => Compilation.GetSpecialType(SpecialType.System_String),
            _ => throw new Exception("Unsupported literal type")
        };
        ITypeSymbol type = new LiteralTypeSymbol(underlying, value, Compilation);

        BoundLiteralExpressionKind kind = syntax.Kind switch
        {
            SyntaxKind.NumericLiteralExpression => BoundLiteralExpressionKind.NumericLiteral,
            SyntaxKind.StringLiteralExpression => BoundLiteralExpressionKind.StringLiteral,
            SyntaxKind.CharacterLiteralExpression => BoundLiteralExpressionKind.CharLiteral,
            SyntaxKind.TrueLiteralExpression => BoundLiteralExpressionKind.TrueLiteral,
            SyntaxKind.FalseLiteralExpression => BoundLiteralExpressionKind.FalseLiteral,
            SyntaxKind.NullLiteralExpression => BoundLiteralExpressionKind.NullLiteral,

            _ => throw new Exception("Unsupported literal type")
        };

        return new BoundLiteralExpression(kind, value, type);
    }

    private BoundExpression BindUnitExpression(UnitExpressionSyntax syntax)
    {
        return new BoundUnitExpression(Compilation.GetSpecialType(SpecialType.System_Unit));
    }

    private BoundExpression BindInterpolatedStringExpression(InterpolatedStringExpressionSyntax syntax)
    {
        BoundExpression? result = null;
        foreach (var content in syntax.Contents)
        {
            BoundExpression expr = content switch
            {
                InterpolatedStringTextSyntax text => new BoundLiteralExpression(
                    BoundLiteralExpressionKind.StringLiteral,
                    text.Token.Text,
                    Compilation.GetSpecialType(SpecialType.System_String)),
                InterpolationSyntax interpolation => BindExpression(interpolation.Expression),
                _ => throw new InvalidOperationException("Unknown interpolated string content")
            };

            if (result is null)
            {
                result = expr;
            }
            else
            {
                var concatMethod = ResolveStringConcatMethod(result, expr);
                result = new BoundInvocationExpression(concatMethod, [result, expr]);
            }
        }

        return result ?? new BoundLiteralExpression(
            BoundLiteralExpressionKind.StringLiteral,
            string.Empty,
            Compilation.GetSpecialType(SpecialType.System_String));
    }

    private BoundExpression BindIdentifierName(IdentifierNameSyntax syntax)
    {
        var name = syntax.Identifier.Text;
        var symbol = LookupSymbol(name);

        if (symbol is null)
        {
            _diagnostics.ReportTheNameDoesNotExistInTheCurrentContext(name, syntax.Identifier.GetLocation());
            return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
        }

        if (symbol is IMethodSymbol)
        {
            var methods = LookupSymbols(name)
                .OfType<IMethodSymbol>()
                .ToImmutableArray();

            if (methods.IsDefaultOrEmpty)
                return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);

            var receiver = BindImplicitMethodGroupReceiver(methods);

            if (receiver is BoundErrorExpression error)
                return error;

            return BindMethodGroup(receiver, methods, syntax.Identifier.GetLocation());
        }

        switch (symbol)
        {
            case INamespaceSymbol ns:
                return new BoundNamespaceExpression(ns);
            case ITypeSymbol type:
                return new BoundTypeExpression(type);
            case ILocalSymbol local:
                return new BoundLocalAccess(local);
            case IParameterSymbol param:
                return new BoundParameterAccess(param);
            case IFieldSymbol field:
                {
                    if (!EnsureMemberAccessible(field, syntax.Identifier.GetLocation(), GetSymbolKindForDiagnostic(field)))
                        return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.Inaccessible);

                    return new BoundFieldAccess(field);
                }
            case IPropertySymbol prop:
                {
                    if (!EnsureMemberAccessible(prop, syntax.Identifier.GetLocation(), GetSymbolKindForDiagnostic(prop)))
                        return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.Inaccessible);

                    return new BoundPropertyAccess(prop);
                }
            default:
                return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
        }
    }

    private BoundMethodGroupExpression CreateMethodGroup(BoundExpression? receiver, ImmutableArray<IMethodSymbol> methods)
    {
        Func<ITypeSymbol?>? delegateFactory = null;

        if (!methods.IsDefaultOrEmpty && methods.Length == 1)
        {
            var method = methods[0];
            delegateFactory = () => Compilation.GetMethodReferenceDelegate(method);
        }

        return new BoundMethodGroupExpression(receiver, methods, Compilation.ErrorTypeSymbol, delegateFactory);
    }

    private BoundExpression? BindImplicitMethodGroupReceiver(ImmutableArray<IMethodSymbol> methods)
    {
        if (!methods.Any(static method => !method.IsStatic))
            return null;

        if (_containingSymbol is IMethodSymbol method && (!method.IsStatic || method.IsNamedConstructor))
        {
            var containingType = method.ContainingType ?? Compilation.ErrorTypeSymbol;
            return new BoundSelfExpression(containingType);
        }

        return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
    }

    private BoundExpression BindBinaryExpression(BinaryExpressionSyntax syntax)
    {
        var left = BindExpression(syntax.Left);
        var right = BindExpression(syntax.Right);

        var opKind = syntax.OperatorToken.Kind;

        // 1. Specialfall: string + any → string-konkatenering
        if (opKind == SyntaxKind.PlusToken)
        {
            var leftIsString = left.Type.SpecialType == SpecialType.System_String ||
                                (left.Type is LiteralTypeSymbol lls && lls.UnderlyingType.SpecialType == SpecialType.System_String);
            var rightIsString = right.Type.SpecialType == SpecialType.System_String ||
                                 (right.Type is LiteralTypeSymbol rls && rls.UnderlyingType.SpecialType == SpecialType.System_String);

            if (left.Type is LiteralTypeSymbol litLeft &&
                right.Type is LiteralTypeSymbol litRight &&
                (leftIsString || rightIsString))
            {
                var stringType = Compilation.GetSpecialType(SpecialType.System_String);
                var value = (litLeft.ConstantValue?.ToString() ?? string.Empty) +
                            (litRight.ConstantValue?.ToString() ?? string.Empty);
                var resultType = new LiteralTypeSymbol(stringType, value, Compilation);
                return new BoundLiteralExpression(BoundLiteralExpressionKind.StringLiteral, value, resultType);
            }

            if (leftIsString || rightIsString)
            {
                var concatMethod = ResolveStringConcatMethod(left, right);
                return new BoundInvocationExpression(concatMethod, [left, right]);
            }
        }

        // 2. Överlagrade operatorer
        var userDefinedOperator = ResolveUserDefinedOperator(opKind, left.Type, right.Type);
        if (userDefinedOperator is not null)
        {
            return new BoundInvocationExpression(userDefinedOperator, [left, right]);
        }

        // 3. Inbyggda operatorer
        if (BoundBinaryOperator.TryLookup(Compilation, opKind, left.Type, right.Type, out var op))
        {
            return new BoundBinaryExpression(left, op, right);
        }

        // 4. Fel
        _diagnostics.ReportOperatorCannotBeAppliedToOperandsOfTypes(
            opKind.ToString(),
            left.Type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
            right.Type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
            syntax.OperatorToken.GetLocation());

        return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
    }

    private IMethodSymbol ResolveStringConcatMethod(BoundExpression left, BoundExpression right)
    {
        var stringType = Compilation.GetSpecialType(SpecialType.System_String);
        var candidates = stringType.GetMembers("Concat").OfType<IMethodSymbol>();

        var resolution = OverloadResolver.ResolveOverload(candidates, [left, right], Compilation);

        if (!resolution.Success)
            throw new InvalidOperationException("No matching Concat method found.");

        return resolution.Method!;
    }

    private IMethodSymbol? ResolveUserDefinedOperator(SyntaxKind opKind, ITypeSymbol leftType, ITypeSymbol rightType)
    {
        var opName = GetOperatorMethodName(opKind); // e.g. "op_Addition" for +
        if (opName is null)
            return null;

        foreach (var type in new[] { leftType, rightType })
        {
            var candidates = type.GetMembers(opName).OfType<IMethodSymbol>();

            foreach (var method in candidates)
            {
                if (!method.IsStatic || method.Parameters.Length != 2)
                    continue;

                if (Compilation.ClassifyConversion(leftType, method.Parameters[0].Type).IsImplicit &&
                    Compilation.ClassifyConversion(rightType, method.Parameters[1].Type).IsImplicit)
                {
                    return method;
                }
            }
        }

        return null;
    }

    private static string? GetOperatorMethodName(SyntaxKind kind) => kind switch
    {
        SyntaxKind.PlusToken => "op_Addition",
        SyntaxKind.MinusToken => "op_Subtraction",
        SyntaxKind.StarToken => "op_Multiply",
        SyntaxKind.SlashToken => "op_Division",
        SyntaxKind.EqualsEqualsToken => "op_Equality",
        SyntaxKind.NotEqualsExpression => "op_Inequality",
        _ => null
    };

    private BoundExpression BindInvocationExpression(InvocationExpressionSyntax syntax)
    {
        BoundExpression? receiver;
        string methodName;

        if (syntax.Expression is MemberAccessExpressionSyntax memberAccess)
        {
            var boundMember = BindMemberAccessExpression(memberAccess);

            if (boundMember is BoundErrorExpression)
                return boundMember;

            if (boundMember is BoundMethodGroupExpression methodGroup)
                return BindInvocationOnMethodGroup(methodGroup, syntax);

            if (boundMember is BoundMemberAccessExpression { Member: IMethodSymbol method } memberExpr)
            {
                var argExprs = new List<BoundExpression>();
                bool argErrors = false;
                foreach (var arg in syntax.ArgumentList.Arguments)
                {
                    var boundArg = BindExpression(arg.Expression);
                    if (boundArg is BoundErrorExpression)
                        argErrors = true;
                    argExprs.Add(boundArg);
                }

                if (argErrors)
                    return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.ArgumentBindingFailed);

                if (method.Parameters.Length == argExprs.Count)
                {
                    var convertedArgs = ConvertArguments(method.Parameters, argExprs, syntax.ArgumentList.Arguments);
                    return new BoundInvocationExpression(method, convertedArgs, memberExpr.Receiver);
                }

                receiver = memberExpr.Receiver;
                methodName = method.Name;
            }
            else if (boundMember is BoundTypeExpression { Type: INamedTypeSymbol namedType })
            {
                var argExprs = new List<BoundExpression>();
                bool argErrors = false;
                foreach (var arg in syntax.ArgumentList.Arguments)
                {
                    var boundArg = BindExpression(arg.Expression);
                    if (boundArg is BoundErrorExpression)
                        argErrors = true;
                    argExprs.Add(boundArg);
                }

                if (argErrors)
                    return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.ArgumentBindingFailed);

                return BindConstructorInvocation(namedType, argExprs.ToArray(), syntax);
            }
            else
            {
                receiver = boundMember;
                methodName = "Invoke";
            }
        }
        else if (syntax.Expression is MemberBindingExpressionSyntax memberBinding)
        {
            var boundMember = BindMemberBindingExpression(memberBinding);

            if (boundMember is BoundErrorExpression)
                return boundMember;

            if (boundMember is BoundMethodGroupExpression methodGroup)
                return BindInvocationOnMethodGroup(methodGroup, syntax);

            if (boundMember is BoundMemberAccessExpression { Member: IMethodSymbol method } memberExpr)
            {
                var argExprs = new List<BoundExpression>();
                bool argErrors = false;
                foreach (var arg in syntax.ArgumentList.Arguments)
                {
                    var boundArg = BindExpression(arg.Expression);
                    if (boundArg is BoundErrorExpression)
                        argErrors = true;
                    argExprs.Add(boundArg);
                }

                if (argErrors)
                    return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.ArgumentBindingFailed);

                if (method.Parameters.Length == argExprs.Count)
                {
                    var convertedArgs = ConvertArguments(method.Parameters, argExprs, syntax.ArgumentList.Arguments);
                    return new BoundInvocationExpression(method, convertedArgs, memberExpr.Receiver);
                }

                receiver = memberExpr.Receiver;
                methodName = method.Name;
            }
            else
            {
                receiver = boundMember;
                methodName = "Invoke";
            }
        }
        else if (syntax.Expression is IdentifierNameSyntax id)
        {
            var boundIdentifier = BindIdentifierName(id);

            if (boundIdentifier is BoundErrorExpression)
                return boundIdentifier;

            if (boundIdentifier is BoundMethodGroupExpression methodGroup)
                return BindInvocationOnMethodGroup(methodGroup, syntax);

            if (boundIdentifier is BoundLocalAccess or BoundParameterAccess or BoundFieldAccess or BoundPropertyAccess)
            {
                receiver = boundIdentifier;
                methodName = "Invoke";
            }
            else
            {
                receiver = null;
                methodName = id.Identifier.Text;
            }
        }
        else if (syntax.Expression is GenericNameSyntax generic)
        {
            var boundTypeArguments = TryBindTypeArguments(generic);
            if (boundTypeArguments is null)
                return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.TypeMismatch);

            var symbolCandidates = LookupSymbols(generic.Identifier.Text)
                .OfType<IMethodSymbol>()
                .ToImmutableArray();

            if (!symbolCandidates.IsDefaultOrEmpty)
            {
                var instantiated = InstantiateMethodCandidates(symbolCandidates, boundTypeArguments.Value, generic, syntax.GetLocation());
                if (!instantiated.IsDefaultOrEmpty)
                {
                    var methodGroup = CreateMethodGroup(null, instantiated);
                    return BindInvocationOnMethodGroup(methodGroup, syntax);
                }
            }

            var genericBoundArguments = new BoundExpression[syntax.ArgumentList.Arguments.Count];
            bool genericHasErrors = false;
            int genericIndex = 0;
            foreach (var arg in syntax.ArgumentList.Arguments)
            {
                var boundArg = BindExpression(arg.Expression);
                if (boundArg is BoundErrorExpression)
                    genericHasErrors = true;
                genericBoundArguments[genericIndex++] = boundArg;
            }

            if (genericHasErrors)
                return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);

            var typeExpr = BindTypeSyntax(generic);
            if (typeExpr is BoundTypeExpression type && type.Type is INamedTypeSymbol namedType)
                return BindConstructorInvocation(namedType, genericBoundArguments, syntax);

            return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
        }
        else
        {
            receiver = BindExpression(syntax.Expression);
            if (receiver is BoundErrorExpression)
                return receiver;

            methodName = "Invoke";
        }

        // Bind arguments
        var boundArgumentsList = new List<BoundExpression>();
        bool hasErrors = false;
        foreach (var arg in syntax.ArgumentList.Arguments)
        {
            var boundArg = BindExpression(arg.Expression);
            if (boundArg is BoundErrorExpression)
                hasErrors = true;
            boundArgumentsList.Add(boundArg);
        }

        if (hasErrors)
            return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);

        var boundArguments = boundArgumentsList.ToArray();

        // Handle different receiver kinds
        if (receiver is BoundNamespaceExpression nsReceiver)
        {
            var typeInNs = nsReceiver.Namespace
                .GetMembers(methodName)
                .OfType<INamedTypeSymbol>()
                .FirstOrDefault();

            if (typeInNs is null)
            {
                _diagnostics.ReportTypeOrNamespaceNameDoesNotExistInTheNamespace(methodName, nsReceiver.Namespace.Name, syntax.Expression.GetLocation());
                return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
            }

            return BindConstructorInvocation(typeInNs, boundArguments, syntax, receiver);
        }

        if (receiver is BoundTypeExpression typeReceiver)
        {
            var candidateMethods = new SymbolQuery(methodName, typeReceiver.Type, IsStatic: true)
                .LookupMethods(this)
                .ToImmutableArray();

            if (!candidateMethods.IsDefaultOrEmpty)
            {
                var accessibleMethods = GetAccessibleMethods(candidateMethods, syntax.Expression.GetLocation());

                if (accessibleMethods.IsDefaultOrEmpty)
                    return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.Inaccessible);

                var resolution = OverloadResolver.ResolveOverload(accessibleMethods, boundArguments, Compilation);
                if (resolution.Success)
                {
                    var method = resolution.Method!;
                    var convertedArgs = ConvertArguments(method.Parameters, boundArguments, syntax.ArgumentList.Arguments);
                    return new BoundInvocationExpression(method, convertedArgs, receiver);
                }

                if (resolution.IsAmbiguous)
                {
                    _diagnostics.ReportCallIsAmbiguous(methodName, resolution.AmbiguousCandidates, syntax.GetLocation());
                    return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.Ambiguous);
                }

                var nestedType = typeReceiver.Type
                    .GetMembers(methodName)
                    .OfType<INamedTypeSymbol>()
                    .FirstOrDefault();

                if (nestedType is not null)
                    return BindConstructorInvocation(nestedType, boundArguments, syntax, receiver);

                _diagnostics.ReportNoOverloadForMethod(methodName, boundArguments.Length, syntax.GetLocation());
                return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.OverloadResolutionFailed);
            }

            var nested = typeReceiver.Type
                .GetMembers(methodName)
                .OfType<INamedTypeSymbol>()
                .FirstOrDefault();

            if (nested is not null)
                return BindConstructorInvocation(nested, boundArguments, syntax, receiver);

            _diagnostics.ReportMemberDoesNotContainDefinition(typeReceiver.Type.Name, methodName, syntax.Expression.GetLocation());

            return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
        }

        if (receiver != null)
        {
            var candidates = new SymbolQuery(methodName, receiver.Type, IsStatic: false)
                .LookupMethods(this)
                .ToImmutableArray();

            if (candidates.IsDefaultOrEmpty)
            {
                if (methodName == "Invoke")
                    _diagnostics.ReportInvalidInvocation(syntax.Expression.GetLocation());
                else
                    _diagnostics.ReportTheNameDoesNotExistInTheCurrentContext(methodName, syntax.Expression.GetLocation());

                return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
            }

            var accessibleCandidates = GetAccessibleMethods(candidates, syntax.Expression.GetLocation());

            if (accessibleCandidates.IsDefaultOrEmpty)
                return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.Inaccessible);

            var resolution = OverloadResolver.ResolveOverload(accessibleCandidates, boundArguments, Compilation);
            if (resolution.Success)
            {
                var method = resolution.Method!;
                var convertedArgs = ConvertArguments(method.Parameters, boundArguments, syntax.ArgumentList.Arguments);
                return new BoundInvocationExpression(method, convertedArgs, receiver);
            }

            if (resolution.IsAmbiguous)
            {
                _diagnostics.ReportCallIsAmbiguous(methodName, resolution.AmbiguousCandidates, syntax.GetLocation());
                return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.Ambiguous);
            }

            _diagnostics.ReportNoOverloadForMethod(methodName, boundArguments.Length, syntax.GetLocation());
            return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.OverloadResolutionFailed);
        }

        // No receiver -> try methods first, then constructors
        var methodCandidates = new SymbolQuery(methodName).LookupMethods(this).ToImmutableArray();

        if (!methodCandidates.IsDefaultOrEmpty)
        {
            var accessibleMethods = GetAccessibleMethods(methodCandidates, syntax.Expression.GetLocation());

            if (accessibleMethods.IsDefaultOrEmpty)
                return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.Inaccessible);

            var resolution = OverloadResolver.ResolveOverload(accessibleMethods, boundArguments, Compilation);
            if (resolution.Success)
            {
                var method = resolution.Method!;
                var convertedArgs = ConvertArguments(method.Parameters, boundArguments, syntax.ArgumentList.Arguments);
                return new BoundInvocationExpression(method, convertedArgs, null);
            }

            if (resolution.IsAmbiguous)
            {
                _diagnostics.ReportCallIsAmbiguous(methodName, resolution.AmbiguousCandidates, syntax.GetLocation());
                return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.Ambiguous);
            }

            // Fall back to type if overload resolution failed
            var typeFallback = LookupType(methodName) as INamedTypeSymbol;
            if (typeFallback is not null)
                return BindConstructorInvocation(typeFallback, boundArguments, syntax);

            _diagnostics.ReportNoOverloadForMethod(methodName, boundArguments.Length, syntax.GetLocation());
            return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.OverloadResolutionFailed);
        }

        var typeSymbol = LookupType(methodName) as INamedTypeSymbol;
        if (typeSymbol is not null)
            return BindConstructorInvocation(typeSymbol, boundArguments, syntax);

        _diagnostics.ReportTheNameDoesNotExistInTheCurrentContext(methodName, syntax.Expression.GetLocation());
        return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
    }

    private BoundExpression[] BindInvocationArguments(SeparatedSyntaxList<ArgumentSyntax> arguments, out bool hasErrors)
    {
        if (arguments.Count == 0)
        {
            hasErrors = false;
            return Array.Empty<BoundExpression>();
        }

        var boundArguments = new BoundExpression[arguments.Count];
        var seenErrors = false;

        for (int i = 0; i < arguments.Count; i++)
        {
            var boundArg = BindExpression(arguments[i].Expression);
            if (boundArg is BoundErrorExpression)
                seenErrors = true;

            boundArguments[i] = boundArg;
        }

        hasErrors = seenErrors;
        return boundArguments;
    }

    private BoundExpression BindInvocationOnMethodGroup(BoundMethodGroupExpression methodGroup, InvocationExpressionSyntax syntax)
    {
        var boundArguments = BindInvocationArguments(syntax.ArgumentList.Arguments, out var hasErrors);
        if (hasErrors)
            return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.ArgumentBindingFailed);

        var methodName = methodGroup.Methods[0].Name;
        var selected = methodGroup.SelectedMethod;

        if (selected is not null && selected.Parameters.Length == boundArguments.Length)
        {
            var converted = ConvertArguments(selected.Parameters, boundArguments, syntax.ArgumentList.Arguments);
            return new BoundInvocationExpression(selected, converted, methodGroup.Receiver);
        }

        var resolution = OverloadResolver.ResolveOverload(methodGroup.Methods, boundArguments, Compilation);

        if (resolution.Success)
        {
            var method = resolution.Method!;
            var convertedArgs = ConvertArguments(method.Parameters, boundArguments, syntax.ArgumentList.Arguments);
            return new BoundInvocationExpression(method, convertedArgs, methodGroup.Receiver);
        }

        if (resolution.IsAmbiguous)
        {
            _diagnostics.ReportCallIsAmbiguous(methodName, resolution.AmbiguousCandidates, syntax.GetLocation());
            return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.Ambiguous);
        }

        _diagnostics.ReportNoOverloadForMethod(methodName, boundArguments.Length, syntax.GetLocation());
        return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.OverloadResolutionFailed);
    }

    private ITypeSymbol? TryGetCommonParameterType(ImmutableArray<IMethodSymbol> candidates, int index)
    {
        if (candidates.IsDefaultOrEmpty)
            return null;

        ITypeSymbol? parameterType = null;

        foreach (var candidate in candidates)
        {
            if (candidate.Parameters.Length <= index)
                return null;

            var candidateType = candidate.Parameters[index].Type;

            if (parameterType is null)
            {
                parameterType = candidateType;
                continue;
            }

            if (!SymbolEqualityComparer.Default.Equals(parameterType, candidateType))
                return null;
        }

        return parameterType;
    }

    private BoundExpression BindConstructorInvocation(
        INamedTypeSymbol typeSymbol,
        BoundExpression[] boundArguments,
        InvocationExpressionSyntax syntax,
        BoundExpression? receiver = null)
    {
        var resolution = OverloadResolver.ResolveOverload(typeSymbol.Constructors, boundArguments, Compilation);
        if (resolution.Success)
        {
            var constructor = resolution.Method!;
            if (!EnsureMemberAccessible(constructor, syntax.Expression.GetLocation(), "constructor"))
                return new BoundErrorExpression(typeSymbol, null, BoundExpressionReason.Inaccessible);
            var convertedArgs = ConvertArguments(constructor.Parameters, boundArguments, syntax.ArgumentList.Arguments);
            return new BoundObjectCreationExpression(constructor, convertedArgs, receiver);
        }

        if (resolution.IsAmbiguous)
        {
            _diagnostics.ReportCallIsAmbiguous(typeSymbol.Name, resolution.AmbiguousCandidates, syntax.GetLocation());
            return new BoundErrorExpression(typeSymbol, null, BoundExpressionReason.Ambiguous);
        }

        _diagnostics.ReportNoOverloadForMethod(typeSymbol.Name, boundArguments.Length, syntax.GetLocation());
        return new BoundErrorExpression(typeSymbol, null, BoundExpressionReason.OverloadResolutionFailed);
    }

    private BoundExpression BindObjectCreationExpression(ObjectCreationExpressionSyntax syntax)
    {
        INamedTypeSymbol? typeSymbol = null;

        var typeExpr = BindTypeSyntax(syntax.Type);

        if (typeExpr is BoundTypeExpression boundType)
        {
            typeSymbol = boundType.Type as INamedTypeSymbol;
        }
        else
        {
            //_diagnostics.ReportInvalidObjectCreation(syntax.Type.GetLocation());
            return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
        }

        if (typeSymbol == null)
        {
            //_diagnostics.ReportInvalidObjectCreation(syntax.Type.GetLocation());
            return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
        }

        if (typeSymbol == null)
        {
            _diagnostics.ReportTheNameDoesNotExistInTheCurrentContext(syntax.Type.ToString(), syntax.Type.GetLocation());
            return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
        }

        if (typeSymbol.TypeKind != TypeKind.Error)
        {
            var validatedType = EnsureTypeAccessible(typeSymbol, syntax.Type.GetLocation());
            if (validatedType.TypeKind == TypeKind.Error)
                return new BoundErrorExpression(typeSymbol, null, BoundExpressionReason.Inaccessible);
        }

        // Bind arguments
        var boundArguments = new BoundExpression[syntax.ArgumentList.Arguments.Count];
        bool hasErrors = false;
        int i = 0;
        foreach (var arg in syntax.ArgumentList.Arguments)
        {
            var boundArg = BindExpression(arg.Expression);
            if (boundArg is BoundErrorExpression)
                hasErrors = true;
            boundArguments[i++] = boundArg;
        }

        if (hasErrors)
            return new BoundErrorExpression(typeSymbol, null, BoundExpressionReason.ArgumentBindingFailed);

        // Overload resolution
        var resolution = OverloadResolver.ResolveOverload(typeSymbol.Constructors, boundArguments, Compilation);
        if (resolution.Success)
        {
            var constructor = resolution.Method!;
            if (!EnsureMemberAccessible(constructor, syntax.Type.GetLocation(), "constructor"))
                return new BoundErrorExpression(typeSymbol, null, BoundExpressionReason.Inaccessible);

            var convertedArgs = ConvertArguments(constructor.Parameters, boundArguments, syntax.ArgumentList.Arguments);
            return new BoundObjectCreationExpression(constructor, convertedArgs);
        }

        if (resolution.IsAmbiguous)
        {
            _diagnostics.ReportCallIsAmbiguous(typeSymbol.Name, resolution.AmbiguousCandidates, syntax.GetLocation());
            return new BoundErrorExpression(typeSymbol, null, BoundExpressionReason.Ambiguous);
        }

        _diagnostics.ReportNoOverloadForMethod(typeSymbol.Name, boundArguments.Length, syntax.GetLocation());
        return new BoundErrorExpression(typeSymbol, null, BoundExpressionReason.OverloadResolutionFailed);
    }

    private BoundExpression BindConditionalAccessExpression(ConditionalAccessExpressionSyntax syntax)
    {
        var receiver = BindExpression(syntax.Expression);

        if (receiver is BoundErrorExpression)
            return receiver;

        BoundExpression whenNotNull;

        switch (syntax.WhenNotNull)
        {
            case MemberBindingExpressionSyntax memberBinding:
                {
                    var name = memberBinding.Name.Identifier.Text;
                    var member = receiver.Type is null
                        ? null
                        : new SymbolQuery(name, receiver.Type, IsStatic: false).Lookup(this).FirstOrDefault();

                    if (member is null)
                    {
                        _diagnostics.ReportTheNameDoesNotExistInTheCurrentContext(name, memberBinding.Name.GetLocation());
                        whenNotNull = new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
                    }
                    else
                    {
                        if (!EnsureMemberAccessible(member, memberBinding.Name.GetLocation(), GetSymbolKindForDiagnostic(member)))
                        {
                            whenNotNull = new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.Inaccessible);
                        }
                        else
                        {
                            whenNotNull = new BoundMemberAccessExpression(receiver, member);
                        }
                    }
                    break;
                }

            case InvocationExpressionSyntax { Expression: MemberBindingExpressionSyntax memberBinding } invocation:
                {
                    var name = memberBinding.Name.Identifier.Text;
                    var boundArguments = invocation.ArgumentList.Arguments.Select(a => BindExpression(a.Expression)).ToArray();

                    var candidates = receiver.Type is null
                        ? ImmutableArray<IMethodSymbol>.Empty
                        : new SymbolQuery(name, receiver.Type, IsStatic: false).LookupMethods(this).ToImmutableArray();

                    if (candidates.IsDefaultOrEmpty)
                    {
                        _diagnostics.ReportTheNameDoesNotExistInTheCurrentContext(name, memberBinding.Name.GetLocation());
                        whenNotNull = new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
                    }
                    else
                    {
                        var accessibleCandidates = GetAccessibleMethods(candidates, memberBinding.Name.GetLocation());

                        if (accessibleCandidates.IsDefaultOrEmpty)
                        {
                            whenNotNull = new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.Inaccessible);
                        }
                        else
                        {
                            var resolution = OverloadResolver.ResolveOverload(accessibleCandidates, boundArguments, Compilation);
                            if (resolution.Success)
                            {
                                whenNotNull = new BoundInvocationExpression(resolution.Method!, boundArguments, receiver);
                            }
                            else if (resolution.IsAmbiguous)
                            {
                                _diagnostics.ReportCallIsAmbiguous(name, resolution.AmbiguousCandidates, invocation.GetLocation());
                                whenNotNull = new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.Ambiguous);
                            }
                            else
                            {
                                _diagnostics.ReportNoOverloadForMethod(name, boundArguments.Length, invocation.GetLocation());
                                whenNotNull = new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.OverloadResolutionFailed);
                            }
                        }
                    }
                    break;
                }

            default:
                whenNotNull = new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
                break;
        }

        var resultType = whenNotNull.Type;
        if (!resultType.IsNullable())
            resultType = new NullableTypeSymbol(resultType, null, null, null, []);

        return new BoundConditionalAccessExpression(receiver, whenNotNull, resultType);
    }

    private BoundExpression BindElementAccessExpression(ElementAccessExpressionSyntax syntax)
    {
        var receiver = BindExpression(syntax.Expression);
        var argumentExprs = syntax.ArgumentList.Arguments.Select(x => BindExpression(x.Expression)).ToArray();

        var receiverType = receiver.Type;
        if (receiverType is null)
        {
            _diagnostics.ReportInvalidInvocation(syntax.GetLocation());
            return new BoundErrorExpression(
                Compilation.GetSpecialType(SpecialType.System_Object),
                null,
                BoundExpressionReason.NotFound);
        }

        if (receiverType.TypeKind is TypeKind.Array)
        {
            return new BoundArrayAccessExpression(receiver, argumentExprs, ((IArrayTypeSymbol)receiverType).ElementType);
        }

        var indexer = ResolveIndexer(receiverType, argumentExprs.Length);

        if (indexer is null)
        {
            _diagnostics.ReportCannotApplyIndexingWithToAnExpressionOfType(
                receiverType.Name,
                syntax.GetLocation());
            return new BoundErrorExpression(receiverType, null, BoundExpressionReason.NotFound);
        }

        return new BoundIndexerAccessExpression(receiver, argumentExprs, indexer);
    }

    private IPropertySymbol? ResolveIndexer(ITypeSymbol receiverType, int argCount)
    {
        return receiverType
            .GetMembers()
            .OfType<IPropertySymbol>()
            .FirstOrDefault(p => p.IsIndexer &&
                                 p.GetMethod is not null &&
                                 p.GetMethod.Parameters.Length == argCount);
    }

    private BoundExpression BindAssignment(ExpressionSyntax leftSyntax, ExpressionSyntax rightSyntax, SyntaxNode node)
    {
        if (leftSyntax is ElementAccessExpressionSyntax elementAccess)
        {
            var right = BindExpression(rightSyntax);

            var receiver = BindExpression(elementAccess.Expression);
            var args = elementAccess.ArgumentList.Arguments.Select(x => BindExpression(x.Expression)).ToArray();

            if (receiver.Type is IArrayTypeSymbol arrayType)
            {
                if (arrayType.ElementType.TypeKind != TypeKind.Error &&
                    ShouldAttemptConversion(right))
                {
                    if (!IsAssignable(arrayType.ElementType, right.Type, out var conversion))
                    {
                        _diagnostics.ReportCannotAssignFromTypeToType(
                            right.Type.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                            arrayType.ElementType.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                            rightSyntax.GetLocation());
                        return new BoundErrorExpression(arrayType.ElementType, null, BoundExpressionReason.TypeMismatch);
                    }

                    right = ApplyConversion(right, arrayType.ElementType, conversion, rightSyntax);
                }

                return new BoundArrayAssignmentExpression(
                    new BoundArrayAccessExpression(receiver, args, arrayType.ElementType),
                    right);
            }

            var indexer = ResolveIndexer(receiver.Type!, args.Length);

            if (indexer is null || indexer.SetMethod is null)
            {
                _diagnostics.ReportLeftOfAssignmentMustBeAVariablePropertyOrIndexer(node.GetLocation());
                return new BoundErrorExpression(receiver.Type!, null, BoundExpressionReason.NotFound);
            }

            var access = new BoundIndexerAccessExpression(receiver, args, indexer);
            if (indexer.Type.TypeKind != TypeKind.Error &&
                ShouldAttemptConversion(right))
            {
                if (!IsAssignable(indexer.Type, right.Type, out var conversion))
                {
                    _diagnostics.ReportCannotAssignFromTypeToType(
                        right.Type.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        indexer.Type.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        rightSyntax.GetLocation());
                    return new BoundErrorExpression(indexer.Type, null, BoundExpressionReason.TypeMismatch);
                }

                right = ApplyConversion(right, indexer.Type, conversion, rightSyntax);
            }

            return new BoundIndexerAssignmentExpression(access, right);
        }

        // Fall back to normal variable/property assignment
        var left = BindExpression(leftSyntax);

        if (left.Symbol is ILocalSymbol localSymbol)
        {
            if (!localSymbol.IsMutable)
            {
                _diagnostics.ReportThisValueIsNotMutable(leftSyntax.GetLocation());
                return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
            }

            var right2 = BindExpression(rightSyntax);

            if (right2 is BoundEmptyCollectionExpression)
            {
                return new BoundLocalAssignmentExpression(localSymbol, new BoundEmptyCollectionExpression(localSymbol.Type));
            }

            if (localSymbol.Type.TypeKind != TypeKind.Error &&
                ShouldAttemptConversion(right2))
            {
                if (!IsAssignable(localSymbol.Type, right2.Type!, out var conversion))
                {
                    _diagnostics.ReportCannotAssignFromTypeToType(
                        right2.Type!.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        localSymbol.Type.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        rightSyntax.GetLocation());
                    return new BoundErrorExpression(localSymbol.Type, null, BoundExpressionReason.TypeMismatch);
                }

                right2 = ApplyConversion(right2, localSymbol.Type, conversion, rightSyntax);
            }

            return new BoundLocalAssignmentExpression(localSymbol, right2);
        }
        else if (left.Symbol is IFieldSymbol fieldSymbol)
        {
            var right2 = BindExpression(rightSyntax);

            if (right2 is BoundEmptyCollectionExpression)
            {
                return new BoundFieldAssignmentExpression(right2, fieldSymbol, new BoundEmptyCollectionExpression(fieldSymbol.Type));
            }

            if (fieldSymbol.Type.TypeKind != TypeKind.Error &&
                ShouldAttemptConversion(right2))
            {
                if (!IsAssignable(fieldSymbol.Type, right2.Type!, out var conversion))
                {
                    _diagnostics.ReportCannotAssignFromTypeToType(
                        right2.Type!.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        fieldSymbol.Type.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        rightSyntax.GetLocation());
                    return new BoundErrorExpression(fieldSymbol.Type, null, BoundExpressionReason.TypeMismatch);
                }

                right2 = ApplyConversion(right2, fieldSymbol.Type, conversion, rightSyntax);
            }

            return new BoundFieldAssignmentExpression(GetReceiver(left), fieldSymbol, right2);
        }
        else if (left.Symbol is IPropertySymbol propertySymbol)
        {
            SourceFieldSymbol? backingField = null;

            if (propertySymbol.SetMethod is null)
            {
                if (!TryGetWritableAutoPropertyBackingField(propertySymbol, left, out backingField))
                {
                    _diagnostics.ReportPropertyOrIndexerCannotBeAssignedIsReadOnly(propertySymbol.Name, leftSyntax.GetLocation());
                    return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
                }
            }

            var right2 = BindExpression(rightSyntax);

            if (right2 is BoundEmptyCollectionExpression)
            {
                var empty = new BoundEmptyCollectionExpression(propertySymbol.Type);

                if (backingField is not null)
                {
                    return new BoundFieldAssignmentExpression(right2, backingField, empty);
                }

                return new BoundPropertyAssignmentExpression(right2, propertySymbol, empty);
            }

            if (propertySymbol.Type.TypeKind != TypeKind.Error &&
                ShouldAttemptConversion(right2))
            {
                if (!IsAssignable(propertySymbol.Type, right2.Type!, out var conversion))
                {
                    _diagnostics.ReportCannotAssignFromTypeToType(
                        right2.Type!.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        propertySymbol.Type.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        rightSyntax.GetLocation());
                    return new BoundErrorExpression(propertySymbol.Type, null, BoundExpressionReason.TypeMismatch);
                }

                right2 = ApplyConversion(right2, propertySymbol.Type, conversion, rightSyntax);
            }

            if (backingField is not null)
            {
                return new BoundFieldAssignmentExpression(GetReceiver(left), backingField, right2);
            }

            return new BoundPropertyAssignmentExpression(GetReceiver(left), propertySymbol, right2);
        }

        return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
    }

    private BoundExpression BindAssignmentExpression(AssignmentExpressionSyntax syntax)
        => BindAssignment(syntax.Left, syntax.Right, syntax);

    private BoundStatement BindAssignmentStatement(AssignmentStatementSyntax syntax)
    {
        var bound = BindAssignment(syntax.Left, syntax.Right, syntax);
        return bound is BoundAssignmentExpression assignment
            ? new BoundAssignmentStatement(assignment)
            : new BoundExpressionStatement(bound);
    }

    private bool TryGetWritableAutoPropertyBackingField(
        IPropertySymbol propertySymbol,
        BoundExpression left,
        out SourceFieldSymbol? backingField)
    {
        backingField = null;

        if (propertySymbol is not SourcePropertySymbol sourceProperty ||
            !sourceProperty.IsAutoProperty ||
            sourceProperty.BackingField is null)
        {
            return false;
        }

        if (_containingSymbol is not IMethodSymbol methodSymbol ||
            !methodSymbol.IsConstructor)
        {
            return false;
        }

        if (!SymbolEqualityComparer.Default.Equals(methodSymbol.ContainingType, propertySymbol.ContainingType))
        {
            return false;
        }

        if (propertySymbol.IsStatic)
        {
            return false;
        }

        if (!IsConstructorSelfReceiver(methodSymbol, GetReceiver(left)))
        {
            return false;
        }

        backingField = sourceProperty.BackingField;
        return true;
    }

    private static bool IsConstructorSelfReceiver(IMethodSymbol methodSymbol, BoundExpression? receiver)
    {
        if (receiver is null)
            return true;

        if (receiver is BoundSelfExpression)
            return true;

        if (methodSymbol.IsNamedConstructor &&
            receiver is BoundLocalAccess localAccess &&
            localAccess.Local is SourceLocalSymbol localSymbol &&
            localSymbol.Name == "__self")
        {
            return true;
        }

        return false;
    }

    private BoundExpression? GetReceiver(BoundExpression left)
    {
        // Unwrap receive from member access

        if (left is BoundMemberAccessExpression pa)
            return pa.Receiver;

        if (left is BoundPropertyAccess or BoundFieldAccess)
            return null;

        return left;
    }

    protected bool IsAssignable(ITypeSymbol targetType, ITypeSymbol sourceType, out Conversion conversion)
    {
        if (targetType.TypeKind == TypeKind.Error || sourceType.TypeKind == TypeKind.Error)
        {
            conversion = new Conversion(isImplicit: true, isIdentity: true);
            return true;
        }

        conversion = Compilation.ClassifyConversion(sourceType, targetType);
        return conversion.Exists && conversion.IsImplicit;
    }

    private static bool ShouldAttemptConversion(BoundExpression expression)
    {
        return expression is BoundMethodGroupExpression || expression.Type is { TypeKind: not TypeKind.Error };
    }

    private BoundExpression ApplyConversion(BoundExpression expression, ITypeSymbol targetType, Conversion conversion, SyntaxNode? syntax = null)
    {
        if (!conversion.Exists || expression is BoundErrorExpression)
            return expression;

        if (targetType.TypeKind == TypeKind.Error)
            return expression;

        if (expression is BoundMethodGroupExpression methodGroup)
        {
            return ConvertMethodGroupToDelegate(methodGroup, targetType, syntax);
        }

        if (conversion.IsIdentity)
        {
            if (expression is BoundLiteralExpression literal &&
                literal.Kind == BoundLiteralExpressionKind.NullLiteral &&
                !SymbolEqualityComparer.Default.Equals(literal.GetConvertedType(), targetType))
            {
                return new BoundLiteralExpression(literal.Kind, literal.Value, literal.Type, targetType);
            }

            return expression;
        }

        if (expression is BoundLiteralExpression literalNull &&
            literalNull.Kind == BoundLiteralExpressionKind.NullLiteral)
        {
            return new BoundLiteralExpression(literalNull.Kind, literalNull.Value, literalNull.Type, targetType);
        }

        if (targetType is NullableTypeSymbol nullableTarget && !nullableTarget.UnderlyingType.IsValueType)
            return expression;

        return new BoundCastExpression(expression, targetType, conversion);
    }

    private static string GetMethodGroupDisplay(BoundMethodGroupExpression methodGroup)
    {
        var method = methodGroup.Methods[0];
        return method.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat);
    }

    private BoundMethodGroupExpression ReportMethodGroupRequiresDelegate(BoundMethodGroupExpression methodGroup, SyntaxNode syntax)
    {
        _diagnostics.ReportMethodGroupRequiresDelegateType(GetMethodGroupDisplay(methodGroup), syntax.GetLocation());

        if (methodGroup.Reason != BoundExpressionReason.None)
            return methodGroup;

        return new BoundMethodGroupExpression(
            methodGroup.Receiver,
            methodGroup.Methods,
            methodGroup.MethodGroupType,
            methodGroup.DelegateTypeFactory,
            methodGroup.SelectedMethod,
            BoundExpressionReason.OverloadResolutionFailed);
    }

    protected BoundExpression[] ConvertArguments(ImmutableArray<IParameterSymbol> parameters, IReadOnlyList<BoundExpression> arguments, SeparatedSyntaxList<ArgumentSyntax> argumentSyntaxes)
    {
        var converted = new BoundExpression[arguments.Count];

        for (int i = 0; i < arguments.Count; i++)
        {
            var argument = arguments[i];
            var parameter = parameters[i];

            if (parameter.RefKind is RefKind.Ref or RefKind.Out or RefKind.In)
            {
                converted[i] = argument;
                continue;
            }

            if (!ShouldAttemptConversion(argument) ||
                parameter.Type.TypeKind == TypeKind.Error)
            {
                converted[i] = argument;
                continue;
            }

            if (!IsAssignable(parameter.Type, argument.Type, out var conversion))
            {
                _diagnostics.ReportCannotConvertFromTypeToType(
                    argument.Type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    parameter.Type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    argumentSyntaxes[i].Expression.GetLocation());

                converted[i] = new BoundErrorExpression(parameter.Type, null, BoundExpressionReason.TypeMismatch);
                continue;
            }

            converted[i] = ApplyConversion(argument, parameter.Type, conversion, argumentSyntaxes[i].Expression);
        }

        return converted;
    }

    private BoundExpression BindCollectionExpression(CollectionExpressionSyntax syntax)
    {
        var targetType = GetTargetType(syntax);

        // Empty collection: defer to target type if available
        if (syntax.Elements.Count == 0)
        {
            if (targetType != null)
                return new BoundEmptyCollectionExpression(targetType);

            return new BoundEmptyCollectionExpression();
        }

        var elements = new List<BoundExpression>(syntax.Elements.Count);
        var elementNodes = new List<SyntaxNode>(syntax.Elements.Count);

        foreach (var elementSyntax in syntax.Elements)
        {
            BoundExpression boundElement;
            SyntaxNode elementNode;
            switch (elementSyntax)
            {
                case ExpressionElementSyntax exprElem:
                    boundElement = BindExpression(exprElem.Expression);
                    elementNode = exprElem.Expression;
                    break;
                case SpreadElementSyntax spreadElem:
                    var spreadExpr = BindExpression(spreadElem.Expression);
                    if (!IsSpreadEnumerable(spreadExpr.Type!))
                    {
                        _diagnostics.ReportSpreadSourceMustBeEnumerable(
                            spreadExpr.Type!.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                            spreadElem.GetLocation());
                    }

                    boundElement = new BoundSpreadElement(spreadExpr);
                    elementNode = spreadElem.Expression;
                    break;
                default:
                    continue;
            }

            elements.Add(boundElement);
            elementNodes.Add(elementNode);
        }

        if (targetType is IArrayTypeSymbol arrayType)
        {
            var elementType = arrayType.ElementType;

            var converted = ImmutableArray.CreateBuilder<BoundExpression>(elements.Count);

            for (var i = 0; i < elements.Count; i++)
            {
                var element = elements[i];
                var elementSyntax = elementNodes[i];

                if (element is BoundSpreadElement spread)
                {
                    var sourceType = GetSpreadElementType(spread.Expression.Type!);
                    if (!IsAssignable(elementType, sourceType, out _))
                    {
                        _diagnostics.ReportCannotConvertFromTypeToType(
                            sourceType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                            elementType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                            syntax.GetLocation());
                    }

                    converted.Add(element);
                    continue;
                }

                if (elementType.TypeKind != TypeKind.Error &&
                    ShouldAttemptConversion(element))
                {
                    if (!IsAssignable(elementType, element.Type!, out var conversion))
                    {
                        _diagnostics.ReportCannotConvertFromTypeToType(
                            element.Type!.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                            elementType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                            syntax.GetLocation());
                    }
                    else
                    {
                        converted.Add(ApplyConversion(element, elementType, conversion, elementSyntax));
                        continue;
                    }
                }

                converted.Add(element);
            }

            return new BoundCollectionExpression(arrayType, converted.ToImmutable());
        }

        if (targetType is INamedTypeSymbol namedType)
        {
            // Look for an Add method to infer element type
            var addMethod = new SymbolQuery("Add", namedType, IsStatic: false)
                .Lookup(this)
                .FirstOrDefault() as IMethodSymbol;

            var elementType = addMethod?.Parameters.Length == 1
                ? addMethod.Parameters[0].Type
                : null;

            if (elementType is null && namedType.TypeArguments.Length == 1)
                elementType = namedType.TypeArguments[0];

            elementType ??= Compilation.GetSpecialType(SpecialType.System_Object);

            var converted = ImmutableArray.CreateBuilder<BoundExpression>(elements.Count);

            for (var i = 0; i < elements.Count; i++)
            {
                var element = elements[i];
                var elementSyntax = elementNodes[i];

                if (element is BoundSpreadElement spread)
                {
                    var sourceType = GetSpreadElementType(spread.Expression.Type!);
                    if (!IsAssignable(elementType, sourceType, out _))
                    {
                        _diagnostics.ReportCannotConvertFromTypeToType(
                            sourceType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                            elementType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                            syntax.GetLocation());
                    }

                    converted.Add(element);
                    continue;
                }

                if (elementType.TypeKind != TypeKind.Error &&
                    ShouldAttemptConversion(element))
                {
                    if (!IsAssignable(elementType, element.Type!, out var conversion))
                    {
                        _diagnostics.ReportCannotConvertFromTypeToType(
                            element.Type!.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                            elementType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                            syntax.GetLocation());
                    }
                    else
                    {
                        converted.Add(ApplyConversion(element, elementType, conversion, elementSyntax));
                        continue;
                    }
                }

                converted.Add(element);
            }

            return new BoundCollectionExpression(namedType, converted.ToImmutable(), addMethod);
        }

        // Fallback to array if target type couldn't be determined
        var inferredElementType = InferCollectionElementType(elements);
        var fallbackArray = Compilation.CreateArrayTypeSymbol(inferredElementType);

        var convertedFallback = ImmutableArray.CreateBuilder<BoundExpression>(elements.Count);

        for (var i = 0; i < elements.Count; i++)
        {
            var element = elements[i];
            var elementSyntax = elementNodes[i];

            if (element is BoundSpreadElement)
            {
                convertedFallback.Add(element);
                continue;
            }

            if (inferredElementType.TypeKind != TypeKind.Error &&
                ShouldAttemptConversion(element))
            {
                if (IsAssignable(inferredElementType, element.Type!, out var conversion))
                {
                    convertedFallback.Add(ApplyConversion(element, inferredElementType, conversion, elementSyntax));
                    continue;
                }
            }

            convertedFallback.Add(element);
        }

        return new BoundCollectionExpression(fallbackArray, convertedFallback.ToImmutable());
    }

    private ITypeSymbol InferCollectionElementType(IEnumerable<BoundExpression> elements)
    {
        ITypeSymbol? inferred = null;

        foreach (var element in elements)
        {
            ITypeSymbol? elementType = element switch
            {
                BoundSpreadElement spread when spread.Expression.Type is ITypeSymbol spreadType
                    => GetSpreadElementType(spreadType),
                _ => element.Type
            };

            if (elementType is null)
                continue;

            elementType = TypeSymbolNormalization.NormalizeForInference(elementType);

            if (elementType.TypeKind == TypeKind.Error)
                continue;

            if (inferred is null)
            {
                inferred = elementType;
                continue;
            }

            inferred = MergeInferredElementType(inferred, elementType);
        }

        return inferred ?? Compilation.GetSpecialType(SpecialType.System_Object);
    }

    private ITypeSymbol MergeInferredElementType(ITypeSymbol current, ITypeSymbol candidate)
    {
        if (SymbolEqualityComparer.Default.Equals(current, candidate))
            return current;

        if (IsAssignable(current, candidate, out _))
            return current;

        if (IsAssignable(candidate, current, out _))
            return candidate;

        return Compilation.GetSpecialType(SpecialType.System_Object);
    }

    private ITypeSymbol GetSpreadElementType(ITypeSymbol type)
    {
        if (type is INamedTypeSymbol named)
        {
            foreach (var iface in named.AllInterfaces)
            {
                if (iface.SpecialType == SpecialType.System_Collections_Generic_IEnumerable_T && iface.TypeArguments.Length == 1)
                    return iface.TypeArguments[0];
            }
        }

        if (type is IArrayTypeSymbol array)
            return array.ElementType;

        if (type is INamedTypeSymbol namedType && namedType.TypeArguments.Length == 1)
            return namedType.TypeArguments[0];

        return Compilation.GetSpecialType(SpecialType.System_Object);
    }

    private bool IsSpreadEnumerable(ITypeSymbol type)
    {
        if (type is IArrayTypeSymbol)
            return true;

        if (type is INamedTypeSymbol named)
        {
            foreach (var iface in named.AllInterfaces)
            {
                if (iface.SpecialType == SpecialType.System_Collections_IEnumerable ||
                    iface.SpecialType == SpecialType.System_Collections_Generic_IEnumerable_T)
                    return true;
            }
        }

        return false;
    }

    public override IEnumerable<ISymbol> LookupSymbols(string name)
    {
        var seen = new HashSet<ISymbol>();
        Binder? current = this;

        while (current is not null)
        {
            if (current is BlockBinder block)
            {
                if (block._locals.TryGetValue(name, out var local) && seen.Add(local.Symbol))
                    yield return local.Symbol;

                if (block._functions.TryGetValue(name, out var func) && seen.Add(func))
                    yield return func;
            }

            if (current is TopLevelBinder topLevelBinder)
            {
                foreach (var param in topLevelBinder.GetParameters())
                    if (param.Name == name && seen.Add(param))
                        yield return param;
            }

            if (current is MethodBinder methodBinder)
            {
                foreach (var param in methodBinder.GetMethodSymbol().Parameters)
                    if (param.Name == name && seen.Add(param))
                        yield return param;
            }

            if (current is TypeMemberBinder typeMemberBinder)
            {
                foreach (var member in typeMemberBinder.ContainingSymbol.GetMembers(name))
                    if (seen.Add(member))
                        yield return member;
            }

            if (current is ImportBinder importBinder)
            {
                foreach (var ns in importBinder.GetImportedNamespacesOrTypeScopes())
                {
                    foreach (var member in ns.GetMembers(name))
                        if (seen.Add(member))
                            yield return member;
                }

                foreach (var type in importBinder.GetImportedTypes())
                {
                    if (type.Name == name && seen.Add(type))
                        yield return type;
                }

                var aliasMap = importBinder.GetAliases();
                if (aliasMap.TryGetValue(name, out var symbols))
                {
                    foreach (var symbol in symbols)
                        if (seen.Add(symbol))
                            yield return symbol;
                }
            }

            current = current.ParentBinder;
        }

        foreach (var member in Compilation.GlobalNamespace.GetMembers(name))
            if (seen.Add(member))
                yield return member;
    }

    public override IEnumerable<ISymbol> LookupAvailableSymbols()
    {
        var seen = new HashSet<string>();
        Binder? current = this;

        while (current is not null)
        {
            // Locals and scoped symbols
            if (current is BlockBinder block)
            {
                foreach (var local in block._locals.Values)
                {
                    if (seen.Add(local.Symbol.Name))
                        yield return local.Symbol;
                }
            }

            // Import namespaces
            if (current is ImportBinder importBinder)
            {
                foreach (var ns in importBinder.GetImportedNamespacesOrTypeScopes())
                {
                    foreach (var member in ns.GetMembers())
                    {
                        if (seen.Add(member.Name))
                            yield return member;
                    }
                }
            }

            if (current is TopLevelBinder topLevelBinder)
            {
                foreach (var param in topLevelBinder.GetParameters())
                {
                    if (seen.Add(param.Name))
                        yield return param;
                }
            }

            current = current.ParentBinder;
        }

        // Also include GlobalNamespace as a last fallback
        foreach (var member in Compilation.GlobalNamespace.GetMembers())
        {
            if (seen.Add(member.Name))
                yield return member;
        }
    }

    public override BoundFunctionStatement BindFunction(FunctionStatementSyntax function)
    {
        // Get the binder from the factory
        var binder = SemanticModel.GetBinder(function, this);

        if (binder is not FunctionBinder functionBinder)
            throw new InvalidOperationException("Expected FunctionBinder");

        // Register the symbol in the current scope
        var symbol = functionBinder.GetMethodSymbol();

        // Bind the body with method binder
        var methodBinder = functionBinder.GetMethodBodyBinder();
        var blockBinder = (BlockBinder)SemanticModel.GetBinder(function.Body, methodBinder);
        blockBinder.BindBlockStatement(function.Body);

        return new BoundFunctionStatement(symbol); // Possibly include body here if needed
    }
}
