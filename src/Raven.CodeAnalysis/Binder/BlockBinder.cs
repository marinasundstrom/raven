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
    private readonly Dictionary<string, ILabelSymbol> _labelsByName = new(StringComparer.Ordinal);
    private readonly Dictionary<LabeledStatementSyntax, ILabelSymbol> _labelsBySyntax = new();
    private readonly Dictionary<ILabelSymbol, LabeledStatementSyntax> _syntaxByLabel = new(SymbolEqualityComparer.Default);
    private readonly HashSet<SyntaxNode> _labelDeclarationNodes = new();
    private readonly Dictionary<LambdaExpressionSyntax, ImmutableArray<INamedTypeSymbol>> _lambdaDelegateTargets = new(ReferenceEqualityComparer.Instance);
    private readonly Dictionary<LambdaRebindKey, BoundLambdaExpression> _reboundLambdaCache = new();
    private int _scopeDepth;
    private bool _allowReturnsInExpression;
    private int _loopDepth;
    private int _expressionContextDepth;
    private int _tempCounter;

    public BlockBinder(ISymbol containingSymbol, Binder parent) : base(parent)
    {
        _containingSymbol = containingSymbol;
    }

    public override ISymbol ContainingSymbol => _containingSymbol;

    public override ISymbol? BindDeclaredSymbol(SyntaxNode node)
    {
        return node switch
        {
            VariableDeclaratorSyntax
            {
                Identifier.ValueText: "_",
                Identifier.IsMissing: false,
                Parent: VariableDeclarationSyntax { BindingKeyword.Kind: SyntaxKind.LetKeyword or SyntaxKind.ValKeyword }
            } => null,
            VariableDeclaratorSyntax v => BindLocalDeclaration(v).Symbol,
            CompilationUnitSyntax unit => BindCompilationUnit(unit).Symbol,
            SingleVariableDesignationSyntax singleVariableDesignation => BindSingleVariableDesignation(singleVariableDesignation).Local,
            FunctionStatementSyntax functionStatement => BindFunction(functionStatement).Method,
            LabeledStatementSyntax labeledStatement => DeclareLabelSymbol(labeledStatement),
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

    private BoundVariableDeclarator BindLocalDeclaration(VariableDeclaratorSyntax variableDeclarator)
    {
        var name = variableDeclarator.Identifier.ValueText;
        var decl = (VariableDeclarationSyntax)variableDeclarator.Parent!;
        var bindingKeyword = decl.BindingKeyword;
        var isUsingDeclaration = decl.Parent is UsingDeclarationStatementSyntax;
        var initializer = variableDeclarator.Initializer;

        var isShadowingExistingInScope = false;

        if (_locals.TryGetValue(name, out var existing) && existing.Depth == _scopeDepth)
        {
            var isSameDeclarator = existing.Symbol.DeclaringSyntaxReferences.Any(reference =>
                reference.SyntaxTree == variableDeclarator.SyntaxTree &&
                reference.Span == variableDeclarator.Span);

            if (isSameDeclarator)
                return new BoundVariableDeclarator(existing.Symbol, null);

            isShadowingExistingInScope = true;
        }

        if (!isShadowingExistingInScope && LookupSymbol(name) is ILocalSymbol or IParameterSymbol or IFieldSymbol)
            isShadowingExistingInScope = true;

        if (isShadowingExistingInScope)
            _diagnostics.ReportVariableShadowsPreviousDeclaration(name, variableDeclarator.Identifier.GetLocation());
        var isConst = bindingKeyword.IsKind(SyntaxKind.ConstKeyword);
        var isMutable = bindingKeyword.IsKind(SyntaxKind.VarKeyword);
        var shouldDispose = isUsingDeclaration;

        ITypeSymbol type = Compilation.ErrorTypeSymbol;
        BoundExpression? boundInitializer = null;
        ITypeSymbol? initializerValueType = null;
        var typeLocation = variableDeclarator.TypeAnnotation?.Type.GetLocation()
            ?? bindingKeyword.GetLocation();
        object? constantValue = null;
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
        }

        var constantValueComputed = false;

        if (type.TypeKind != TypeKind.Error &&
            initializer is not null &&
            boundInitializer is not null &&
            ShouldAttemptConversion(boundInitializer))
        {
            if (!IsAssignable(type, boundInitializer.Type!, out var conversion))
            {
                if (isConst &&
                    initializer is not null &&
                    ConstantValueEvaluator.TryEvaluate(initializer.Value, out var evaluated) &&
                    ConstantValueEvaluator.TryConvert(type, evaluated, out var converted))
                {
                    constantValue = converted;
                    constantValueComputed = true;

                    if (boundInitializer is BoundLiteralExpression literal)
                    {
                        var literalValue = converted ?? literal.Value;
                        boundInitializer = new BoundLiteralExpression(literal.Kind, literalValue!, literal.Type!, type);
                    }

                    CacheBoundNode(initializer.Value, boundInitializer);
                }
                else
                {
                    _diagnostics.ReportCannotAssignFromTypeToType(
                        boundInitializer.Type!.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        type.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        initializer.Value.GetLocation());
                    boundInitializer = new BoundErrorExpression(type, null, BoundExpressionReason.TypeMismatch);
                }
            }
            else
            {
                boundInitializer = ApplyConversion(boundInitializer, type, conversion, initializer.Value);
                CacheBoundNode(initializer.Value, boundInitializer);
            }
        }

        type = EnsureTypeAccessible(type, typeLocation);

        if (!constantValueComputed &&
            isConst &&
            initializer is not null &&
            type.TypeKind != TypeKind.Error &&
            boundInitializer is not BoundErrorExpression)
        {
            if (!ConstantValueEvaluator.TryEvaluate(initializer.Value, out var rawConstant))
            {
                _diagnostics.ReportConstLocalMustBeConstant(name, initializer.Value.GetLocation());
            }
            else if (!ConstantValueEvaluator.TryConvert(type, rawConstant, out constantValue))
            {
                var display = type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);
                _diagnostics.ReportConstLocalCannotConvert(name, display, initializer.Value.GetLocation());
            }
        }

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

        var declarator = new BoundVariableDeclarator(CreateLocalSymbol(variableDeclarator, name, isMutable, type, isConst, constantValue), boundInitializer);

        if (shouldDispose)
            _localsToDispose.Add((declarator.Local, _scopeDepth));

        CacheBoundNode(variableDeclarator, declarator);

        return declarator;
    }

    protected ITypeSymbol EnsureTypeAccessible(ITypeSymbol type, Location location)
    {
        if (type.TypeKind == TypeKind.Error)
            return type;

        if (EnsureMemberAccessible(type, location, "type"))
            return type;

        return Compilation.ErrorTypeSymbol;
    }

    protected bool EnsureMemberAccessible(ISymbol symbol, Location location, string symbolKind)
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

    private ImmutableArray<IPropertySymbol> GetAccessibleProperties(
        ImmutableArray<IPropertySymbol> properties,
        Location location)
    {
        if (properties.IsDefaultOrEmpty)
            return properties;

        var builder = ImmutableArray.CreateBuilder<IPropertySymbol>();

        foreach (var property in properties)
        {
            if (IsSymbolAccessible(property))
                builder.Add(property);
        }

        if (builder.Count > 0)
            return builder.ToImmutable();

        EnsureMemberAccessible(properties[0], location, "property");
        return ImmutableArray<IPropertySymbol>.Empty;
    }

    private BoundExpression BindMethodGroup(BoundExpression? receiver, ImmutableArray<IMethodSymbol> methods, Location location)
    {
        var accessibleMethods = GetAccessibleMethods(methods, location);

        if (accessibleMethods.IsDefaultOrEmpty)
            return ErrorExpression(reason: BoundExpressionReason.Inaccessible);

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

    private SourceLocalSymbol CreateLocalSymbol(SyntaxNode declaringSyntax, string name, bool isMutable, ITypeSymbol type, bool isConst = false, object? constantValue = null)
    {
        var symbol = new SourceLocalSymbol(
            name,
            type,
            isMutable,
            _containingSymbol,
            _containingSymbol.ContainingType as INamedTypeSymbol,
            _containingSymbol?.ContainingNamespace,
            [declaringSyntax.GetLocation()],
            [declaringSyntax.GetReference()],
            isConst,
            constantValue);

        _locals[name] = (symbol, _scopeDepth);
        return symbol;
    }

    private SourceLocalSymbol CreateTempLocal(string nameHint, ITypeSymbol type, SyntaxNode syntax)
    {
        var containingType = _containingSymbol.ContainingType as INamedTypeSymbol;
        var containingNamespace = _containingSymbol.ContainingNamespace;
        var name = $"<{nameHint}>__{_tempCounter++}";

        return new SourceLocalSymbol(
            name,
            type,
            isMutable: true,
            _containingSymbol,
            containingType,
            containingNamespace,
            [syntax.GetLocation()],
            [syntax.GetReference()]);
    }

    public override BoundStatement BindStatement(StatementSyntax statement)
    {
        if (TryGetCachedBoundNode(statement) is BoundStatement cached)
            return cached;

        BoundStatement boundNode = statement switch
        {
            LocalDeclarationStatementSyntax localDeclaration => BindLocalDeclarationStatement(localDeclaration),
            UsingDeclarationStatementSyntax usingDeclaration => BindUsingDeclarationStatement(usingDeclaration),
            AssignmentStatementSyntax assignmentStatement => BindAssignmentStatement(assignmentStatement),
            ExpressionStatementSyntax expressionStmt => BindExpressionStatement(expressionStmt),
            IfStatementSyntax ifStmt => BindIfStatement(ifStmt),
            WhileStatementSyntax whileStmt => BindWhileStatement(whileStmt),
            TryStatementSyntax tryStmt => BindTryStatement(tryStmt),
            FunctionStatementSyntax function => BindFunction(function),
            ReturnStatementSyntax returnStatement => BindReturnStatement(returnStatement),
            ThrowStatementSyntax throwStatement => BindThrowStatement(throwStatement),
            BlockStatementSyntax blockStmt => BindBlockStatement(blockStmt),
            ForStatementSyntax forStmt => BindForStatement(forStmt),
            LabeledStatementSyntax labeledStatement => BindLabeledStatement(labeledStatement),
            GotoStatementSyntax gotoStatement => BindGotoStatement(gotoStatement),
            BreakStatementSyntax breakStatement => BindBreakStatement(breakStatement),
            ContinueStatementSyntax continueStatement => BindContinueStatement(continueStatement),
            YieldReturnStatementSyntax yieldReturn => BindYieldReturnStatement(yieldReturn),
            YieldBreakStatementSyntax yieldBreak => BindYieldBreakStatement(yieldBreak),
            EmptyStatementSyntax emptyStatement => new BoundExpressionStatement(BoundFactory.UnitExpression()),
            IncompleteStatementSyntax incompleteStatement => BindIncompleteStatement(incompleteStatement),
            _ => throw new NotSupportedException($"Unsupported statement: {statement.Kind}")
        };

        CacheBoundNode(statement, boundNode);

        return boundNode;
    }

    private BoundStatement BindIncompleteStatement(IncompleteStatementSyntax incompleteStatement)
    {
        var errorExpression = ErrorExpression(Compilation.ErrorTypeSymbol, reason: BoundExpressionReason.OtherError);
        return new BoundExpressionStatement(errorExpression);
    }

    private BoundStatement BindLocalDeclarationStatement(LocalDeclarationStatementSyntax localDeclaration)
    {
        var declaration = localDeclaration.Declaration;

        if ((declaration.BindingKeyword.IsKind(SyntaxKind.LetKeyword) || declaration.BindingKeyword.IsKind(SyntaxKind.ValKeyword)) &&
            declaration.Declarators.Count > 0 &&
            IsDiscardDeclarator(declaration.Declarators[0]))
        {
            return BindDiscardDeclarator(declaration.Declarators[0], isUsingDeclaration: false);
        }

        var boundDeclarators = ImmutableArray.CreateBuilder<BoundVariableDeclarator>(declaration.Declarators.Count);

        foreach (var declarator in declaration.Declarators)
        {
            boundDeclarators.Add(BindLocalDeclaration(declarator));
        }

        return new BoundLocalDeclarationStatement(boundDeclarators.ToImmutable(), isUsing: false);
    }

    private BoundStatement BindUsingDeclarationStatement(UsingDeclarationStatementSyntax usingDeclaration)
    {
        var declaration = usingDeclaration.Declaration;

        if ((declaration.BindingKeyword.IsKind(SyntaxKind.LetKeyword) || declaration.BindingKeyword.IsKind(SyntaxKind.ValKeyword)) &&
            declaration.Declarators.Count > 0 &&
            IsDiscardDeclarator(declaration.Declarators[0]))
        {
            return BindDiscardDeclarator(declaration.Declarators[0], isUsingDeclaration: true);
        }

        var boundDeclarators = ImmutableArray.CreateBuilder<BoundVariableDeclarator>(declaration.Declarators.Count);

        foreach (var declarator in declaration.Declarators)
        {
            boundDeclarators.Add(BindLocalDeclaration(declarator));
        }

        return new BoundLocalDeclarationStatement(boundDeclarators.ToImmutable(), isUsing: true);
    }

    private static bool IsDiscardDeclarator(VariableDeclaratorSyntax variableDeclarator)
    {
        return !variableDeclarator.Identifier.IsMissing &&
               variableDeclarator.Identifier.ValueText == "_";
    }

    private BoundStatement BindDiscardDeclarator(
        VariableDeclaratorSyntax variableDeclarator,
        bool isUsingDeclaration)
    {
        var initializer = variableDeclarator.Initializer;

        if (initializer is null)
        {
            _diagnostics.ReportLocalVariableMustBeInitialized("_", variableDeclarator.Identifier.GetLocation());
            return new BoundExpressionStatement(BoundFactory.UnitExpression());
        }

        var boundInitializer = BindExpression(initializer.Value, allowReturn: false);

        if (variableDeclarator.TypeAnnotation is not null)
        {
            var annotatedType = ResolveType(variableDeclarator.TypeAnnotation.Type);
            annotatedType = EnsureTypeAccessible(annotatedType, variableDeclarator.TypeAnnotation.Type.GetLocation());

            if (annotatedType.TypeKind != TypeKind.Error && ShouldAttemptConversion(boundInitializer))
            {
                if (!IsAssignable(annotatedType, boundInitializer.Type!, out var conversion))
                {
                    _diagnostics.ReportCannotAssignFromTypeToType(
                        boundInitializer.Type!.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        annotatedType.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        initializer.Value.GetLocation());
                    boundInitializer = new BoundErrorExpression(annotatedType, null, BoundExpressionReason.TypeMismatch);
                }
                else
                {
                    boundInitializer = ApplyConversion(boundInitializer, annotatedType, conversion, initializer.Value);
                }
            }
        }

        CacheBoundNode(initializer.Value, boundInitializer);

        if (isUsingDeclaration)
        {
            _diagnostics.ReportDiscardExpressionNotAllowed(variableDeclarator.Identifier.GetLocation());
        }

        return new BoundExpressionStatement(boundInitializer);
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
            DefaultExpressionSyntax defaultExpression => BindDefaultExpression(defaultExpression),
            TypeOfExpressionSyntax typeOfExpression => BindTypeOfExpression(typeOfExpression),
            TupleExpressionSyntax tupleExpression => BindTupleExpression(tupleExpression),
            IfExpressionSyntax ifExpression => BindIfExpression(ifExpression),
            BlockSyntax block => BindBlock(block, allowReturn: _allowReturnsInExpression),
            IsPatternExpressionSyntax isPatternExpression => BindIsPatternExpression(isPatternExpression),
            MatchExpressionSyntax matchExpression => BindMatchExpression(matchExpression),
            TryExpressionSyntax tryExpression => BindTryExpression(tryExpression),
            LambdaExpressionSyntax lambdaExpression => BindLambdaExpression(lambdaExpression),
            InterpolatedStringExpressionSyntax interpolated => BindInterpolatedStringExpression(interpolated),
            UnaryExpressionSyntax unaryExpression => BindUnaryExpression(unaryExpression),
            PostfixUnaryExpressionSyntax postfixUnary => BindPostfixUnaryExpression(postfixUnary),
            SelfExpressionSyntax selfExpression => BindSelfExpression(selfExpression),
            DiscardExpressionSyntax discardExpression => BindDiscardExpression(discardExpression),
            UnitExpressionSyntax unitExpression => BindUnitExpression(unitExpression),
            ExpressionSyntax.Missing missing => BindMissingExpression(missing),
            _ => throw new NotSupportedException($"Unsupported expression: {syntax.Kind}")
        };

        //CacheBoundNode(syntax, boundNode);

        return boundNode;
    }

    private BoundExpression BindSelfExpression(SelfExpressionSyntax selfExpression)
    {
        if (_containingSymbol is IMethodSymbol method)
        {
            if (!method.IsStatic || method.IsNamedConstructor)
            {
                var containingType = method.ContainingType;
                return new BoundSelfExpression(containingType);
            }

            if (method.IsExtensionMethod && method.Parameters.Length > 0)
            {
                var receiver = method.Parameters[0];
                return new BoundParameterAccess(receiver);
            }
        }

        //_diagnostics.ReportSelfNotAllowed(selfExpression.GetLocation());
        return ErrorExpression(reason: BoundExpressionReason.NotFound);
    }

    private BoundExpression BindCompoundAssignmentValue(
        BoundExpression leftValue,
        BoundExpression rightValue,
        ITypeSymbol targetType,
        SyntaxKind binaryOperatorKind,
        ExpressionSyntax rightSyntax)
    {
        var preparedRight = PrepareRightForAssignment(rightValue, targetType, rightSyntax);

        if (preparedRight is BoundErrorExpression)
            return preparedRight;

        var binary = BindBinaryExpression(binaryOperatorKind, leftValue, preparedRight, rightSyntax.GetLocation());

        return ConvertValueForAssignment(binary, targetType, rightSyntax);
    }

    private BoundExpression PrepareRightForAssignment(BoundExpression right, ITypeSymbol targetType, SyntaxNode syntax)
    {
        if (targetType.TypeKind != TypeKind.Error && ShouldAttemptConversion(right))
        {
            if (!IsAssignable(targetType, right.Type!, out var conversion))
            {
                _diagnostics.ReportCannotAssignFromTypeToType(
                    right.Type!.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    targetType.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    syntax.GetLocation());
                return new BoundErrorExpression(targetType, null, BoundExpressionReason.TypeMismatch);
            }

            right = ApplyConversion(right, targetType, conversion, syntax);
        }

        return right;
    }

    private static SyntaxKind? GetBinaryOperatorFromAssignment(SyntaxKind assignmentOperatorKind)
    {
        return assignmentOperatorKind switch
        {
            SyntaxKind.PlusEqualsToken => SyntaxKind.PlusToken,
            SyntaxKind.MinusEqualsToken => SyntaxKind.MinusToken,
            SyntaxKind.StarEqualsToken => SyntaxKind.StarToken,
            SyntaxKind.SlashEqualsToken => SyntaxKind.SlashToken,
            SyntaxKind.AmpersandEqualsToken => SyntaxKind.AmpersandToken,
            SyntaxKind.BarEqualsToken => SyntaxKind.BarToken,
            _ => null,
        };
    }

    private BoundExpression BindDiscardExpression(DiscardExpressionSyntax discardExpression)
    {
        _diagnostics.ReportDiscardExpressionNotAllowed(discardExpression.GetLocation());
        return ErrorExpression(reason: BoundExpressionReason.UnsupportedOperation);
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
        if (unaryExpression.Kind == SyntaxKind.AwaitExpression)
            return BindAwaitExpression(unaryExpression);

        if (unaryExpression.Kind == SyntaxKind.PreIncrementExpression)
            return BindIncrementOrDecrement(unaryExpression.Expression, unaryExpression.OperatorToken, SyntaxKind.PlusToken, isPostfix: false);

        if (unaryExpression.Kind == SyntaxKind.PreDecrementExpression)
            return BindIncrementOrDecrement(unaryExpression.Expression, unaryExpression.OperatorToken, SyntaxKind.MinusToken, isPostfix: false);

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

    private BoundExpression BindPostfixUnaryExpression(PostfixUnaryExpressionSyntax postfixUnary)
    {
        var binaryOperatorKind = postfixUnary.OperatorToken.Kind == SyntaxKind.PlusPlusToken
            ? SyntaxKind.PlusToken
            : SyntaxKind.MinusToken;

        return BindIncrementOrDecrement(postfixUnary.Expression, postfixUnary.OperatorToken, binaryOperatorKind, isPostfix: true);
    }

    private BoundExpression BindIncrementOrDecrement(
        ExpressionSyntax operandSyntax,
        SyntaxToken operatorToken,
        SyntaxKind binaryOperatorKind,
        bool isPostfix)
    {
        var operand = BindExpression(operandSyntax);

        if (operand is BoundErrorExpression)
            return operand;

        return operand switch
        {
            BoundLocalAccess local => BindIncrementForLocal(local, operandSyntax, binaryOperatorKind, isPostfix),
            BoundParameterAccess parameter => BindIncrementForParameter(parameter, operandSyntax, binaryOperatorKind, isPostfix),
            BoundArrayAccessExpression arrayAccess => BindIncrementCore(
                operandSyntax,
                operand,
                arrayAccess.Type ?? Compilation.ErrorTypeSymbol,
                binaryOperatorKind,
                isPostfix,
                value => BoundFactory.CreateArrayAssignmentExpression(arrayAccess, value)),
            BoundIndexerAccessExpression indexerAccess => BindIncrementCore(
                operandSyntax,
                operand,
                indexerAccess.Type ?? Compilation.ErrorTypeSymbol,
                binaryOperatorKind,
                isPostfix,
                value => BoundFactory.CreateIndexerAssignmentExpression(indexerAccess, value)),
            BoundFieldAccess fieldAccess => BindIncrementForField(fieldAccess, operandSyntax, binaryOperatorKind, isPostfix),
            BoundMemberAccessExpression memberAccess when memberAccess.Symbol is IPropertySymbol propertySymbol
                => BindIncrementForProperty(memberAccess, propertySymbol, operandSyntax, binaryOperatorKind, isPostfix),
            _ => BindInvalidIncrementOperand(operatorToken)
        };
    }

    private BoundExpression BindIncrementForLocal(
        BoundLocalAccess localAccess,
        ExpressionSyntax operandSyntax,
        SyntaxKind binaryOperatorKind,
        bool isPostfix)
    {
        var localSymbol = localAccess.Local;
        var localType = localSymbol.Type ?? Compilation.ErrorTypeSymbol;
        var targetType = GetIncrementTargetType(localType);

        if (!localSymbol.IsMutable)
        {
            _diagnostics.ReportThisValueIsNotMutable(operandSyntax.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.NotFound);
        }

        return BindIncrementCore(
            operandSyntax,
            localAccess,
            targetType,
            binaryOperatorKind,
            isPostfix,
            value => localType is ByRefTypeSymbol byRef
                ? BoundFactory.CreateByRefAssignmentExpression(localAccess, byRef.ElementType, value)
                : BoundFactory.CreateLocalAssignmentExpression(localSymbol, value));
    }

    private BoundExpression BindIncrementForParameter(
        BoundParameterAccess parameterAccess,
        ExpressionSyntax operandSyntax,
        SyntaxKind binaryOperatorKind,
        bool isPostfix)
    {
        var parameterSymbol = parameterAccess.Parameter;
        var parameterType = parameterSymbol.Type ?? Compilation.ErrorTypeSymbol;
        var targetType = GetIncrementTargetType(parameterType);

        if (!parameterSymbol.IsMutable)
        {
            _diagnostics.ReportThisValueIsNotMutable(operandSyntax.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.NotFound);
        }

        return BindIncrementCore(
            operandSyntax,
            parameterAccess,
            targetType,
            binaryOperatorKind,
            isPostfix,
            value => parameterType is ByRefTypeSymbol byRef && parameterSymbol.RefKind is RefKind.Ref or RefKind.Out
                ? BoundFactory.CreateByRefAssignmentExpression(parameterAccess, byRef.ElementType, value)
                : BoundFactory.CreateParameterAssignmentExpression(parameterSymbol, value));
    }

    private BoundExpression BindIncrementForField(
        BoundFieldAccess fieldAccess,
        ExpressionSyntax operandSyntax,
        SyntaxKind binaryOperatorKind,
        bool isPostfix)
    {
        var fieldSymbol = fieldAccess.Field;

        if (fieldSymbol.IsConst)
        {
            _diagnostics.ReportThisValueIsNotMutable(operandSyntax.GetLocation());
            return new BoundErrorExpression(fieldSymbol.Type, null, BoundExpressionReason.NotFound);
        }

        if (!CanAssignToField(fieldSymbol, fieldAccess.Receiver, operandSyntax))
            return new BoundErrorExpression(fieldSymbol.Type, fieldSymbol, BoundExpressionReason.NotFound);

        var targetType = fieldSymbol.Type ?? Compilation.ErrorTypeSymbol;

        return BindIncrementCore(
            operandSyntax,
            fieldAccess,
            targetType,
            binaryOperatorKind,
            isPostfix,
            value => CreateFieldAssignmentExpression(fieldAccess.Receiver, fieldSymbol, value));
    }

    private BoundExpression BindIncrementForProperty(
        BoundMemberAccessExpression memberAccess,
        IPropertySymbol propertySymbol,
        ExpressionSyntax operandSyntax,
        SyntaxKind binaryOperatorKind,
        bool isPostfix)
    {
        SourceFieldSymbol? backingField = null;

        if (propertySymbol.SetMethod is null)
        {
            if (!TryGetWritableAutoPropertyBackingField(propertySymbol, memberAccess, out backingField))
            {
                _diagnostics.ReportPropertyOrIndexerCannotBeAssignedIsReadOnly(propertySymbol.Name, operandSyntax.GetLocation());
                return ErrorExpression(reason: BoundExpressionReason.NotFound);
            }

            var backingAccess = new BoundFieldAccess(memberAccess.Receiver, backingField);

            if (!CanAssignToField(backingField, memberAccess.Receiver, operandSyntax))
                return new BoundErrorExpression(backingField.Type, backingField, BoundExpressionReason.NotFound);

            return BindIncrementCore(
                operandSyntax,
                backingAccess,
                propertySymbol.Type,
                binaryOperatorKind,
                isPostfix,
                value => CreateFieldAssignmentExpression(memberAccess.Receiver, backingField, value));
        }

        return BindIncrementCore(
            operandSyntax,
            memberAccess,
            propertySymbol.Type,
            binaryOperatorKind,
            isPostfix,
            value => BoundFactory.CreatePropertyAssignmentExpression(memberAccess.Receiver, propertySymbol, value));
    }

    private BoundExpression BindIncrementCore(
        ExpressionSyntax operandSyntax,
        BoundExpression valueExpression,
        ITypeSymbol targetType,
        SyntaxKind binaryOperatorKind,
        bool isPostfix,
        Func<BoundExpression, BoundAssignmentExpression> createAssignment)
    {
        if (targetType.TypeKind == TypeKind.Error)
            return ErrorExpression(reason: BoundExpressionReason.TypeMismatch);

        var initialValue = ConvertValueForAssignment(valueExpression, targetType, operandSyntax);

        if (initialValue is BoundErrorExpression)
            return initialValue;

        var tempLocal = CreateTempLocal("inc", targetType, operandSyntax);
        var tempAccess = new BoundLocalAccess(tempLocal);

        var statements = new List<BoundStatement>
        {
            new BoundLocalDeclarationStatement([new BoundVariableDeclarator(tempLocal, initialValue)])
        };

        var stepLiteral = CreateStepLiteral();
        var preparedStep = PrepareRightForAssignment(stepLiteral, targetType, operandSyntax);

        if (preparedStep is BoundErrorExpression)
            return preparedStep;

        var updatedValue = BindBinaryExpression(binaryOperatorKind, tempAccess, preparedStep, operandSyntax.GetLocation());
        var convertedUpdatedValue = ConvertValueForAssignment(updatedValue, targetType, operandSyntax);

        if (convertedUpdatedValue is BoundErrorExpression)
            return convertedUpdatedValue;

        var assignment = createAssignment(convertedUpdatedValue);
        statements.Add(new BoundExpressionStatement(assignment));

        var resultExpression = isPostfix
            ? (BoundExpression)new BoundLocalAccess(tempLocal)
            : convertedUpdatedValue;

        statements.Add(new BoundExpressionStatement(resultExpression));

        return BoundFactory.CreateBlockExpression(statements);
    }

    private BoundExpression BindInvalidIncrementOperand(SyntaxToken operatorToken)
    {
        _diagnostics.ReportLeftOfAssignmentMustBeAVariablePropertyOrIndexer(operatorToken.GetLocation());
        return ErrorExpression(reason: BoundExpressionReason.UnsupportedOperation);
    }

    private ITypeSymbol GetIncrementTargetType(ITypeSymbol type)
    {
        return type is ByRefTypeSymbol byRef ? byRef.ElementType : type;
    }

    private BoundLiteralExpression CreateStepLiteral()
    {
        var intType = Compilation.GetSpecialType(SpecialType.System_Int32);
        var literalType = new LiteralTypeSymbol(intType, 1, Compilation);
        return new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 1, literalType);
    }

    private BoundExpression BindAwaitExpression(UnaryExpressionSyntax awaitExpression)
    {
        if (!IsAwaitExpressionAllowed())
        {
            _diagnostics.ReportAwaitExpressionRequiresAsyncContext(awaitExpression.OperatorToken.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.UnsupportedOperation);
        }

        var operand = BindExpression(awaitExpression.Expression);

        if (operand is BoundErrorExpression)
            return operand;

        var operandType = operand.Type;

        if (operandType is null || operandType.TypeKind == TypeKind.Error)
        {
            _diagnostics.ReportExpressionIsNotAwaitable(
                operandType?.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat) ?? "<unknown>",
                awaitExpression.Expression.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.UnsupportedOperation);
        }

        if (!AwaitablePattern.TryFind(operandType, IsSymbolAccessible, out var awaitable, out var failure, out var awaiterType))
        {
            switch (failure)
            {
                case AwaitablePatternFailure.IsCompletedMissing:
                    _diagnostics.ReportAwaiterMissingIsCompleted(
                        (awaiterType ?? operandType).ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        awaitExpression.Expression.GetLocation());
                    break;
                case AwaitablePatternFailure.GetResultMissing:
                    _diagnostics.ReportAwaiterMissingGetResult(
                        (awaiterType ?? operandType).ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        awaitExpression.Expression.GetLocation());
                    break;
                default:
                    _diagnostics.ReportExpressionIsNotAwaitable(
                        operandType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        awaitExpression.Expression.GetLocation());
                    break;
            }

            return ErrorExpression(reason: BoundExpressionReason.UnsupportedOperation);
        }

        var resultType = awaitable.GetResultMethod.ReturnType;
        if (resultType.SpecialType == SpecialType.System_Void)
            resultType = Compilation.GetSpecialType(SpecialType.System_Unit);

        return new BoundAwaitExpression(
            operand,
            resultType,
            awaitable.AwaiterType,
            awaitable.GetAwaiterMethod,
            awaitable.GetResultMethod,
            awaitable.IsCompletedProperty);
    }

    private bool IsAwaitExpressionAllowed()
    {
        static bool IsAsyncSymbol(ISymbol? symbol)
        {
            return symbol switch
            {
                ILambdaSymbol lambda => lambda.IsAsync,
                IMethodSymbol method => method.IsAsync,
                _ => false,
            };
        }

        for (Binder? current = this; current is not null; current = current.ParentBinder)
        {
            if (IsAsyncSymbol(current.ContainingSymbol))
                return true;

            if (current is BlockBinder block && IsAsyncSymbol(block.ContainingSymbol))
                return true;
        }

        return false;
    }

    private static bool IsImplicitReturnTarget(BlockStatementSyntax block, ExpressionStatementSyntax expressionStatement)
    {
        if (block.Statements.Count == 0 || block.Statements.LastOrDefault() != expressionStatement)
            return false;

        return block.Parent switch
        {
            BaseMethodDeclarationSyntax => true,
            FunctionStatementSyntax => true,
            AccessorDeclarationSyntax => true,
            _ => false,
        };
    }

    private BoundExpression BindAddressOfExpression(BoundExpression operand, UnaryExpressionSyntax syntax)
    {
        if (operand is BoundErrorExpression)
            return operand;

        switch (operand)
        {
            case BoundLocalAccess or BoundParameterAccess:
                return new BoundAddressOfExpression(operand);

            case BoundFieldAccess fieldAccess:
                return new BoundAddressOfExpression(fieldAccess);

            case BoundMemberAccessExpression { Member: IFieldSymbol } memberAccess:
                return new BoundAddressOfExpression(memberAccess);

            case BoundArrayAccessExpression arrayAccess:
                return new BoundAddressOfExpression(arrayAccess);

            case BoundSelfExpression selfExpression:
                return new BoundAddressOfExpression(selfExpression);
        }

        //_diagnostics.ReportInvalidAddressOf(syntax.Expression.GetLocation());
        return ErrorExpression(reason: BoundExpressionReason.ArgumentBindingFailed /*,.InvalidAddressOfTarget */);
    }

    private BoundExpression BindMissingExpression(ExpressionSyntax.Missing missing)
    {
        return ErrorExpression(reason: BoundExpressionReason.NotFound);
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

    private BoundExpression BindTypeOfExpression(TypeOfExpressionSyntax typeOfExpression)
    {
        var operandType = ResolveType(typeOfExpression.Type);

        if (operandType.ContainsErrorType())
            return ErrorExpression(operandType, reason: BoundExpressionReason.NotFound);

        var systemType = Compilation.GetSpecialType(SpecialType.System_Type);

        return new BoundTypeOfExpression(operandType, systemType);
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

            if (SymbolEqualityComparer.Default.Equals(delegateParameter.Type, methodParameter.Type))
                continue;

            var conversion = Compilation.ClassifyConversion(delegateParameter.Type, methodParameter.Type);
            if (!conversion.Exists || !conversion.IsImplicit)
                return false;
        }

        if (SymbolEqualityComparer.Default.Equals(method.ReturnType, invoke.ReturnType))
            return true;

        var returnConversion = Compilation.ClassifyConversion(method.ReturnType, invoke.ReturnType);
        return returnConversion.Exists && returnConversion.IsImplicit;
    }

    private static ImmutableArray<ISymbol> AsSymbolCandidates(ImmutableArray<IMethodSymbol> methods)
    {
        if (methods.IsDefaultOrEmpty)
            return ImmutableArray<ISymbol>.Empty;

        return ImmutableArray.CreateRange(methods, static m => (ISymbol)m);
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

            var pattern = BindPattern(arm.Pattern, scrutinee.Type);

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

    private BoundExpression BindTryExpression(TryExpressionSyntax tryExpression)
    {
        if (tryExpression.Expression is TryExpressionSyntax nestedTry)
        {
            _diagnostics.ReportTryExpressionCannotBeNested(nestedTry.TryKeyword.GetLocation());
            return ErrorExpression();
        }

        var expression = BindExpression(tryExpression.Expression);
        var exceptionType = Compilation.GetTypeByMetadataName("System.Exception") ?? Compilation.ErrorTypeSymbol;
        var expressionType = expression.Type ?? Compilation.ErrorTypeSymbol;
        var resultType = TypeSymbolNormalization.NormalizeUnion(new[] { expressionType, exceptionType });
        return new BoundTryExpression(expression, exceptionType, resultType);
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
            case BoundCasePattern casePattern:
                {
                    var scrutineeUnion = scrutineeType.TryGetDiscriminatedUnion()
                        ?? scrutineeType.TryGetDiscriminatedUnionCase()?.Union;

                    var caseUnion = UnwrapAlias(casePattern.CaseSymbol.Union);

                    if (scrutineeUnion is null || !SymbolEqualityComparer.Default.Equals(UnwrapAlias(scrutineeUnion), caseUnion))
                    {
                        var patternDisplay = $"for case '{casePattern.CaseSymbol.Name}'";
                        _diagnostics.ReportMatchExpressionArmPatternInvalid(
                            patternDisplay,
                            scrutineeType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                            patternSyntax.GetLocation());
                        return;
                    }

                    if (patternSyntax is CasePatternSyntax caseSyntax && caseSyntax.ArgumentList is { } argumentList)
                    {
                        var parameterTypes = casePattern.CaseSymbol.ConstructorParameters;
                        var elementCount = Math.Min(parameterTypes.Length, casePattern.Arguments.Length);
                        var argumentCount = Math.Min(argumentList.Arguments.Count, elementCount);

                        for (var i = 0; i < argumentCount; i++)
                        {
                            EnsureMatchArmPatternValid(
                                parameterTypes[i].Type,
                                argumentList.Arguments[i],
                                casePattern.Arguments[i]);
                        }
                    }

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
            case BoundTuplePattern tuplePattern:
                {
                    var tuplePatternType = UnwrapAlias(tuplePattern.Type);

                    if (tuplePatternType.TypeKind == TypeKind.Error)
                        return;

                    if (!PatternCanMatch(scrutineeType, tuplePatternType))
                    {
                        var patternDisplay = GetMatchPatternDisplay(tuplePatternType);
                        _diagnostics.ReportMatchExpressionArmPatternInvalid(
                            patternDisplay,
                            scrutineeType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                            patternSyntax.GetLocation());
                        return;
                    }

                    if (patternSyntax is TuplePatternSyntax tupleSyntax)
                    {
                        var elementTypes = GetTupleElementTypes(scrutineeType);

                        if (elementTypes.Length == 0)
                            elementTypes = GetTupleElementTypes(tuplePatternType);

                        var patternElements = tuplePattern.Elements;
                        var elementCount = Math.Min(patternElements.Length, tupleSyntax.Elements.Count);

                        for (var i = 0; i < elementCount; i++)
                        {
                            var elementType = elementTypes.Length > i
                                ? elementTypes[i]
                                : patternElements[i].Type ?? Compilation.ErrorTypeSymbol;

                            EnsureMatchArmPatternValid(elementType, tupleSyntax.Elements[i].Pattern, patternElements[i]);
                        }
                    }

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

        if (scrutineeType is ITypeUnionSymbol scrutineeUnion)
        {
            foreach (var member in scrutineeUnion.Types)
            {
                if (PatternCanMatch(member, patternType))
                    return true;
            }

            return false;
        }

        if (patternType is ITypeUnionSymbol patternUnion)
        {
            foreach (var member in patternUnion.Types)
            {
                if (PatternCanMatch(scrutineeType, member))
                    return true;
            }

            return false;
        }

        if (patternType.TryGetDiscriminatedUnionCase() is { } caseType)
        {
            var targetUnion = scrutineeType.TryGetDiscriminatedUnion()
                ?? scrutineeType.TryGetDiscriminatedUnionCase()?.Union;

            if (targetUnion is not null &&
                SymbolEqualityComparer.Default.Equals(UnwrapAlias(targetUnion), UnwrapAlias(caseType.Union)))
            {
                return true;
            }
        }

        return IsAssignable(patternType, scrutineeType, out _) ||
               IsAssignable(scrutineeType, patternType, out _);
    }

    private ImmutableArray<ITypeSymbol> GetTupleElementTypes(ITypeSymbol type)
    {
        type = UnwrapAlias(type);

        if (type is INamedTypeSymbol named)
        {
            var elements = named.TupleElements;
            if (!elements.IsDefaultOrEmpty)
                return elements.Select(e => e.Type).ToImmutableArray();
        }

        if (type is ITupleTypeSymbol tuple)
        {
            return tuple.TupleElements.Select(e => e.Type).ToImmutableArray();
        }

        return ImmutableArray<ITypeSymbol>.Empty;
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

        var catchAllIndex = GetCatchAllArmIndex(scrutineeType, arms);

        if (IsBooleanType(scrutineeType))
        {
            EnsureBooleanMatchExhaustive(matchExpression, arms, catchAllIndex);
            return;
        }

        var discriminatedUnion = scrutineeType.TryGetDiscriminatedUnion()
            ?? scrutineeType.TryGetDiscriminatedUnionCase()?.Union;

        if (discriminatedUnion is not null)
        {
            EnsureDiscriminatedUnionMatchExhaustive(matchExpression, arms, discriminatedUnion, catchAllIndex);
            return;
        }

        if (scrutineeType is not ITypeUnionSymbol union)
        {
            if (catchAllIndex >= 0)
                return;

            _diagnostics.ReportMatchExpressionNotExhaustive(
                "_",
                matchExpression.GetLocation());
            return;
        }

        var remaining = new HashSet<ITypeSymbol>(
            GetUnionMembers(union),
            SymbolEqualityComparer.Default);

        var literalCoverage = CreateLiteralCoverage(remaining);

        HashSet<ITypeSymbol>? guaranteedRemaining = null;
        Dictionary<ITypeSymbol, HashSet<object?>>? guaranteedLiteralCoverage = null;
        if (catchAllIndex >= 0)
        {
            guaranteedRemaining = new HashSet<ITypeSymbol>(remaining, SymbolEqualityComparer.Default);
            guaranteedLiteralCoverage = CloneLiteralCoverage(literalCoverage);
        }

        var reportedRedundantCatchAll = false;

        for (var i = 0; i < arms.Length; i++)
        {
            var arm = arms[i];
            var guardGuaranteesMatch = BoundNodeFacts.MatchArmGuardGuaranteesMatch(arm.Guard);

            if (guaranteedRemaining is not null && guardGuaranteesMatch && i < catchAllIndex)
                RemoveCoveredUnionMembers(guaranteedRemaining, arm.Pattern, guaranteedLiteralCoverage);

            if (guardGuaranteesMatch)
                RemoveCoveredUnionMembers(remaining, arm.Pattern, literalCoverage);

            if (remaining.Count == 0)
            {
                if (catchAllIndex >= 0 && !reportedRedundantCatchAll)
                {
                    if (i < catchAllIndex)
                    {
                        if (guaranteedRemaining is null || guaranteedRemaining.Count == 0)
                        {
                            ReportRedundantCatchAll(matchExpression, catchAllIndex);
                            reportedRedundantCatchAll = true;
                        }
                    }
                    else if (i == catchAllIndex && guaranteedRemaining is not null && guaranteedRemaining.Count == 0)
                    {
                        ReportRedundantCatchAll(matchExpression, catchAllIndex);
                        reportedRedundantCatchAll = true;
                    }
                }

                return;
            }

            if (catchAllIndex >= 0 && !reportedRedundantCatchAll && i == catchAllIndex && guaranteedRemaining is not null && guaranteedRemaining.Count == 0)
            {
                ReportRedundantCatchAll(matchExpression, catchAllIndex);
                reportedRedundantCatchAll = true;
            }
        }

        if (literalCoverage is not null && literalCoverage.Count > 0)
        {
            foreach (var (type, constants) in literalCoverage)
            {
                if (remaining.Contains(type))
                {
                    ReportMissingLiteralCoverage(matchExpression, type, constants);
                    return;
                }
            }
        }

        if (catchAllIndex >= 0)
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

    private int GetCatchAllArmIndex(ITypeSymbol scrutineeType, ImmutableArray<BoundMatchArm> arms)
    {
        for (var i = 0; i < arms.Length; i++)
        {
            var arm = arms[i];

            if (arm.Guard is not null)
                continue;

            if (IsCatchAllPattern(scrutineeType, arm.Pattern))
                return i;
        }

        return -1;
    }

    private void ReportRedundantCatchAll(MatchExpressionSyntax matchExpression, int catchAllIndex)
    {
        var patternLocation = matchExpression.Arms[catchAllIndex].Pattern.GetLocation();
        _diagnostics.ReportMatchExpressionCatchAllRedundant(patternLocation);
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
            case BoundTuplePattern tuplePattern:
                {
                    var elementTypes = GetTupleElementTypes(scrutineeType);

                    if (elementTypes.Length == 0 && tuplePattern.Elements.Length == 0)
                        return true;

                    if (elementTypes.Length != tuplePattern.Elements.Length)
                        return false;

                    for (var i = 0; i < tuplePattern.Elements.Length; i++)
                    {
                        if (!IsCatchAllPattern(elementTypes[i], tuplePattern.Elements[i]))
                            return false;
                    }

                    return true;
                }
        }

        return false;
    }

    private void EnsureDiscriminatedUnionMatchExhaustive(
        MatchExpressionSyntax matchExpression,
        ImmutableArray<BoundMatchArm> arms,
        IDiscriminatedUnionSymbol union,
        int catchAllIndex)
    {
        var remaining = new HashSet<IDiscriminatedUnionCaseSymbol>(union.Cases, SymbolEqualityComparer.Default);

        HashSet<IDiscriminatedUnionCaseSymbol>? guaranteedRemaining = null;
        if (catchAllIndex >= 0)
            guaranteedRemaining = new HashSet<IDiscriminatedUnionCaseSymbol>(remaining, SymbolEqualityComparer.Default);

        var reportedRedundantCatchAll = false;

        for (var i = 0; i < arms.Length; i++)
        {
            var arm = arms[i];
            var guardGuaranteesMatch = BoundNodeFacts.MatchArmGuardGuaranteesMatch(arm.Guard);

            if (guaranteedRemaining is not null && guardGuaranteesMatch && i < catchAllIndex)
                RemoveCoveredCases(guaranteedRemaining, arm.Pattern, union);

            if (guardGuaranteesMatch)
                RemoveCoveredCases(remaining, arm.Pattern, union);

            if (remaining.Count == 0)
            {
                if (catchAllIndex >= 0 && !reportedRedundantCatchAll)
                {
                    if (i < catchAllIndex)
                    {
                        if (guaranteedRemaining is null || guaranteedRemaining.Count == 0)
                        {
                            ReportRedundantCatchAll(matchExpression, catchAllIndex);
                            reportedRedundantCatchAll = true;
                        }
                    }
                    else if (i == catchAllIndex && guaranteedRemaining is not null && guaranteedRemaining.Count == 0)
                    {
                        ReportRedundantCatchAll(matchExpression, catchAllIndex);
                        reportedRedundantCatchAll = true;
                    }
                }

                return;
            }

            if (catchAllIndex >= 0 && !reportedRedundantCatchAll && i == catchAllIndex && guaranteedRemaining is not null && guaranteedRemaining.Count == 0)
            {
                ReportRedundantCatchAll(matchExpression, catchAllIndex);
                reportedRedundantCatchAll = true;
            }
        }

        var missingCase = remaining.FirstOrDefault();

        if (missingCase is not null)
        {
            _diagnostics.ReportMatchExpressionNotExhaustive(
                missingCase.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                matchExpression.GetLocation());
        }
    }

    private void RemoveCoveredUnionMembers(
        HashSet<ITypeSymbol> remaining,
        BoundPattern pattern,
        Dictionary<ITypeSymbol, HashSet<object?>>? literalCoverage = null)
    {
        switch (pattern)
        {
            case BoundDiscardPattern:
                remaining.Clear();
                literalCoverage?.Clear();
                break;
            case BoundDeclarationPattern declaration:
                RemoveMembersAssignableToPattern(remaining, declaration.DeclaredType, literalCoverage);
                break;
            case BoundConstantPattern constant:
                {
                    var literalType = (LiteralTypeSymbol)UnwrapAlias(constant.LiteralType);

                    if (TryUpdateLiteralCoverage(remaining, literalCoverage, literalType))
                        break;

                    if (constant.ConstantValue is null)
                    {
                        foreach (var candidate in remaining.ToArray())
                        {
                            var candidateType = UnwrapAlias(candidate);

                            if (candidateType.TypeKind == TypeKind.Null)
                            {
                                remaining.Remove(candidate);
                                literalCoverage?.Remove(candidate);
                            }
                        }

                        break;
                    }

                    foreach (var candidate in remaining.ToArray())
                    {
                        var candidateType = UnwrapAlias(candidate);

                        if (SymbolEqualityComparer.Default.Equals(candidateType, literalType))
                        {
                            remaining.Remove(candidate);
                            literalCoverage?.Remove(candidate);
                        }
                    }

                    break;
                }
            case BoundOrPattern orPattern:
                RemoveCoveredUnionMembers(remaining, orPattern.Left, literalCoverage);
                RemoveCoveredUnionMembers(remaining, orPattern.Right, literalCoverage);
                break;
            case BoundTuplePattern tuplePattern:
                RemoveMembersAssignableToPattern(remaining, tuplePattern.Type, literalCoverage);
                break;
        }
    }

    private void RemoveCoveredCases(
        HashSet<IDiscriminatedUnionCaseSymbol> remaining,
        BoundPattern pattern,
        IDiscriminatedUnionSymbol union)
    {
        switch (pattern)
        {
            case BoundDiscardPattern:
                remaining.Clear();
                break;
            case BoundDeclarationPattern declaration:
                {
                    var declaredType = UnwrapAlias(declaration.DeclaredType);

                    if (declaredType.SpecialType == SpecialType.System_Object ||
                        SymbolEqualityComparer.Default.Equals(declaredType, UnwrapAlias((ITypeSymbol)union)))
                    {
                        remaining.Clear();
                        break;
                    }

                    var declarationUnion = declaredType.TryGetDiscriminatedUnion()
                        ?? declaredType.TryGetDiscriminatedUnionCase()?.Union;

                    if (declarationUnion is not null &&
                        SymbolEqualityComparer.Default.Equals(UnwrapAlias(declarationUnion), UnwrapAlias(union)))
                    {
                        remaining.Clear();
                    }

                    break;
                }
            case BoundCasePattern casePattern:
                if (SymbolEqualityComparer.Default.Equals(UnwrapAlias(casePattern.CaseSymbol.Union), UnwrapAlias(union)))
                    remaining.Remove(casePattern.CaseSymbol);
                break;
            case BoundOrPattern orPattern:
                RemoveCoveredCases(remaining, orPattern.Left, union);
                RemoveCoveredCases(remaining, orPattern.Right, union);
                break;
        }
    }

    private void RemoveMembersAssignableToPattern(
        HashSet<ITypeSymbol> remaining,
        ITypeSymbol patternType,
        Dictionary<ITypeSymbol, HashSet<object?>>? literalCoverage = null)
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
                literalCoverage?.Remove(candidate);
                continue;
            }

            if (IsAssignable(patternType, candidateType, out _))
            {
                remaining.Remove(candidate);
                literalCoverage?.Remove(candidate);
                continue;
            }

            if (patternType is ITypeUnionSymbol patternUnion &&
                candidateType is ITypeUnionSymbol candidateUnion &&
                TypeCoverageHelper.UnionIsCoveredByTypes(patternUnion, candidateUnion.Types))
            {
                remaining.Remove(candidate);
                literalCoverage?.Remove(candidate);
            }
        }
    }

    private static IEnumerable<ITypeSymbol> GetUnionMembers(ITypeUnionSymbol union)
    {
        foreach (var member in union.Types)
        {
            if (member is ITypeUnionSymbol nested)
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

    private static bool IsBooleanType(ITypeSymbol type)
    {
        type = UnwrapAlias(type);

        if (type.SpecialType == SpecialType.System_Boolean)
            return true;

        if (type is LiteralTypeSymbol literal)
            return IsBooleanType(literal.UnderlyingType);

        return false;
    }

    private void EnsureBooleanMatchExhaustive(
        MatchExpressionSyntax matchExpression,
        ImmutableArray<BoundMatchArm> arms,
        int catchAllIndex)
    {
        if (catchAllIndex >= 0)
            return;

        var remaining = BooleanCoverage.All;

        for (var i = 0; i < arms.Length && remaining != BooleanCoverage.None; i++)
        {
            var arm = arms[i];

            if (arm.Guard is not null)
                continue;

            var covered = GetBooleanCoverage(arm.Pattern);
            remaining &= ~covered;
        }

        if (remaining == BooleanCoverage.None)
            return;

        if ((remaining & BooleanCoverage.True) != 0)
            _diagnostics.ReportMatchExpressionNotExhaustive("true", matchExpression.GetLocation());

        if ((remaining & BooleanCoverage.False) != 0)
            _diagnostics.ReportMatchExpressionNotExhaustive("false", matchExpression.GetLocation());
    }

    private BooleanCoverage GetBooleanCoverage(BoundPattern pattern)
    {
        switch (pattern)
        {
            case BoundDiscardPattern:
                return BooleanCoverage.All;
            case BoundDeclarationPattern declaration when IsBooleanType(declaration.DeclaredType):
                return BooleanCoverage.All;
            case BoundConstantPattern { ConstantValue: bool value }:
                return value ? BooleanCoverage.True : BooleanCoverage.False;
            case BoundOrPattern orPattern:
                return GetBooleanCoverage(orPattern.Left) | GetBooleanCoverage(orPattern.Right);
            case BoundAndPattern andPattern:
                return GetBooleanCoverage(andPattern.Left) & GetBooleanCoverage(andPattern.Right);
            case BoundNotPattern notPattern:
                return BooleanCoverage.All & ~GetBooleanCoverage(notPattern.Pattern);
            default:
                return BooleanCoverage.None;
        }
    }

    private static Dictionary<ITypeSymbol, HashSet<object?>>? CreateLiteralCoverage(IEnumerable<ITypeSymbol> members)
    {
        Dictionary<ITypeSymbol, HashSet<object?>>? coverage = null;

        foreach (var member in members)
        {
            var type = UnwrapAlias(member);

            if (TypeCoverageHelper.RequiresLiteralCoverage(type))
            {
                coverage ??= new Dictionary<ITypeSymbol, HashSet<object?>>(SymbolEqualityComparer.Default);
                coverage[member] = new HashSet<object?>();
            }
        }

        return coverage;
    }

    private static Dictionary<ITypeSymbol, HashSet<object?>>? CloneLiteralCoverage(Dictionary<ITypeSymbol, HashSet<object?>>? coverage)
    {
        if (coverage is null)
            return null;

        var clone = new Dictionary<ITypeSymbol, HashSet<object?>>(SymbolEqualityComparer.Default);

        foreach (var (type, constants) in coverage)
            clone[type] = new HashSet<object?>(constants);

        return clone;
    }

    private static bool TryUpdateLiteralCoverage(
        HashSet<ITypeSymbol> remaining,
        Dictionary<ITypeSymbol, HashSet<object?>>? literalCoverage,
        LiteralTypeSymbol literal)
    {
        if (literalCoverage is null || literalCoverage.Count == 0)
            return false;

        var updated = false;

        foreach (var entry in literalCoverage.ToArray())
        {
            var candidate = entry.Key;

            if (!remaining.Contains(candidate))
            {
                literalCoverage.Remove(candidate);
                continue;
            }

            var targetType = UnwrapAlias(candidate);

            if (!TypeCoverageHelper.LiteralBelongsToType(literal, targetType))
                continue;

            updated = true;

            var constants = entry.Value;
            constants.Add(literal.ConstantValue);

            if (TypeCoverageHelper.LiteralsCoverType(targetType, constants))
            {
                remaining.Remove(candidate);
                literalCoverage.Remove(candidate);
            }
        }

        return updated;
    }

    private void ReportMissingLiteralCoverage(
        MatchExpressionSyntax matchExpression,
        ITypeSymbol type,
        HashSet<object?> constants)
    {
        var targetType = UnwrapAlias(type);

        if (targetType.SpecialType == SpecialType.System_Boolean)
        {
            if (!constants.Contains(true))
                _diagnostics.ReportMatchExpressionNotExhaustive("true", matchExpression.GetLocation());

            if (!constants.Contains(false))
                _diagnostics.ReportMatchExpressionNotExhaustive("false", matchExpression.GetLocation());
        }
    }

    [Flags]
    private enum BooleanCoverage : byte
    {
        None = 0,
        False = 1,
        True = 2,
        All = False | True,
    }

    private BoundExpression BindIfExpression(IfExpressionSyntax ifExpression)
    {
        var condition = BindExpression(ifExpression.Condition);

        var boolType = Compilation.GetSpecialType(SpecialType.System_Boolean);
        var conversion = Compilation.ClassifyConversion(condition.Type, boolType);
        if (!conversion.Exists)
        {
            _diagnostics.ReportCannotConvertFromTypeToType(condition.Type, boolType, ifExpression.Condition.GetLocation());
        }

        var thenBinder = SemanticModel.GetBinder(ifExpression, this);
        var thenExpr = thenBinder is BlockBinder bb
            ? bb.BindExpression(ifExpression.Expression, _allowReturnsInExpression)
            : thenBinder.BindExpression(ifExpression.Expression);

        if (ifExpression.ElseClause is null)
        {
            _diagnostics.ReportIfExpressionRequiresElse(ifExpression.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.OtherError);
        }

        var elseBinder = SemanticModel.GetBinder(ifExpression.ElseClause, this);
        var elseExpr = elseBinder is BlockBinder ebb
            ? ebb.BindExpression(ifExpression.ElseClause.Expression, _allowReturnsInExpression)
            : elseBinder.BindExpression(ifExpression.ElseClause.Expression);

        return new BoundIfExpression(condition, thenExpr, elseExpr);
    }

    private BoundExpression BindMemberAccessExpression(MemberAccessExpressionSyntax memberAccess)
    {
        // Binding for explicit receiver
        var receiver = BindExpression(memberAccess.Expression);

        if (IsErrorExpression(receiver))
            return receiver is BoundErrorExpression boundError
                ? boundError
                : new BoundErrorExpression(receiver.Type ?? Compilation.ErrorTypeSymbol, null, BoundExpressionReason.OtherError);

        if (receiver.Type is { } boundReceiverType && boundReceiverType.ContainsErrorType())
            return new BoundErrorExpression(boundReceiverType, receiver.Symbol, BoundExpressionReason.OtherError);

        var simpleName = memberAccess.Name;
        if (simpleName.Identifier.IsMissing)
        {
            _diagnostics.ReportIdentifierExpected(simpleName.Identifier.GetLocation());
            _diagnostics.ReportTheNameDoesNotExistInTheCurrentContext(string.Empty, simpleName.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.NotFound);
        }

        var name = simpleName.Identifier.ValueText;
        ImmutableArray<ITypeSymbol>? explicitTypeArguments = null;
        GenericNameSyntax? genericTypeSyntax = null;

        if (simpleName is GenericNameSyntax genericName)
        {
            var boundTypeArguments = TryBindTypeArguments(genericName);
            if (boundTypeArguments is null)
                return ErrorExpression(reason: BoundExpressionReason.TypeMismatch);

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
            return ErrorExpression(reason: BoundExpressionReason.NotFound);
        }

        if (receiver is BoundTypeExpression typeExpr)
        {
            var nonMethodMember = new SymbolQuery(name, typeExpr.Type, IsStatic: true)
                .Lookup(this)
                .FirstOrDefault(static m => m is not IMethodSymbol);

            if (nonMethodMember is not null)
            {
                if (!EnsureMemberAccessible(nonMethodMember, memberAccess.Name.GetLocation(), GetSymbolKindForDiagnostic(nonMethodMember)))
                    return ErrorExpression(reason: BoundExpressionReason.Inaccessible);

                if (nonMethodMember is ITypeSymbol typeMember)
                {
                    if (explicitTypeArguments is { } typeArgs && genericTypeSyntax is not null && typeMember is INamedTypeSymbol namedMember)
                    {
                        if (!ValidateTypeArgumentConstraints(namedMember, typeArgs, i => GetTypeArgumentLocation(genericTypeSyntax.TypeArgumentList.Arguments, genericTypeSyntax.GetLocation(), i), namedMember.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)))
                            return ErrorExpression(reason: BoundExpressionReason.TypeMismatch);

                        if (TryConstructGeneric(namedMember, typeArgs, namedMember.Arity) is INamedTypeSymbol constructedType)
                            typeMember = constructedType;
                    }

                    return new BoundTypeExpression(typeMember);
                }

                return new BoundMemberAccessExpression(typeExpr, nonMethodMember);
            }

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
                if (TryBindDiscriminatedUnionCase(typeExpr.Type, name, memberAccess.Name.GetLocation()) is BoundExpression unionCase)
                    return unionCase;

                var typeName = typeExpr.Symbol!.Name;
                _diagnostics.ReportMemberDoesNotContainDefinition(typeName, memberAccess.Name.ToString(), memberAccess.Name.GetLocation());
                return ErrorExpression(reason: BoundExpressionReason.NotFound);
            }

            if (!EnsureMemberAccessible(member, memberAccess.Name.GetLocation(), GetSymbolKindForDiagnostic(member)))
                return ErrorExpression(reason: BoundExpressionReason.Inaccessible);

            if (member is ITypeSymbol typeMemberSymbol)
            {
                if (BindDiscriminatedUnionCaseType(typeMemberSymbol) is { } unionCase)
                    return unionCase;

                return new BoundTypeExpression(typeMemberSymbol);
            }

            return new BoundMemberAccessExpression(typeExpr, member);
        }

        if (receiver.Type is not null)
        {
            var receiverType = receiver.Type.UnwrapLiteralType() ?? receiver.Type;

            var nonMethodMember = new SymbolQuery(name, receiverType, IsStatic: false)
                .Lookup(this)
                .FirstOrDefault(static m => m is not IMethodSymbol);

            if (nonMethodMember is not null)
            {
                if (!EnsureMemberAccessible(nonMethodMember, nameLocation, GetSymbolKindForDiagnostic(nonMethodMember)))
                    return ErrorExpression(reason: BoundExpressionReason.Inaccessible);

                return new BoundMemberAccessExpression(receiver, nonMethodMember);
            }

            var methodCandidates = ImmutableArray<IMethodSymbol>.Empty;

            var instanceMethods = new SymbolQuery(name, receiverType, IsStatic: false)
                .LookupMethods(this)
                .ToImmutableArray();

            if (!instanceMethods.IsDefaultOrEmpty)
                methodCandidates = instanceMethods;

            if (IsExtensionReceiver(receiver))
            {
                var extensionMethods = LookupExtensionMethods(name, receiverType)
                    .ToImmutableArray();

                if (!extensionMethods.IsDefaultOrEmpty)
                {
                    methodCandidates = methodCandidates.IsDefaultOrEmpty
                        ? extensionMethods
                        : methodCandidates.AddRange(extensionMethods);
                }
            }

            if (!methodCandidates.IsDefaultOrEmpty)
            {
                if (explicitTypeArguments is { } typeArgs && genericTypeSyntax is not null)
                {
                    var instantiated = InstantiateMethodCandidates(methodCandidates, typeArgs, genericTypeSyntax, memberAccess.Name.GetLocation());
                    if (!instantiated.IsDefaultOrEmpty)
                        return BindMethodGroup(receiver, instantiated, nameLocation);
                }
                else
                {
                    return BindMethodGroup(receiver, methodCandidates, nameLocation);
                }
            }

            var instanceMember = new SymbolQuery(name, receiverType, IsStatic: false)
                .Lookup(this)
                .FirstOrDefault();

            if (instanceMember is not null)
            {
                if (!EnsureMemberAccessible(instanceMember, nameLocation, GetSymbolKindForDiagnostic(instanceMember)))
                    return ErrorExpression(reason: BoundExpressionReason.Inaccessible);

                return new BoundMemberAccessExpression(receiver, instanceMember);
            }

            if (IsExtensionReceiver(receiver))
            {
                var extensionProperties = LookupExtensionProperties(name, receiverType).ToImmutableArray();

                if (!extensionProperties.IsDefaultOrEmpty)
                {
                    var accessibleProperties = GetAccessibleProperties(extensionProperties, nameLocation);

                    if (!accessibleProperties.IsDefaultOrEmpty)
                    {
                        if (accessibleProperties.Length == 1)
                            return new BoundMemberAccessExpression(receiver, accessibleProperties[0]);

                        var ambiguousMethods = accessibleProperties
                            .Select(p => p.GetMethod ?? p.SetMethod)
                            .Where(m => m is not null)
                            .Cast<IMethodSymbol>()
                            .ToImmutableArray();

                        if (!ambiguousMethods.IsDefaultOrEmpty)
                            _diagnostics.ReportCallIsAmbiguous(name, ambiguousMethods, nameLocation);

                        return ErrorExpression(
                            reason: BoundExpressionReason.Ambiguous,
                            candidates: AsSymbolCandidates(ambiguousMethods));
                    }

                    EnsureMemberAccessible(extensionProperties[0], nameLocation, GetSymbolKindForDiagnostic(extensionProperties[0]));
                    return ErrorExpression(reason: BoundExpressionReason.Inaccessible);
                }
            }
        }

        _diagnostics.ReportTheNameDoesNotExistInTheCurrentContext(name, nameLocation);
        return ErrorExpression(reason: BoundExpressionReason.NotFound);
    }

    private BoundExpression BindMemberBindingExpression(MemberBindingExpressionSyntax memberBinding)
    {
        var simpleName = memberBinding.Name;
        var memberName = simpleName.Identifier.ValueText;
        ImmutableArray<ITypeSymbol>? explicitTypeArguments = null;
        GenericNameSyntax? genericTypeSyntax = null;

        if (simpleName is GenericNameSyntax genericName)
        {
            var typeArgs = TryBindTypeArguments(genericName);
            if (typeArgs is null)
                return ErrorExpression(reason: BoundExpressionReason.TypeMismatch);

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
                if (TryBindDiscriminatedUnionCase(expectedType, memberName, nameLocation) is BoundExpression unionCase)
                    return unionCase;

                _diagnostics.ReportTheNameDoesNotExistInTheCurrentContext(memberName, nameLocation);
                return ErrorExpression(reason: BoundExpressionReason.NotFound);
            }

            if (!EnsureMemberAccessible(member, nameLocation, GetSymbolKindForDiagnostic(member)))
                return ErrorExpression(reason: BoundExpressionReason.Inaccessible);

            if (member is ITypeSymbol typeMember)
            {
                if (BindDiscriminatedUnionCaseType(typeMember) is { } unionCase)
                    return unionCase;

                return new BoundTypeExpression(typeMember);
            }

            return new BoundMemberAccessExpression(new BoundTypeExpression(expectedType), member);
        }

        _diagnostics.ReportMemberAccessRequiresTargetType(memberName, nameLocation);
        return ErrorExpression(reason: BoundExpressionReason.NotFound);
    }

    private BoundExpression? TryBindDiscriminatedUnionCase(ITypeSymbol? receiverType, string memberName, Location location)
    {
        var targetType = receiverType?.UnwrapLiteralType() ?? receiverType;
        if (targetType is not INamedTypeSymbol namedType)
            return null;

        if (namedType.TryGetDiscriminatedUnion() is null)
            return null;

        foreach (var member in namedType.GetMembers(memberName))
        {
            if (member is not ITypeSymbol typeMember)
                continue;

            if (typeMember.TryGetDiscriminatedUnionCase() is null)
                continue;

            var accessibleType = EnsureTypeAccessible(typeMember, location);
            if (accessibleType.TypeKind == TypeKind.Error)
                return ErrorExpression(reason: BoundExpressionReason.Inaccessible);

            return BindDiscriminatedUnionCaseType(accessibleType);
        }

        return null;
    }

    private BoundExpression? BindDiscriminatedUnionCaseType(ITypeSymbol typeMember)
    {
        var isUnionCase = typeMember.TryGetDiscriminatedUnionCase() is not null;

        if (!isUnionCase &&
            typeMember is INamedTypeSymbol namedType &&
            namedType.ContainingType?.TryGetDiscriminatedUnion() is not null)
        {
            isUnionCase = true;
        }

        if (!isUnionCase && typeMember.DeclaringSyntaxReferences.Any(static r => r.GetSyntax() is Raven.CodeAnalysis.Syntax.UnionCaseClauseSyntax))
        {
            isUnionCase = true;
        }

        if (!isUnionCase)
            return null;

        if (typeMember is INamedTypeSymbol caseType)
        {
            var parameterlessCtor = caseType.Constructors.FirstOrDefault(static ctor => ctor.Parameters.Length == 0);

            if (parameterlessCtor is not null)
                return new BoundObjectCreationExpression(parameterlessCtor, ImmutableArray<BoundExpression>.Empty);
        }

        return new BoundTypeExpression(typeMember);
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

                case AssignmentExpressionSyntax assign when assign.Right == node && assign.Left is ExpressionSyntax leftExpr:
                    var left = BindExpression(leftExpr);
                    return left.Type;

                case AssignmentStatementSyntax assign when assign.Right.Contains(node) && assign.Left is ExpressionSyntax leftStmtExpr:
                    var leftStmt = BindExpression(leftStmtExpr);
                    return leftStmt.Type;

                case ReturnStatementSyntax returnStmt:
                    if (_containingSymbol is IMethodSymbol method)
                        return GetReturnTargetType(method);

                    break;

                case BinaryExpressionSyntax binary when binary.Left == node:
                    if (GetTargetType(binary) is { } binaryTargetLeft)
                        return binaryTargetLeft;

                    return BindExpression(binary.Right).Type;

                case BinaryExpressionSyntax binary when binary.Right == node:
                    if (GetTargetType(binary) is { } binaryTargetRight)
                        return binaryTargetRight;

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

                            var argumentCount = argList.Arguments.Count;
                            IMethodSymbol? targetMethod = null;
                            var targetUsesImplicitExtension = false;
                            var argumentExpression = arg.Expression;

                            if (invocation.Expression is MemberAccessExpressionSyntax memberAccess)
                            {
                                var boundMember = BindMemberAccessExpression(memberAccess);
                                if (boundMember is BoundMethodGroupExpression methodGroup)
                                {
                                    var extensionReceiverImplicit = methodGroup.Receiver is not null && IsExtensionReceiver(methodGroup.Receiver);
                                    var methods = methodGroup.Methods;

                                    if (!methods.IsDefaultOrEmpty)
                                    {
                                        methods = methods
                                            .Where(m => AreArgumentsCompatibleWithMethod(m, argumentCount, methodGroup.Receiver))
                                            .ToImmutableArray();
                                    }

                                    if (methods.IsDefaultOrEmpty)
                                    {
                                        current = current.Parent;
                                        continue;
                                    }

                                    methods = FilterMethodsForLambda(methods, index, argumentExpression, extensionReceiverImplicit);
                                    RecordLambdaTargets(argumentExpression, methods, index, extensionReceiverImplicit);
                                    if (methods.Length == 1)
                                    {
                                        targetMethod = methods[0];
                                        targetUsesImplicitExtension = extensionReceiverImplicit && targetMethod.IsExtensionMethod;
                                    }
                                    else if (!methods.IsDefaultOrEmpty && TryGetCommonParameterType(methods, index, extensionReceiverImplicit) is ITypeSymbol common)
                                        return common;
                                }
                                else if (boundMember is BoundMemberAccessExpression { Receiver: var receiver, Member: IMethodSymbol m })
                                {
                                    var extensionReceiverImplicit = receiver is not null && IsExtensionReceiver(receiver);
                                    if (AreArgumentsCompatibleWithMethod(m, argumentCount, receiver))
                                    {
                                        targetMethod = m;
                                        targetUsesImplicitExtension = extensionReceiverImplicit && m.IsExtensionMethod;
                                        RecordLambdaTargets(argumentExpression, ImmutableArray.Create(m), index, extensionReceiverImplicit);
                                        if (TryGetLambdaParameter(m, index, extensionReceiverImplicit, out var parameter))
                                            return parameter.Type;
                                    }
                                }
                            }
                            else if (invocation.Expression is MemberBindingExpressionSyntax memberBinding)
                            {
                                var boundMember = BindMemberBindingExpression(memberBinding);
                                if (boundMember is BoundMethodGroupExpression methodGroup)
                                {
                                    var extensionReceiverImplicit = methodGroup.Receiver is not null && IsExtensionReceiver(methodGroup.Receiver);
                                    var methods = methodGroup.Methods;

                                    if (!methods.IsDefaultOrEmpty)
                                    {
                                        methods = methods
                                            .Where(m => AreArgumentsCompatibleWithMethod(m, argumentCount, methodGroup.Receiver))
                                            .ToImmutableArray();
                                    }

                                    if (methods.IsDefaultOrEmpty)
                                    {
                                        current = current.Parent;
                                        continue;
                                    }

                                    methods = FilterMethodsForLambda(methods, index, argumentExpression, extensionReceiverImplicit);
                                    RecordLambdaTargets(argumentExpression, methods, index, extensionReceiverImplicit);
                                    if (methods.Length == 1)
                                    {
                                        targetMethod = methods[0];
                                        targetUsesImplicitExtension = extensionReceiverImplicit && targetMethod.IsExtensionMethod;
                                    }
                                    else if (!methods.IsDefaultOrEmpty && TryGetCommonParameterType(methods, index, extensionReceiverImplicit) is ITypeSymbol common)
                                        return common;
                                }
                                else if (boundMember is BoundMemberAccessExpression { Receiver: var receiver, Member: IMethodSymbol m })
                                {
                                    var extensionReceiverImplicit = receiver is not null && IsExtensionReceiver(receiver);
                                    if (AreArgumentsCompatibleWithMethod(m, argumentCount, receiver))
                                    {
                                        targetMethod = m;
                                        targetUsesImplicitExtension = extensionReceiverImplicit && m.IsExtensionMethod;
                                        RecordLambdaTargets(argumentExpression, ImmutableArray.Create(m), index, extensionReceiverImplicit);
                                        if (TryGetLambdaParameter(m, index, extensionReceiverImplicit, out var parameter))
                                            return parameter.Type;
                                    }
                                }
                            }
                            else if (invocation.Expression is IdentifierNameSyntax id)
                            {
                                var candidates = new SymbolQuery(id.Identifier.ValueText)
                                    .LookupMethods(this)
                                    .ToImmutableArray();
                                var accessible = GetAccessibleMethods(candidates, id.Identifier.GetLocation(), reportIfInaccessible: false);

                                if (!accessible.IsDefaultOrEmpty)
                                {
                                    accessible = accessible
                                        .Where(m => AreArgumentsCompatibleWithMethod(m, argumentCount, receiver: null))
                                        .ToImmutableArray();

                                    if (accessible.IsDefaultOrEmpty)
                                    {
                                        current = current.Parent;
                                        continue;
                                    }

                                    accessible = FilterMethodsForLambda(accessible, index, argumentExpression, extensionReceiverImplicit: false);
                                }

                                RecordLambdaTargets(argumentExpression, accessible, index, extensionReceiverImplicit: false);

                                if (accessible.Length == 1)
                                {
                                    targetMethod = accessible[0];
                                    targetUsesImplicitExtension = false;
                                }
                                else if (!accessible.IsDefaultOrEmpty)
                                {
                                    if (TryGetCommonParameterType(accessible, index, extensionReceiverImplicit: false) is ITypeSymbol common)
                                        return common;
                                }
                            }

                            if (targetMethod is not null && targetMethod.Parameters.Length > index)
                            {
                                RecordLambdaTargets(argumentExpression, ImmutableArray.Create(targetMethod), index, targetUsesImplicitExtension);
                                if (TryGetLambdaParameter(targetMethod, index, targetUsesImplicitExtension, out var parameter))
                                    return parameter.Type;
                            }
                        }

                        current = current.Parent;
                        continue;
                    }

                case ExpressionStatementSyntax expressionStatement:
                    if (expressionStatement.Expression == node &&
                        expressionStatement.Parent is BlockStatementSyntax block &&
                        IsImplicitReturnTarget(block, expressionStatement) &&
                        _containingSymbol is IMethodSymbol methodSymbol)
                    {
                        return GetReturnTargetType(methodSymbol);
                    }

                    return null;

                default:
                    current = current.Parent;
                    continue;
            }

            break;
        }

        return null;
    }

    private static ITypeSymbol GetReturnTargetType(IMethodSymbol method)
    {
        if (method is SourceMethodSymbol { HasAsyncReturnTypeError: true } or SourceLambdaSymbol { HasAsyncReturnTypeError: true })
            return method.ReturnType;

        var returnType = method.ReturnType;

        if (returnType is ErrorTypeSymbol)
        {
            return returnType;
        }

        if (method.IsAsync &&
            returnType is INamedTypeSymbol namedReturn &&
            namedReturn.OriginalDefinition.SpecialType == SpecialType.System_Threading_Tasks_Task_T &&
            namedReturn.TypeArguments.Length == 1 &&
            namedReturn.TypeArguments[0] is { } resultType)
        {
            return resultType;
        }

        return returnType;
    }

    protected BoundExpression BindTypeSyntax(TypeSyntax syntax)
    {
        if (syntax is NullTypeSyntax)
        {
            return new BoundTypeExpression(Compilation.NullTypeSymbol);
        }

        if (syntax is LiteralTypeSyntax literalType)
        {
            var token = literalType.Token;
            var value = token.Value ?? token.ValueText!;

            if (value is string stringValue)
            {
                if (literalType.Kind == SyntaxKind.TrueLiteralType)
                {
                    value = true;
                }
                else if (literalType.Kind == SyntaxKind.FalseLiteralType)
                {
                    value = false;
                }
                else
                {
                    value = stringValue;
                }
            }

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

        if (syntax is PointerTypeSyntax pointerTypeSyntax)
        {
            if (BindTypeSyntax(pointerTypeSyntax.ElementType) is not BoundTypeExpression elementTypeExpression)
                return ErrorExpression(reason: BoundExpressionReason.TypeMismatch);

            var elementType = elementTypeExpression.Type;

            if (elementType.ContainsErrorType())
                return new BoundTypeExpression(elementType);

            var pointerType = Compilation.CreatePointerTypeSymbol(elementType);
            return new BoundTypeExpression(pointerType);
        }

        if (syntax is ArrayTypeSyntax arrayTypeSyntax)
        {
            if (BindTypeSyntax(arrayTypeSyntax.ElementType) is not BoundTypeExpression elementTypeExpression)
                return ErrorExpression(reason: BoundExpressionReason.TypeMismatch);

            var elementType = elementTypeExpression.Type;

            if (elementType.ContainsErrorType())
                return new BoundTypeExpression(elementType);

            foreach (var rankSpecifier in arrayTypeSyntax.RankSpecifiers)
            {
                var rank = rankSpecifier.CommaTokens.Count + 1;
                elementType = Compilation.CreateArrayTypeSymbol(elementType, rank);
            }

            return new BoundTypeExpression(elementType);
        }

        if (syntax is TupleTypeSyntax tupleTypeSyntax)
        {
            var boundElements = new List<(string? name, ITypeSymbol type)>();

            foreach (var element in tupleTypeSyntax.Elements)
            {
                if (BindTypeSyntax(element.Type) is not BoundTypeExpression bt)
                    return ErrorExpression(reason: BoundExpressionReason.TypeMismatch);

                boundElements.Add((element.NameColon?.Name.ToString(), bt.Type));
            }

            var tupleType = Compilation.CreateTupleTypeSymbol(boundElements);

            return new BoundTypeExpression(tupleType);
        }

        if (syntax is IdentifierNameSyntax id)
        {
            return BindTypeName(id.Identifier.ValueText, id.GetLocation(), []);
        }

        if (syntax is GenericNameSyntax generic)
        {
            if (!TryBindTypeArguments(generic, out var typeArgs, out var requestedArity))
                return ErrorExpression(reason: BoundExpressionReason.TypeMismatch);

            return BindTypeName(
                generic.Identifier.ValueText,
                generic.GetLocation(),
                typeArgs,
                generic.TypeArgumentList.Arguments,
                requestedArity);
        }

        if (syntax is QualifiedNameSyntax qualified)
        {
            var left = BindTypeSyntax(qualified.Left);

            if (left is not BoundNamespaceExpression ns && left is not BoundTypeExpression leftType)
                return ErrorExpression(reason: BoundExpressionReason.NotFound);

            string name;
            ImmutableArray<ITypeSymbol> typeArgs = [];

            GenericNameSyntax? rightGeneric = null;
            int requestedArity = 0;

            if (qualified.Right is IdentifierNameSyntax id2)
            {
                name = id2.Identifier.ValueText;
            }
            else if (qualified.Right is GenericNameSyntax generic2)
            {
                name = generic2.Identifier.ValueText;
                rightGeneric = generic2;
                if (!TryBindTypeArguments(generic2, out typeArgs, out requestedArity))
                    return ErrorExpression(reason: BoundExpressionReason.TypeMismatch);
            }
            else
            {
                return ErrorExpression(reason: BoundExpressionReason.NotFound);
            }

            if (rightGeneric is null)
                requestedArity = typeArgs.Length;

            ISymbol? member = left switch
            {
                BoundNamespaceExpression nsExpr => nsExpr.Namespace.GetMembers(name)
                    .FirstOrDefault(s => s is INamespaceSymbol ||
                        (s is INamedTypeSymbol type && type.Arity == requestedArity)),
                BoundTypeExpression typeExpr => typeExpr.Type.GetMembers(name)
                    .OfType<INamedTypeSymbol>()
                    .FirstOrDefault(m => m.Arity == requestedArity),
                _ => null
            };

            if (member is INamespaceSymbol nsResult)
                return new BoundNamespaceExpression(nsResult);

            if (member is INamedTypeSymbol namedType)
            {
                if (namedType.Arity != requestedArity)
                    return ErrorExpression(reason: BoundExpressionReason.TypeMismatch);

                if (!typeArgs.IsEmpty && rightGeneric is not null && !ValidateTypeArgumentConstraints(namedType, typeArgs, i => GetTypeArgumentLocation(rightGeneric.TypeArgumentList.Arguments, rightGeneric.GetLocation(), i), namedType.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)))
                    return ErrorExpression(reason: BoundExpressionReason.TypeMismatch);

                var constructed = typeArgs.IsEmpty
                    ? namedType
                    : TryConstructGeneric(namedType, typeArgs, namedType.Arity) ?? namedType;

                if (!typeArgs.IsEmpty &&
                    constructed is ConstructedNamedTypeSymbol constructedNamed &&
                    constructedNamed.TypeArguments.Any(static argument => argument is ITypeParameterSymbol))
                {
                    var fallback = constructedNamed.Construct(typeArgs.ToArray());
                    if (fallback is INamedTypeSymbol substituted)
                        constructed = substituted;
                }

                return new BoundTypeExpression(constructed);
            }

            if (member is null && left is BoundNamespaceExpression namespaceExpression && typeArgs.IsEmpty)
            {
                var metadataName = namespaceExpression.Namespace.QualifyName(name);
                if (!string.IsNullOrEmpty(metadataName))
                {
                    if (requestedArity > 0)
                        metadataName = string.Concat(metadataName, "`", requestedArity);

                    var metadataType = Compilation.GetTypeByMetadataName(metadataName);
                    if (metadataType is not null && metadataType.TypeKind != TypeKind.Error)
                        return new BoundTypeExpression(metadataType);
                }
            }

            return ErrorExpression(reason: BoundExpressionReason.NotFound);
        }

        return ErrorExpression(reason: BoundExpressionReason.NotFound);
    }

    private BoundExpression BindCompoundAssignment(
        ExpressionSyntax leftSyntax,
        ExpressionSyntax rightSyntax,
        SyntaxNode node,
        SyntaxKind operatorTokenKind)
    {
        var binaryOperator = GetBinaryOperatorFromAssignment(operatorTokenKind);
        if (binaryOperator is null)
            return ErrorExpression(reason: BoundExpressionReason.UnsupportedOperation);

        if (leftSyntax is DiscardExpressionSyntax)
        {
            _diagnostics.ReportLeftOfAssignmentMustBeAVariablePropertyOrIndexer(node.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.UnsupportedOperation);
        }

        if (leftSyntax is ElementAccessExpressionSyntax elementAccess)
        {
            var right = BindExpression(rightSyntax);

            if (IsErrorExpression(right))
                return AsErrorExpression(right);

            var receiver = BindExpression(elementAccess.Expression);
            var args = elementAccess.ArgumentList.Arguments.Select(x => BindExpression(x.Expression)).ToArray();

            if (IsErrorExpression(receiver))
                return receiver is BoundErrorExpression boundError
                    ? boundError
                    : new BoundErrorExpression(
                        receiver.Type ?? Compilation.ErrorTypeSymbol,
                        null,
                        BoundExpressionReason.OtherError);

            var firstErrorArg = args.FirstOrDefault(IsErrorExpression);
            if (firstErrorArg is not null)
                return firstErrorArg is BoundErrorExpression errorArg
                    ? errorArg
                    : new BoundErrorExpression(
                        firstErrorArg.Type ?? Compilation.ErrorTypeSymbol,
                        null,
                        BoundExpressionReason.OtherError);

            if (receiver.Type is IArrayTypeSymbol arrayType)
            {
                var arrayAccess = new BoundArrayAccessExpression(receiver, args, arrayType.ElementType);
                var arrayRight = BindCompoundAssignmentValue(arrayAccess, right, arrayType.ElementType, binaryOperator.Value, rightSyntax);
                return BoundFactory.CreateArrayAssignmentExpression(arrayAccess, arrayRight);
            }

            var indexer = ResolveIndexer(receiver.Type!, args.Length);

            if (indexer is null || indexer.SetMethod is null)
            {
                _diagnostics.ReportLeftOfAssignmentMustBeAVariablePropertyOrIndexer(node.GetLocation());
                return new BoundErrorExpression(receiver.Type!, null, BoundExpressionReason.NotFound);
            }

            var indexerAccess = new BoundIndexerAccessExpression(receiver, args, indexer);
            var indexerRight = BindCompoundAssignmentValue(indexerAccess, right, indexer.Type, binaryOperator.Value, rightSyntax);
            return BoundFactory.CreateIndexerAssignmentExpression(indexerAccess, indexerRight);
        }

        var left = BindExpression(leftSyntax);

        if (IsErrorExpression(left))
            return AsErrorExpression(left);

        if (left is BoundLocalAccess localAccess)
        {
            var localSymbol = localAccess.Local;
            var localType = localSymbol.Type;
            var right = BindExpression(rightSyntax);

            if (IsErrorExpression(right))
                return AsErrorExpression(right);

            if (localType is ByRefTypeSymbol byRefLocalType)
            {
                var localByRefRight = BindCompoundAssignmentValue(localAccess, right, byRefLocalType.ElementType, binaryOperator.Value, rightSyntax);
                if (localByRefRight is BoundErrorExpression)
                    return localByRefRight;

                return BoundFactory.CreateByRefAssignmentExpression(localAccess, byRefLocalType.ElementType, localByRefRight);
            }

            if (!localSymbol.IsMutable)
            {
                _diagnostics.ReportThisValueIsNotMutable(leftSyntax.GetLocation());
                return ErrorExpression(reason: BoundExpressionReason.NotFound);
            }

            var localRight = BindCompoundAssignmentValue(localAccess, right, localType, binaryOperator.Value, rightSyntax);
            return BoundFactory.CreateLocalAssignmentExpression(localSymbol, localRight);
        }
        else if (left is BoundParameterAccess parameterAccess)
        {
            var parameterSymbol = parameterAccess.Parameter;
            var parameterType = parameterSymbol.Type;
            var right = BindExpression(rightSyntax);

            if (IsErrorExpression(right))
                return AsErrorExpression(right);

            if (!parameterSymbol.IsMutable)
            {
                _diagnostics.ReportThisValueIsNotMutable(leftSyntax.GetLocation());
                return ErrorExpression(reason: BoundExpressionReason.NotFound);
            }

            if (parameterType is ByRefTypeSymbol byRefParameterType && parameterSymbol.RefKind is RefKind.Ref or RefKind.Out)
            {
                var parameterByRefRight = BindCompoundAssignmentValue(parameterAccess, right, byRefParameterType.ElementType, binaryOperator.Value, rightSyntax);
                if (parameterByRefRight is BoundErrorExpression)
                    return parameterByRefRight;

                return BoundFactory.CreateByRefAssignmentExpression(parameterAccess, byRefParameterType.ElementType, parameterByRefRight);
            }

            var parameterRight = BindCompoundAssignmentValue(parameterAccess, right, parameterType, binaryOperator.Value, rightSyntax);
            return BoundFactory.CreateParameterAssignmentExpression(parameterSymbol, parameterRight);
        }
        else if (left.Symbol is IFieldSymbol fieldSymbol)
        {
            if (fieldSymbol.IsConst)
            {
                _diagnostics.ReportThisValueIsNotMutable(leftSyntax.GetLocation());
                return new BoundErrorExpression(fieldSymbol.Type, null, BoundExpressionReason.NotFound);
            }

            var receiver = GetReceiver(left);

            var right = BindExpression(rightSyntax);

            if (IsErrorExpression(right))
                return AsErrorExpression(right);

            if (!CanAssignToField(fieldSymbol, receiver, leftSyntax))
                return new BoundErrorExpression(fieldSymbol.Type, fieldSymbol, BoundExpressionReason.NotFound);

            var access = new BoundFieldAccess(receiver, fieldSymbol);

            var fieldRight = BindCompoundAssignmentValue(access, right, fieldSymbol.Type, binaryOperator.Value, rightSyntax);
            return CreateFieldAssignmentExpression(receiver, fieldSymbol, fieldRight);
        }
        else if (left.Symbol is IPropertySymbol propertySymbol)
        {
            SourceFieldSymbol? backingField = null;

            var receiver = GetReceiver(left);

            if (propertySymbol.SetMethod is null)
            {
                if (!TryGetWritableAutoPropertyBackingField(propertySymbol, left, out backingField))
                {
                    _diagnostics.ReportPropertyOrIndexerCannotBeAssignedIsReadOnly(propertySymbol.Name, leftSyntax.GetLocation());
                    return ErrorExpression(reason: BoundExpressionReason.NotFound);
                }
            }

            var right = BindExpression(rightSyntax);

            if (IsErrorExpression(right))
                return AsErrorExpression(right);

            if (backingField is not null)
            {
                var backingAccess = new BoundFieldAccess(receiver, backingField);
                if (!CanAssignToField(backingField, receiver, leftSyntax))
                    return new BoundErrorExpression(backingField.Type, backingField, BoundExpressionReason.NotFound);
                var backingRight = BindCompoundAssignmentValue(backingAccess, right, propertySymbol.Type, binaryOperator.Value, rightSyntax);
                return CreateFieldAssignmentExpression(receiver, backingField, backingRight);
            }

            var propertyAccess = new BoundMemberAccessExpression(receiver, propertySymbol);
            var result = BindCompoundAssignmentValue(propertyAccess, right, propertySymbol.Type, binaryOperator.Value, rightSyntax);
            return BoundFactory.CreatePropertyAssignmentExpression(receiver, propertySymbol, result);
        }

        return ErrorExpression(reason: BoundExpressionReason.NotFound);
    }

    private BoundExpression BindTypeName(string name, Location location, ImmutableArray<ITypeSymbol> typeArguments, SeparatedSyntaxList<TypeArgumentSyntax> typeArgumentSyntax = default, int? arityOverride = null)
    {
        var symbol = LookupType(name);

        var requestedArity = arityOverride ?? typeArguments.Length;

        if (symbol is null && typeArguments.IsEmpty)
        {
            var namespaceSymbol = LookupNamespace(name);
            if (namespaceSymbol is not null)
                return new BoundNamespaceExpression(namespaceSymbol);
        }

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
                    return ErrorExpression(reason: BoundExpressionReason.TypeMismatch);
                }

                return new BoundTypeExpression(named);
            }

            var definition = NormalizeDefinition(named);

            if (definition.Arity != requestedArity)
            {
                var match = FindAccessibleNamedType(name, requestedArity);
                if (match is null)
                    return ErrorExpression(reason: BoundExpressionReason.TypeMismatch);

                definition = match;
            }

            if (typeArguments.IsEmpty)
                return new BoundTypeExpression(definition);

            if (!ValidateTypeArgumentConstraints(definition, typeArguments, i => GetTypeArgumentLocation(typeArgumentSyntax, location, i), definition.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)))
                return ErrorExpression(reason: BoundExpressionReason.TypeMismatch);

            var constructed = TryConstructGeneric(definition, typeArguments, definition.Arity) ?? definition;
            return new BoundTypeExpression(constructed);
        }

        var alternate = FindAccessibleNamedType(name, requestedArity);
        if (alternate is not null)
        {
            if (typeArguments.IsEmpty)
                return new BoundTypeExpression(alternate);

            if (!ValidateTypeArgumentConstraints(alternate, typeArguments, i => GetTypeArgumentLocation(typeArgumentSyntax, location, i), alternate.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)))
                return ErrorExpression(reason: BoundExpressionReason.TypeMismatch);

            var constructed = TryConstructGeneric(alternate, typeArguments, alternate.Arity) ?? alternate;
            return new BoundTypeExpression(constructed);
        }

        _diagnostics.ReportTheNameDoesNotExistInTheCurrentContext(name, location);
        return ErrorExpression(reason: BoundExpressionReason.NotFound);
    }

    private bool TryBindTypeArguments(GenericNameSyntax generic, out ImmutableArray<ITypeSymbol> boundArguments, out int requestedArity)
    {
        requestedArity = GetTypeArgumentArity(generic.TypeArgumentList);

        var argumentSyntax = generic.TypeArgumentList.Arguments;
        if (argumentSyntax.Count == 0)
        {
            boundArguments = ImmutableArray<ITypeSymbol>.Empty;
            return true;
        }

        var builder = ImmutableArray.CreateBuilder<ITypeSymbol>(argumentSyntax.Count);
        var hasExplicitArguments = false;
        var hasOmittedArguments = false;

        foreach (var argument in argumentSyntax)
        {
            if (argument.Type.IsMissing)
            {
                hasOmittedArguments = true;
                continue;
            }

            hasExplicitArguments = true;

            if (BindTypeSyntax(argument.Type) is not BoundTypeExpression bt)
            {
                boundArguments = ImmutableArray<ITypeSymbol>.Empty;
                return false;
            }

            builder.Add(bt.Type);
        }

        if (hasExplicitArguments && hasOmittedArguments)
        {
            boundArguments = ImmutableArray<ITypeSymbol>.Empty;
            return false;
        }

        boundArguments = builder.ToImmutable();
        if (hasExplicitArguments && boundArguments.Length != requestedArity)
            return false;

        return true;
    }

    private static int GetTypeArgumentArity(TypeArgumentListSyntax typeArgumentList)
    {
        var arguments = typeArgumentList.Arguments;
        var argumentCount = arguments.Count;
        var separatorCount = arguments.SeparatorCount;

        if (argumentCount == 0)
        {
            // `<>` represents an open generic with a single type parameter.
            return separatorCount == 0 ? 1 : separatorCount + 1;
        }

        // When omitted type arguments are present (e.g., `<,>`), the separator count
        // reflects the intended arity even though some argument nodes may be missing.
        var inferredArity = separatorCount + 1;
        return Math.Max(argumentCount, inferredArity);
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

            return BoundFactory.NullLiteral(convertedType);
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

    private BoundExpression BindDefaultExpression(DefaultExpressionSyntax syntax)
    {
        if (syntax.Type is TypeSyntax explicitType)
        {
            var type = ResolveType(explicitType);
            type = EnsureTypeAccessible(type, explicitType.GetLocation());

            return new BoundDefaultValueExpression(type);
        }

        var targetType = GetTargetType(syntax);

        if (targetType is null)
        {
            _diagnostics.ReportDefaultLiteralRequiresTargetType(syntax.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.TypeMismatch);
        }

        if (targetType.TypeKind == TypeKind.Error)
            return ErrorExpression(reason: BoundExpressionReason.TypeMismatch);

        return new BoundDefaultValueExpression(targetType);
    }

    private BoundExpression BindUnitExpression(UnitExpressionSyntax syntax)
    {
        return BoundFactory.UnitExpression();
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
                    text.Token.ValueText ?? string.Empty,
                    Compilation.GetSpecialType(SpecialType.System_String)),
                InterpolationSyntax interpolation => BindExpression(interpolation.Expression),
                _ => throw new InvalidOperationException("Unknown interpolated string content")
            };

            if (result is null)
            {
                result = expr;
                continue;
            }

            if (result is BoundErrorExpression)
                continue;

            if (expr is BoundErrorExpression)
            {
                result = expr;
                continue;
            }

            if (IsErrorOrNull(result) || IsErrorOrNull(expr))
            {
                result = ErrorExpression(reason: BoundExpressionReason.OtherError);
                continue;
            }

            var concatMethod = ResolveStringConcatMethod(result, expr);
            if (concatMethod is null)
            {
                result = ErrorExpression(reason: BoundExpressionReason.OtherError);
                continue;
            }

            result = new BoundInvocationExpression(concatMethod, [result, expr]);
        }

        return result ?? new BoundLiteralExpression(
            BoundLiteralExpressionKind.StringLiteral,
            string.Empty,
            Compilation.GetSpecialType(SpecialType.System_String));
    }

    private static bool IsErrorOrNull(BoundExpression expression)
    {
        return expression.Type is null || expression.Type.TypeKind == TypeKind.Error;
    }

    private BoundExpression BindIdentifierName(IdentifierNameSyntax syntax)
    {
        var name = syntax.Identifier.ValueText;
        var symbol = LookupSymbol(name);

        if (symbol is null)
        {
            var type = LookupType(name);
            if (type is not null)
                return new BoundTypeExpression(type);

            var ns = LookupNamespace(name);
            if (ns is not null)
                return new BoundNamespaceExpression(ns);

            _diagnostics.ReportTheNameDoesNotExistInTheCurrentContext(name, syntax.Identifier.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.NotFound);
        }

        if (symbol is IMethodSymbol)
        {
            var methods = LookupSymbols(name)
                .OfType<IMethodSymbol>()
                .ToImmutableArray();

            if (methods.IsDefaultOrEmpty)
                return ErrorExpression(reason: BoundExpressionReason.NotFound);

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
                        return ErrorExpression(reason: BoundExpressionReason.Inaccessible);

                    return new BoundFieldAccess(field);
                }
            case IPropertySymbol prop:
                {
                    if (!EnsureMemberAccessible(prop, syntax.Identifier.GetLocation(), GetSymbolKindForDiagnostic(prop)))
                        return ErrorExpression(reason: BoundExpressionReason.Inaccessible);

                    return new BoundPropertyAccess(prop);
                }
            default:
                return ErrorExpression(reason: BoundExpressionReason.NotFound);
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

        return BoundFactory.MethodGroupExpression(receiver, methods, delegateFactory);
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

        return ErrorExpression(reason: BoundExpressionReason.NotFound);
    }

    private BoundExpression BindBinaryExpression(BinaryExpressionSyntax syntax)
    {
        var left = BindExpression(syntax.Left);
        var opKind = syntax.OperatorToken.Kind;

        if (opKind == SyntaxKind.PipeToken)
            return BindPipeExpression(left, syntax);

        var right = BindExpression(syntax.Right);

        return BindBinaryExpression(opKind, left, right, syntax.OperatorToken.GetLocation());
    }

    private BoundExpression BindBinaryExpression(
        SyntaxKind opKind,
        BoundExpression left,
        BoundExpression right,
        Location? diagnosticLocation = null)
    {
        if (left is BoundErrorExpression || right is BoundErrorExpression)
            return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.ArgumentBindingFailed);

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
                if (IsErrorOrNull(left) || IsErrorOrNull(right))
                    return ErrorExpression(reason: BoundExpressionReason.OtherError);

                var concatMethod = ResolveStringConcatMethod(left, right);
                if (concatMethod is not null)
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
        var operatorText = SyntaxFacts.GetSyntaxTokenText(opKind) ?? opKind.ToString();
        _diagnostics.ReportOperatorCannotBeAppliedToOperandsOfTypes(
            operatorText,
            left.Type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
            right.Type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
            diagnosticLocation ?? Location.None);

        return ErrorExpression(reason: BoundExpressionReason.NotFound);
    }

    private BoundExpression BindPipeExpression(BoundExpression left, BinaryExpressionSyntax syntax)
    {
        if (left is BoundErrorExpression)
            return left;

        if (syntax.Right is InvocationExpressionSyntax invocation)
        {
            var boundArguments = BindInvocationArguments(invocation.ArgumentList.Arguments, out var hasErrors);
            if (hasErrors)
                return ErrorExpression(reason: BoundExpressionReason.ArgumentBindingFailed);

            var target = BindPipelineTargetExpression(invocation.Expression);

            if (target is BoundErrorExpression error)
                return error;

            if (target is BoundMethodGroupExpression methodGroup)
                return BindPipelineInvocationOnMethodGroup(methodGroup, invocation, syntax.Left, left, boundArguments);

            if (target is BoundMemberAccessExpression { Member: IMethodSymbol } memberExpr)
                return BindPipelineInvocationOnBoundMethod(memberExpr, invocation, syntax.Left, left, boundArguments);

            if (BindPipelineInvocationOnDelegate(target, invocation, syntax.Left, left, boundArguments) is { } delegateInvocation)
                return delegateInvocation;

            _diagnostics.ReportInvalidInvocation(invocation.Expression.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.NotFound);
        }

        var propertyTarget = BindPipelineTargetExpression(syntax.Right);

        if (propertyTarget is BoundErrorExpression propertyError)
            return propertyError;

        if (propertyTarget is BoundMemberAccessExpression { Member: IPropertySymbol } or BoundPropertyAccess)
            return BindPipelinePropertyAssignment(propertyTarget, syntax.Left, left, syntax.Right);

        _diagnostics.ReportPipeRequiresInvocation(syntax.OperatorToken.GetLocation());
        return ErrorExpression(reason: BoundExpressionReason.NotFound);
    }

    private BoundExpression BindPipelineTargetExpression(ExpressionSyntax expression)
        => expression switch
        {
            MemberAccessExpressionSyntax memberAccess => BindMemberAccessExpression(memberAccess),
            MemberBindingExpressionSyntax memberBinding => BindMemberBindingExpression(memberBinding),
            GenericNameSyntax generic => BindGenericInvocationTarget(generic),
            IdentifierNameSyntax identifier => BindIdentifierName(identifier),
            _ => BindExpression(expression),
        };

    private BoundExpression BindPipelinePropertyAssignment(
        BoundExpression target,
        ExpressionSyntax pipelineSyntax,
        BoundExpression pipelineValue,
        ExpressionSyntax propertySyntax)
    {
        IPropertySymbol? propertySymbol = target switch
        {
            BoundMemberAccessExpression { Member: IPropertySymbol property } => property,
            BoundPropertyAccess propertyAccess => propertyAccess.Property,
            _ => null,
        };

        if (propertySymbol is null)
        {
            _diagnostics.ReportPipeRequiresInvocation(propertySyntax.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.NotFound);
        }

        SourceFieldSymbol? backingField = null;

        if (propertySymbol.SetMethod is null &&
            !TryGetWritableAutoPropertyBackingField(propertySymbol, target, out backingField))
        {
            _diagnostics.ReportPropertyOrIndexerCannotBeAssignedIsReadOnly(propertySymbol.Name, propertySyntax.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.NotFound);
        }

        if (pipelineValue.Type is { } pipelineType &&
            propertySymbol.Type.TypeKind != TypeKind.Error &&
            ShouldAttemptConversion(pipelineValue))
        {
            if (!IsAssignable(propertySymbol.Type, pipelineType, out var conversion))
            {
                _diagnostics.ReportCannotAssignFromTypeToType(
                    pipelineType.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    propertySymbol.Type.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    pipelineSyntax.GetLocation());
                return new BoundErrorExpression(propertySymbol.Type, null, BoundExpressionReason.TypeMismatch);
            }

            pipelineValue = ApplyConversion(pipelineValue, propertySymbol.Type, conversion, pipelineSyntax);
        }

        var receiver = GetReceiver(target);

        if (backingField is not null)
        {
            if (!CanAssignToField(backingField, receiver, pipelineSyntax))
                return new BoundErrorExpression(backingField.Type, backingField, BoundExpressionReason.NotFound);

            return CreateFieldAssignmentExpression(receiver, backingField, pipelineValue);
        }

        return BoundFactory.CreatePropertyAssignmentExpression(receiver, propertySymbol, pipelineValue);
    }

    private BoundExpression BindGenericInvocationTarget(GenericNameSyntax generic)
    {
        var boundTypeArguments = TryBindTypeArguments(generic);
        if (boundTypeArguments is null)
            return ErrorExpression(reason: BoundExpressionReason.TypeMismatch);

        var symbolCandidates = LookupSymbols(generic.Identifier.ValueText)
            .OfType<IMethodSymbol>()
            .ToImmutableArray();

        if (!symbolCandidates.IsDefaultOrEmpty)
        {
            var instantiated = InstantiateMethodCandidates(symbolCandidates, boundTypeArguments.Value, generic, generic.GetLocation());
            if (!instantiated.IsDefaultOrEmpty)
                return CreateMethodGroup(null, instantiated);
        }

        return BindTypeSyntax(generic);
    }

    private BoundExpression BindPipelineInvocationOnMethodGroup(
        BoundMethodGroupExpression methodGroup,
        InvocationExpressionSyntax invocation,
        ExpressionSyntax pipelineSyntax,
        BoundExpression pipelineValue,
        BoundArgument[] boundArguments)
    {
        var methodName = methodGroup.Methods[0].Name;
        var extensionCandidates = methodGroup.Methods
            .Where(static m => m.IsExtensionMethod)
            .ToImmutableArray();
        var staticCandidates = methodGroup.Methods
            .Where(static m => m.IsStatic && !m.IsExtensionMethod)
            .ToImmutableArray();

        if (!extensionCandidates.IsDefaultOrEmpty && IsExtensionReceiver(pipelineValue))
        {
            var resolution = OverloadResolver.ResolveOverload(extensionCandidates, boundArguments, Compilation, pipelineValue, EnsureLambdaCompatible, invocation);
            if (resolution.Success)
            {
                var method = resolution.Method!;
                var converted = ConvertInvocationArguments(
                    method,
                    boundArguments,
                    pipelineValue,
                    pipelineSyntax,
                    out var convertedExtensionReceiver);
                return new BoundInvocationExpression(method, converted, methodGroup.Receiver, convertedExtensionReceiver);
            }

            if (resolution.IsAmbiguous)
            {
                _diagnostics.ReportCallIsAmbiguous(methodName, resolution.AmbiguousCandidates, invocation.GetLocation());
                return ErrorExpression(
                    reason: BoundExpressionReason.Ambiguous,
                    candidates: AsSymbolCandidates(resolution.AmbiguousCandidates));
            }
        }

        if (!staticCandidates.IsDefaultOrEmpty)
        {
            var totalArguments = new BoundArgument[boundArguments.Length + 1];
            totalArguments[0] = new BoundArgument(pipelineValue, RefKind.None, name: null, pipelineSyntax);
            Array.Copy(boundArguments, 0, totalArguments, 1, boundArguments.Length);

            var resolution = OverloadResolver.ResolveOverload(staticCandidates, totalArguments, Compilation, canBindLambda: EnsureLambdaCompatible, callSyntax: invocation);
            if (resolution.Success)
            {
                var method = resolution.Method!;
                var convertedArguments = ConvertPipelineStaticInvocationArguments(method, pipelineValue, pipelineSyntax, boundArguments, invocation);
                return new BoundInvocationExpression(method, convertedArguments, methodGroup.Receiver);
            }

            if (resolution.IsAmbiguous)
            {
                _diagnostics.ReportCallIsAmbiguous(methodName, resolution.AmbiguousCandidates, invocation.GetLocation());
                return ErrorExpression(
                    reason: BoundExpressionReason.Ambiguous,
                    candidates: AsSymbolCandidates(resolution.AmbiguousCandidates));
            }

            ReportSuppressedLambdaDiagnostics(totalArguments);
            _diagnostics.ReportNoOverloadForMethod(methodName, totalArguments.Length, invocation.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.OverloadResolutionFailed);
        }

        ReportSuppressedLambdaDiagnostics(PrependPipelineArgument(pipelineValue, pipelineSyntax, boundArguments));
        _diagnostics.ReportNoOverloadForMethod(methodName, boundArguments.Length + 1, invocation.GetLocation());
        return ErrorExpression(reason: BoundExpressionReason.OverloadResolutionFailed);
    }

    private BoundExpression BindPipelineInvocationOnBoundMethod(
        BoundMemberAccessExpression memberExpr,
        InvocationExpressionSyntax invocation,
        ExpressionSyntax pipelineSyntax,
        BoundExpression pipelineValue,
        BoundArgument[] boundArguments)
    {
        if (memberExpr.Member is not IMethodSymbol method)
        {
            _diagnostics.ReportInvalidInvocation(invocation.Expression.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.NotFound);
        }

        if (method.IsExtensionMethod && IsExtensionReceiver(pipelineValue))
        {
            var converted = ConvertInvocationArguments(
                method,
                boundArguments,
                pipelineValue,
                pipelineSyntax,
                out var convertedExtensionReceiver);
            return new BoundInvocationExpression(method, converted, memberExpr.Receiver, convertedExtensionReceiver);
        }

        if (!method.IsStatic)
        {
            _diagnostics.ReportInvalidInvocation(invocation.Expression.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.NotFound);
        }

        var totalCount = boundArguments.Length + 1;
        if (!SupportsArgumentCount(method.Parameters, totalCount))
        {
            ReportSuppressedLambdaDiagnostics(PrependPipelineArgument(pipelineValue, pipelineSyntax, boundArguments));
            _diagnostics.ReportNoOverloadForMethod(method.Name, totalCount, invocation.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.OverloadResolutionFailed);
        }

        var convertedArguments = ConvertPipelineStaticInvocationArguments(method, pipelineValue, pipelineSyntax, boundArguments, invocation);
        return new BoundInvocationExpression(method, convertedArguments, memberExpr.Receiver);
    }

    private BoundExpression? BindPipelineInvocationOnDelegate(
        BoundExpression target,
        InvocationExpressionSyntax invocation,
        ExpressionSyntax pipelineSyntax,
        BoundExpression pipelineValue,
        BoundArgument[] boundArguments)
    {
        var rawType = target.Type;
        var targetType = rawType.UnwrapLiteralType() ?? rawType;

        if (targetType is not INamedTypeSymbol { TypeKind: TypeKind.Delegate } delegateType)
            return null;

        var invokeMethod = delegateType.GetDelegateInvokeMethod();
        if (invokeMethod is null)
        {
            _diagnostics.ReportInvalidInvocation(invocation.Expression.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.NotFound);
        }

        if (!EnsureMemberAccessible(invokeMethod, invocation.Expression.GetLocation(), GetSymbolKindForDiagnostic(invokeMethod)))
            return ErrorExpression(reason: BoundExpressionReason.Inaccessible);

        var totalCount = boundArguments.Length + 1;
        if (!SupportsArgumentCount(invokeMethod.Parameters, totalCount))
        {
            ReportSuppressedLambdaDiagnostics(PrependPipelineArgument(pipelineValue, pipelineSyntax, boundArguments));
            _diagnostics.ReportNoOverloadForMethod(invokeMethod.Name, totalCount, invocation.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.OverloadResolutionFailed);
        }

        var convertedArguments = ConvertPipelineStaticInvocationArguments(invokeMethod, pipelineValue, pipelineSyntax, boundArguments, invocation);
        return new BoundInvocationExpression(invokeMethod, convertedArguments, target);
    }

    private BoundExpression[] ConvertPipelineStaticInvocationArguments(
        IMethodSymbol method,
        BoundExpression pipelineValue,
        ExpressionSyntax pipelineSyntax,
        BoundArgument[] remainingArguments,
        InvocationExpressionSyntax invocation)
    {
        var parameters = method.Parameters;
        var converted = new BoundExpression[parameters.Length];

        if (parameters.Length == 0)
            return converted;

        converted[0] = ConvertSingleArgument(pipelineValue, parameters[0], pipelineSyntax);

        if (parameters.Length == 1)
            return converted;

        var rest = ConvertArguments(parameters.RemoveAt(0), remainingArguments);
        Array.Copy(rest, 0, converted, 1, rest.Length);
        for (int i = 1 + rest.Length; i < converted.Length; i++)
            converted[i] = CreateOptionalArgument(parameters[i]);

        return converted;
    }

    private static IEnumerable<BoundArgument> PrependPipelineArgument(
        BoundExpression pipelineValue,
        ExpressionSyntax pipelineSyntax,
        IEnumerable<BoundArgument> remainingArguments)
    {
        yield return new BoundArgument(pipelineValue, RefKind.None, null, pipelineSyntax);

        foreach (var argument in remainingArguments)
            yield return argument;
    }

    private IMethodSymbol? ResolveStringConcatMethod(BoundExpression left, BoundExpression right)
    {
        var stringType = Compilation.GetSpecialType(SpecialType.System_String);
        var candidates = stringType.GetMembers("Concat").OfType<IMethodSymbol>();

        var resolution = OverloadResolver.ResolveOverload(
            candidates.ToArray(),
            new[]
            {
                new BoundArgument(left, RefKind.None, null),
                new BoundArgument(right, RefKind.None, null)
            },
            Compilation,
            canBindLambda: EnsureLambdaCompatible);

        if (resolution.Success && resolution.Method is not null)
            return resolution.Method;

        IMethodSymbol? fallback = null;
        var bestScore = int.MaxValue;

        foreach (var candidate in candidates)
        {
            if (candidate.Parameters.Length != 2)
                continue;

            if (left.Type is null || right.Type is null)
                continue;

            var leftConversion = Compilation.ClassifyConversion(left.Type, candidate.Parameters[0].Type);
            var rightConversion = Compilation.ClassifyConversion(right.Type, candidate.Parameters[1].Type);

            if (!leftConversion.IsImplicit || !rightConversion.IsImplicit)
                continue;

            var score = GetConversionScore(leftConversion) + GetConversionScore(rightConversion);

            if (score >= bestScore)
                continue;

            fallback = candidate;
            bestScore = score;
        }

        return fallback;

        static int GetConversionScore(Conversion conversion)
        {
            if (!conversion.Exists)
                return int.MaxValue;

            if (conversion.IsIdentity)
                return 0;

            if (conversion.IsReference)
                return 1;

            if (conversion.IsBoxing)
                return 2;

            if (conversion.IsNumeric)
                return 3;

            if (conversion.IsUserDefined)
                return 4;

            if (conversion.IsUnboxing)
                return 5;

            return 10;
        }
    }

    private IMethodSymbol? ResolveUserDefinedOperator(SyntaxKind opKind, ITypeSymbol leftType, ITypeSymbol rightType)
    {
        static ITypeSymbol UnwrapLiteral(ITypeSymbol type) =>
            type is LiteralTypeSymbol literal ? literal.UnderlyingType : type;

        leftType = UnwrapLiteral(leftType);
        rightType = UnwrapLiteral(rightType);

        if (leftType.SpecialType is SpecialType.System_Int32 or SpecialType.System_Int64 or SpecialType.System_String or SpecialType.System_Boolean ||
            rightType.SpecialType is SpecialType.System_Int32 or SpecialType.System_Int64 or SpecialType.System_String or SpecialType.System_Boolean)
        {
            return null;
        }

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

            if (IsErrorExpression(boundMember))
                return boundMember is BoundErrorExpression boundError
                    ? boundError
                    : new BoundErrorExpression(boundMember.Type ?? Compilation.ErrorTypeSymbol, null, BoundExpressionReason.OtherError);

            if (boundMember is BoundMethodGroupExpression methodGroup)
                return BindInvocationOnMethodGroup(methodGroup, syntax);

            if (boundMember is BoundMemberAccessExpression { Member: IMethodSymbol method } memberExpr)
            {
                var argExprs = new List<BoundArgument>();
                bool argErrors = false;
                foreach (var arg in syntax.ArgumentList.Arguments)
                {
                    var boundArg = BindExpression(arg.Expression);
                    if (HasExpressionErrors(boundArg))
                        argErrors = true;
                    var name = arg.NameColon?.Name.Identifier.ValueText;
                    if (string.IsNullOrEmpty(name))
                        name = null;
                    argExprs.Add(new BoundArgument(boundArg, RefKind.None, name, arg));
                }

                if (argErrors)
                {
                    var candidates = ImmutableArray.Create(method);
                    return ErrorExpression(
                        method.ReturnType,
                        method,
                        BoundExpressionReason.ArgumentBindingFailed,
                        AsSymbolCandidates(candidates));
                }

                var argArray = argExprs.ToArray();

                if (AreArgumentsCompatibleWithMethod(method, argArray.Length, memberExpr.Receiver, argArray))
                {
                    var convertedArgs = ConvertArguments(method.Parameters, argArray);
                    return new BoundInvocationExpression(method, convertedArgs, memberExpr.Receiver);
                }

                receiver = memberExpr.Receiver;
                methodName = method.Name;
            }
            else if (boundMember is BoundTypeExpression { Type: INamedTypeSymbol namedType })
            {
                var argExprs = new List<BoundArgument>();
                bool argErrors = false;
                foreach (var arg in syntax.ArgumentList.Arguments)
                {
                    var boundArg = BindExpression(arg.Expression);
                    if (HasExpressionErrors(boundArg))
                        argErrors = true;
                    var name = arg.NameColon?.Name.Identifier.ValueText;
                    if (string.IsNullOrEmpty(name))
                        name = null;
                    argExprs.Add(new BoundArgument(boundArg, RefKind.None, name, arg));
                }

                if (argErrors)
                {
                    var constructors = namedType.Constructors.Where(static ctor => !ctor.IsStatic).ToImmutableArray();
                    var symbol = constructors.IsDefaultOrEmpty ? (ISymbol)namedType : constructors[0];
                    return ErrorExpression(
                        namedType,
                        symbol,
                        BoundExpressionReason.ArgumentBindingFailed,
                        AsSymbolCandidates(constructors));
                }

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

            if (IsErrorExpression(boundMember))
                return boundMember is BoundErrorExpression boundError
                    ? boundError
                    : new BoundErrorExpression(boundMember.Type ?? Compilation.ErrorTypeSymbol, null, BoundExpressionReason.OtherError);

            if (boundMember is BoundMethodGroupExpression methodGroup)
                return BindInvocationOnMethodGroup(methodGroup, syntax);

            if (boundMember is BoundMemberAccessExpression { Member: IMethodSymbol method } memberExpr)
            {
                var argExprs = new List<BoundArgument>();
                bool argErrors = false;
                foreach (var arg in syntax.ArgumentList.Arguments)
                {
                    var boundArg = BindExpression(arg.Expression);
                    if (HasExpressionErrors(boundArg))
                        argErrors = true;
                    var name = arg.NameColon?.Name.Identifier.ValueText;
                    if (string.IsNullOrEmpty(name))
                        name = null;
                    argExprs.Add(new BoundArgument(boundArg, RefKind.None, name, arg));
                }

                if (argErrors)
                {
                    var candidates = ImmutableArray.Create(method);
                    return ErrorExpression(
                        method.ReturnType,
                        method,
                        BoundExpressionReason.ArgumentBindingFailed,
                        AsSymbolCandidates(candidates));
                }

                var argArray = argExprs.ToArray();

                if (AreArgumentsCompatibleWithMethod(method, argArray.Length, memberExpr.Receiver, argArray))
                {
                    var convertedArgs = ConvertArguments(method.Parameters, argArray);
                    return new BoundInvocationExpression(method, convertedArgs, memberExpr.Receiver);
                }

                receiver = memberExpr.Receiver;
                methodName = method.Name;
            }
            else if (boundMember is BoundTypeExpression { Type: INamedTypeSymbol namedType })
            {
                var argExprs = new List<BoundArgument>();
                bool argErrors = false;
                foreach (var arg in syntax.ArgumentList.Arguments)
                {
                    var boundArg = BindExpression(arg.Expression);
                    if (HasExpressionErrors(boundArg))
                        argErrors = true;
                    var name = arg.NameColon?.Name.Identifier.ValueText;
                    if (string.IsNullOrEmpty(name))
                        name = null;
                    argExprs.Add(new BoundArgument(boundArg, RefKind.None, name, arg));
                }

                if (argErrors)
                {
                    var constructors = namedType.Constructors.Where(static ctor => !ctor.IsStatic).ToImmutableArray();
                    var symbol = constructors.IsDefaultOrEmpty ? (ISymbol)namedType : constructors[0];
                    return ErrorExpression(
                        namedType,
                        symbol,
                        BoundExpressionReason.ArgumentBindingFailed,
                        AsSymbolCandidates(constructors));
                }

                return BindConstructorInvocation(namedType, argExprs.ToArray(), syntax);
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
            if (IsErrorExpression(boundIdentifier))
                return boundIdentifier is BoundErrorExpression boundError
                    ? boundError
                    : new BoundErrorExpression(boundIdentifier.Type ?? Compilation.ErrorTypeSymbol, null, BoundExpressionReason.OtherError);

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
                methodName = id.Identifier.ValueText;
            }
        }
        else if (syntax.Expression is GenericNameSyntax generic)
        {
            var boundTypeArguments = TryBindTypeArguments(generic);
            if (boundTypeArguments is null)
                return ErrorExpression(reason: BoundExpressionReason.TypeMismatch);

            var symbolCandidates = LookupSymbols(generic.Identifier.ValueText)
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

            var genericBoundArguments = new BoundArgument[syntax.ArgumentList.Arguments.Count];
            bool genericHasErrors = false;
            int genericIndex = 0;
            foreach (var arg in syntax.ArgumentList.Arguments)
            {
                var boundArg = BindExpression(arg.Expression);
                if (HasExpressionErrors(boundArg))
                    genericHasErrors = true;
                var name = arg.NameColon?.Name.Identifier.ValueText;
                if (string.IsNullOrEmpty(name))
                    name = null;
                genericBoundArguments[genericIndex++] = new BoundArgument(boundArg, RefKind.None, name, arg);
            }

            if (genericHasErrors)
                return ErrorExpression(reason: BoundExpressionReason.NotFound);

            var typeExpr = BindTypeSyntax(generic);
            if (typeExpr is BoundTypeExpression type && type.Type is INamedTypeSymbol namedType)
                return BindConstructorInvocation(namedType, genericBoundArguments, syntax);

            return ErrorExpression(reason: BoundExpressionReason.NotFound);
        }
        else
        {
            receiver = BindExpression(syntax.Expression);
            if (IsErrorExpression(receiver))
                return receiver is BoundErrorExpression boundError
                    ? boundError
                    : new BoundErrorExpression(receiver.Type ?? Compilation.ErrorTypeSymbol, null, BoundExpressionReason.OtherError);

            methodName = "Invoke";
        }

        // Bind arguments
        var boundArgumentsList = new List<BoundArgument>();
        bool hasErrors = false;
        foreach (var arg in syntax.ArgumentList.Arguments)
        {
            var boundArg = BindExpression(arg.Expression);
            if (HasExpressionErrors(boundArg))
                hasErrors = true;
            var name = arg.NameColon?.Name.Identifier.ValueText;
            if (string.IsNullOrEmpty(name))
                name = null;
            boundArgumentsList.Add(new BoundArgument(boundArg, RefKind.None, name, arg));
        }

        if (hasErrors)
            return InvocationError(receiver, methodName, BoundExpressionReason.ArgumentBindingFailed);

        var boundArguments = boundArgumentsList.ToArray();

        if (receiver is not null)
        {
            var receiverType = receiver.Type.UnwrapLiteralType() ?? receiver.Type;

            if (methodName == "Invoke" &&
                receiverType is INamedTypeSymbol { TypeKind: TypeKind.Delegate } delegateType &&
                delegateType.GetDelegateInvokeMethod() is { } invokeMethod)
            {
                if (!EnsureMemberAccessible(invokeMethod, syntax.Expression.GetLocation(), GetSymbolKindForDiagnostic(invokeMethod)))
                    return ErrorExpression(reason: BoundExpressionReason.Inaccessible);

                if (!AreArgumentsCompatibleWithMethod(invokeMethod, boundArguments.Length, receiver, boundArguments))
                {
                    ReportSuppressedLambdaDiagnostics(boundArguments);
                    _diagnostics.ReportNoOverloadForMethod(methodName, boundArguments.Length, syntax.GetLocation());
                    return ErrorExpression(reason: BoundExpressionReason.OverloadResolutionFailed);
                }

                var converted = ConvertArguments(invokeMethod.Parameters, boundArguments);
                return new BoundInvocationExpression(invokeMethod, converted, receiver);
            }
        }

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
                return ErrorExpression(reason: BoundExpressionReason.NotFound);
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
                    return ErrorExpression(reason: BoundExpressionReason.Inaccessible);

                var resolution = OverloadResolver.ResolveOverload(accessibleMethods, boundArguments, Compilation, canBindLambda: EnsureLambdaCompatible, callSyntax: syntax);
                if (resolution.Success)
                {
                    var method = resolution.Method!;
                    var convertedArgs = ConvertArguments(method.Parameters, boundArguments);
                    return new BoundInvocationExpression(method, convertedArgs, receiver);
                }

                if (resolution.IsAmbiguous)
                {
                    _diagnostics.ReportCallIsAmbiguous(methodName, resolution.AmbiguousCandidates, syntax.GetLocation());
                    return ErrorExpression(
                        reason: BoundExpressionReason.Ambiguous,
                        candidates: AsSymbolCandidates(resolution.AmbiguousCandidates));
                }

                var nestedType = typeReceiver.Type
                    .GetMembers(methodName)
                    .OfType<INamedTypeSymbol>()
                    .FirstOrDefault();

                if (nestedType is not null)
                    return BindConstructorInvocation(nestedType, boundArguments, syntax, receiver);

                ReportSuppressedLambdaDiagnostics(boundArguments);
                _diagnostics.ReportNoOverloadForMethod(methodName, boundArguments.Length, syntax.GetLocation());
                return ErrorExpression(reason: BoundExpressionReason.OverloadResolutionFailed);
            }

            var nested = typeReceiver.Type
                .GetMembers(methodName)
                .OfType<INamedTypeSymbol>()
                .FirstOrDefault();

            if (nested is not null)
                return BindConstructorInvocation(nested, boundArguments, syntax, receiver);

            _diagnostics.ReportMemberDoesNotContainDefinition(typeReceiver.Type.Name, methodName, syntax.Expression.GetLocation());

            return ErrorExpression(reason: BoundExpressionReason.NotFound);
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

                return ErrorExpression(reason: BoundExpressionReason.NotFound);
            }

            var accessibleCandidates = GetAccessibleMethods(candidates, syntax.Expression.GetLocation());

            if (accessibleCandidates.IsDefaultOrEmpty)
                return ErrorExpression(reason: BoundExpressionReason.Inaccessible);

            var resolution = OverloadResolver.ResolveOverload(accessibleCandidates, boundArguments, Compilation, canBindLambda: EnsureLambdaCompatible, callSyntax: syntax);
            if (resolution.Success)
            {
                var method = resolution.Method!;
                var convertedArgs = ConvertArguments(method.Parameters, boundArguments);
                return new BoundInvocationExpression(method, convertedArgs, receiver);
            }

            if (resolution.IsAmbiguous)
            {
                _diagnostics.ReportCallIsAmbiguous(methodName, resolution.AmbiguousCandidates, syntax.GetLocation());
                return ErrorExpression(
                    reason: BoundExpressionReason.Ambiguous,
                    candidates: AsSymbolCandidates(resolution.AmbiguousCandidates));
            }

            _diagnostics.ReportNoOverloadForMethod(methodName, boundArguments.Length, syntax.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.OverloadResolutionFailed);
        }

        // No receiver -> try methods first, then constructors
        var methodCandidates = new SymbolQuery(methodName).LookupMethods(this).ToImmutableArray();

        if (!methodCandidates.IsDefaultOrEmpty)
        {
            var accessibleMethods = GetAccessibleMethods(methodCandidates, syntax.Expression.GetLocation());

            if (accessibleMethods.IsDefaultOrEmpty)
                return ErrorExpression(reason: BoundExpressionReason.Inaccessible);

            var resolution = OverloadResolver.ResolveOverload(accessibleMethods, boundArguments, Compilation, canBindLambda: EnsureLambdaCompatible, callSyntax: syntax);
            if (resolution.Success)
            {
                var method = resolution.Method!;
                var convertedArgs = ConvertArguments(method.Parameters, boundArguments);
                return new BoundInvocationExpression(method, convertedArgs, null);
            }

            if (resolution.IsAmbiguous)
            {
                _diagnostics.ReportCallIsAmbiguous(methodName, resolution.AmbiguousCandidates, syntax.GetLocation());
                return ErrorExpression(
                    reason: BoundExpressionReason.Ambiguous,
                    candidates: AsSymbolCandidates(resolution.AmbiguousCandidates));
            }

            // Fall back to type if overload resolution failed
            var typeFallback = LookupType(methodName) as INamedTypeSymbol;
            if (typeFallback is not null)
                return BindConstructorInvocation(typeFallback, boundArguments, syntax);

            ReportSuppressedLambdaDiagnostics(boundArguments);
            _diagnostics.ReportNoOverloadForMethod(methodName, boundArguments.Length, syntax.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.OverloadResolutionFailed);
        }

        var typeSymbol = LookupType(methodName) as INamedTypeSymbol;
        if (typeSymbol is not null)
            return BindConstructorInvocation(typeSymbol, boundArguments, syntax);

        _diagnostics.ReportTheNameDoesNotExistInTheCurrentContext(methodName, syntax.Expression.GetLocation());
        return ErrorExpression(reason: BoundExpressionReason.NotFound);
    }

    private BoundArgument[] BindInvocationArguments(SeparatedSyntaxList<ArgumentSyntax> arguments, out bool hasErrors)
    {
        if (arguments.Count == 0)
        {
            hasErrors = false;
            return Array.Empty<BoundArgument>();
        }

        var boundArguments = new BoundArgument[arguments.Count];
        var seenErrors = false;

        for (int i = 0; i < arguments.Count; i++)
        {
            var syntax = arguments[i];
            var boundArg = BindExpression(syntax.Expression);
            if (HasExpressionErrors(boundArg))
                seenErrors = true;

            var name = syntax.NameColon?.Name.Identifier.ValueText;
            if (string.IsNullOrEmpty(name))
                name = null;

            boundArguments[i] = new BoundArgument(boundArg, RefKind.None, name, syntax);
        }

        hasErrors = seenErrors;
        return boundArguments;
    }

    private static bool IsErrorExpression(BoundExpression expression)
        => expression is BoundErrorExpression;

    private static bool HasExpressionErrors(BoundExpression expression)
    {
        if (IsErrorExpression(expression))
            return true;

        if (expression is BoundLambdaExpression)
            return false;

        return expression.Type?.ContainsErrorType() == true;
    }

    private BoundErrorExpression AsErrorExpression(BoundExpression expression)
    {
        return expression as BoundErrorExpression
            ?? new BoundErrorExpression(
                expression.Type ?? Compilation.ErrorTypeSymbol,
                null,
                BoundExpressionReason.OtherError);
    }

    private BoundErrorExpression InvocationError(
        BoundExpression? receiver,
        string methodName,
        BoundExpressionReason reason)
    {
        ImmutableArray<IMethodSymbol> candidates = default;
        ITypeSymbol? resultType = null;
        ISymbol? symbol = null;

        if (receiver is BoundNamespaceExpression nsReceiver)
        {
            var typeInNamespace = nsReceiver.Namespace
                .GetMembers(methodName)
                .OfType<INamedTypeSymbol>()
                .FirstOrDefault();

            if (typeInNamespace is not null)
            {
                candidates = typeInNamespace.Constructors
                    .Where(static ctor => !ctor.IsStatic)
                    .ToImmutableArray();
                symbol = candidates.IsDefaultOrEmpty ? typeInNamespace : candidates[0];
                resultType = typeInNamespace;
            }
        }
        else if (receiver is BoundTypeExpression typeReceiver)
        {
            candidates = new SymbolQuery(methodName, typeReceiver.Type, IsStatic: true)
                .LookupMethods(this)
                .ToImmutableArray();

            if (!candidates.IsDefaultOrEmpty)
            {
                symbol = candidates[0];
                resultType = candidates[0].ReturnType;
            }
            else
            {
                symbol = typeReceiver.Type;
                resultType = typeReceiver.Type;
            }
        }
        else if (receiver is not null)
        {
            candidates = new SymbolQuery(methodName, receiver.Type, IsStatic: false)
                .LookupMethods(this)
                .ToImmutableArray();

            if (!candidates.IsDefaultOrEmpty)
            {
                symbol = candidates[0];
                resultType = candidates[0].ReturnType;
            }
            else
            {
                resultType = receiver.Type;
            }
        }
        else
        {
            candidates = new SymbolQuery(methodName)
                .LookupMethods(this)
                .ToImmutableArray();

            if (!candidates.IsDefaultOrEmpty)
            {
                symbol = candidates[0];
                resultType = candidates[0].ReturnType;
            }
        }

        return ErrorExpression(
            resultType ?? Compilation.ErrorTypeSymbol,
            symbol,
            reason,
            AsSymbolCandidates(candidates));
    }

    private BoundExpression BindInvocationOnMethodGroup(BoundMethodGroupExpression methodGroup, InvocationExpressionSyntax syntax)
    {
        var boundArguments = BindInvocationArguments(syntax.ArgumentList.Arguments, out var hasErrors);
        if (hasErrors || methodGroup.Receiver is { } receiver && IsErrorExpression(receiver))
        {
            var selectedForError = methodGroup.SelectedMethod;
            var symbol = selectedForError ?? methodGroup.Methods.FirstOrDefault();
            var returnType = symbol?.ReturnType ?? Compilation.ErrorTypeSymbol;

            return ErrorExpression(
                returnType,
                symbol,
                BoundExpressionReason.ArgumentBindingFailed,
                AsSymbolCandidates(methodGroup.Methods));
        }

        var methodName = methodGroup.Methods[0].Name;
        var selected = methodGroup.SelectedMethod;
        var extensionReceiver = IsExtensionReceiver(methodGroup.Receiver) ? methodGroup.Receiver : null;
        var receiverSyntax = GetInvocationReceiverSyntax(syntax) ?? syntax.Expression;

        if (selected is not null)
        {
            var inferred = OverloadResolver.ApplyTypeArgumentInference(selected, extensionReceiver, boundArguments, Compilation);
            if (inferred is not null)
            {
                selected = inferred;

                if (AreArgumentsCompatibleWithMethod(selected, boundArguments.Length, extensionReceiver, boundArguments))
                {
                    var converted = ConvertInvocationArguments(
                        selected,
                        boundArguments,
                        extensionReceiver,
                        receiverSyntax,
                        out var convertedExtensionReceiver);
                    return new BoundInvocationExpression(selected, converted, methodGroup.Receiver, convertedExtensionReceiver);
                }
            }
        }

        var resolution = OverloadResolver.ResolveOverload(methodGroup.Methods, boundArguments, Compilation, extensionReceiver, EnsureLambdaCompatible, callSyntax: syntax);

        if (resolution.Success)
        {
            var method = resolution.Method!;
            var convertedArgs = ConvertInvocationArguments(
                method,
                boundArguments,
                extensionReceiver,
                receiverSyntax,
                out var convertedExtensionReceiver);
            return new BoundInvocationExpression(method, convertedArgs, methodGroup.Receiver, convertedExtensionReceiver);
        }

        if (resolution.IsAmbiguous)
        {
            _diagnostics.ReportCallIsAmbiguous(methodName, resolution.AmbiguousCandidates, syntax.GetLocation());
            return ErrorExpression(
                reason: BoundExpressionReason.Ambiguous,
                candidates: AsSymbolCandidates(resolution.AmbiguousCandidates));
        }

        if (LookupType(methodName) is INamedTypeSymbol typeFallback)
        {
            return BindConstructorInvocation(typeFallback, boundArguments, syntax, receiver: null);
        }

        ReportSuppressedLambdaDiagnostics(boundArguments);
        _diagnostics.ReportNoOverloadForMethod(methodName, boundArguments.Length, syntax.GetLocation());
        return ErrorExpression(reason: BoundExpressionReason.OverloadResolutionFailed);
    }

    private BoundExpression BindConstructorInvocation(
        INamedTypeSymbol typeSymbol,
        BoundArgument[] boundArguments,
        InvocationExpressionSyntax syntax,
        BoundExpression? receiver = null)
    {
        if (typeSymbol.TypeKind == TypeKind.Error)
            return new BoundErrorExpression(typeSymbol, null, BoundExpressionReason.OtherError);

        var resolution = OverloadResolver.ResolveOverload(typeSymbol.Constructors, boundArguments, Compilation, canBindLambda: EnsureLambdaCompatible, callSyntax: syntax);
        if (resolution.Success)
        {
            var constructor = resolution.Method!;
            if (!EnsureMemberAccessible(constructor, syntax.Expression.GetLocation(), "constructor"))
                return new BoundErrorExpression(typeSymbol, null, BoundExpressionReason.Inaccessible);
            var convertedArgs = ConvertArguments(constructor.Parameters, boundArguments);
            return new BoundObjectCreationExpression(constructor, convertedArgs, receiver);
        }

        if (resolution.IsAmbiguous)
        {
            _diagnostics.ReportCallIsAmbiguous(typeSymbol.Name, resolution.AmbiguousCandidates, syntax.GetLocation());
            return new BoundErrorExpression(
                typeSymbol,
                null,
                BoundExpressionReason.Ambiguous,
                AsSymbolCandidates(resolution.AmbiguousCandidates));
        }

        ReportSuppressedLambdaDiagnostics(boundArguments);
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
            return ErrorExpression(reason: BoundExpressionReason.NotFound);
        }

        if (typeSymbol == null)
        {
            //_diagnostics.ReportInvalidObjectCreation(syntax.Type.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.NotFound);
        }

        if (typeSymbol == null)
        {
            _diagnostics.ReportTheNameDoesNotExistInTheCurrentContext(syntax.Type.ToString(), syntax.Type.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.NotFound);
        }

        if (typeSymbol.TypeKind == TypeKind.Error)
            return new BoundErrorExpression(typeSymbol, null, BoundExpressionReason.OtherError);

        var validatedType = EnsureTypeAccessible(typeSymbol, syntax.Type.GetLocation());
        if (validatedType.TypeKind == TypeKind.Error)
            return new BoundErrorExpression(typeSymbol, null, BoundExpressionReason.Inaccessible);

        // Bind arguments
        var boundArguments = new BoundArgument[syntax.ArgumentList.Arguments.Count];
        bool hasErrors = false;
        int i = 0;
        foreach (var arg in syntax.ArgumentList.Arguments)
        {
            var boundArg = BindExpression(arg.Expression);
            if (IsErrorExpression(boundArg))
                hasErrors = true;
            var name = arg.NameColon?.Name.Identifier.ValueText;
            if (string.IsNullOrEmpty(name))
                name = null;
            boundArguments[i++] = new BoundArgument(boundArg, RefKind.None, name, arg);
        }

        if (hasErrors)
            return new BoundErrorExpression(typeSymbol, null, BoundExpressionReason.ArgumentBindingFailed);

        // Overload resolution
        var resolution = OverloadResolver.ResolveOverload(typeSymbol.Constructors, boundArguments, Compilation, canBindLambda: EnsureLambdaCompatible, callSyntax: syntax);
        if (resolution.Success)
        {
            var constructor = resolution.Method!;
            constructor = EnsureConstructedConstructor(constructor, typeSymbol);
            if (!EnsureMemberAccessible(constructor, syntax.Type.GetLocation(), "constructor"))
                return new BoundErrorExpression(typeSymbol, null, BoundExpressionReason.Inaccessible);

            var convertedArgs = ConvertArguments(constructor.Parameters, boundArguments);
            return new BoundObjectCreationExpression(constructor, convertedArgs);
        }

        if (resolution.IsAmbiguous)
        {
            _diagnostics.ReportCallIsAmbiguous(typeSymbol.Name, resolution.AmbiguousCandidates, syntax.GetLocation());
            return new BoundErrorExpression(
                typeSymbol,
                null,
                BoundExpressionReason.Ambiguous,
                AsSymbolCandidates(resolution.AmbiguousCandidates));
        }

        ReportSuppressedLambdaDiagnostics(boundArguments);
        _diagnostics.ReportNoOverloadForMethod(typeSymbol.Name, boundArguments.Length, syntax.GetLocation());
        return new BoundErrorExpression(typeSymbol, null, BoundExpressionReason.OverloadResolutionFailed);
    }

    protected static IMethodSymbol EnsureConstructedConstructor(IMethodSymbol constructor, INamedTypeSymbol typeSymbol)
    {
        if (constructor is null)
            throw new ArgumentNullException(nameof(constructor));
        if (typeSymbol is null)
            throw new ArgumentNullException(nameof(typeSymbol));

        if (constructor is SubstitutedMethodSymbol || constructor is ConstructedMethodSymbol)
            return constructor;

        if (typeSymbol is not ConstructedNamedTypeSymbol constructed)
            return constructor;

        if (constructed.ConstructedFrom is not INamedTypeSymbol originalDefinition)
            return constructor;

        if (constructor.ContainingType is not INamedTypeSymbol containingDefinition)
            return constructor;

        if (!SymbolEqualityComparer.Default.Equals(containingDefinition, originalDefinition))
            return constructor;

        return new SubstitutedMethodSymbol(constructor, constructed);
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
                    var name = memberBinding.Name.Identifier.ValueText;
                    var receiverType = receiver.Type.UnwrapLiteralType() ?? receiver.Type;

                    var member = receiverType is null
                        ? null
                        : new SymbolQuery(name, receiverType, IsStatic: false).Lookup(this).FirstOrDefault();

                    if (member is null)
                    {
                        _diagnostics.ReportTheNameDoesNotExistInTheCurrentContext(name, memberBinding.Name.GetLocation());
                        whenNotNull = ErrorExpression(reason: BoundExpressionReason.NotFound);
                    }
                    else
                    {
                        if (!EnsureMemberAccessible(member, memberBinding.Name.GetLocation(), GetSymbolKindForDiagnostic(member)))
                        {
                            whenNotNull = ErrorExpression(reason: BoundExpressionReason.Inaccessible);
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
                    var name = memberBinding.Name.Identifier.ValueText;
                    var boundArguments = invocation.ArgumentList.Arguments
                        .Select(a =>
                        {
                            var expr = BindExpression(a.Expression);
                            var name = a.NameColon?.Name.Identifier.ValueText;
                            if (string.IsNullOrEmpty(name))
                                name = null;
                            return new BoundArgument(expr, RefKind.None, name, a);
                        })
                        .ToArray();

                    var receiverType = receiver.Type.UnwrapLiteralType() ?? receiver.Type;

                    var candidates = receiverType is null
                        ? ImmutableArray<IMethodSymbol>.Empty
                        : new SymbolQuery(name, receiverType, IsStatic: false).LookupMethods(this).ToImmutableArray();

                    if (candidates.IsDefaultOrEmpty)
                    {
                        _diagnostics.ReportTheNameDoesNotExistInTheCurrentContext(name, memberBinding.Name.GetLocation());
                        whenNotNull = ErrorExpression(reason: BoundExpressionReason.NotFound);
                    }
                    else
                    {
                        var accessibleCandidates = GetAccessibleMethods(candidates, memberBinding.Name.GetLocation());

                        if (accessibleCandidates.IsDefaultOrEmpty)
                        {
                            whenNotNull = ErrorExpression(reason: BoundExpressionReason.Inaccessible);
                        }
                        else
                        {
                            var resolution = OverloadResolver.ResolveOverload(accessibleCandidates, boundArguments, Compilation, canBindLambda: EnsureLambdaCompatible, callSyntax: invocation);
                            if (resolution.Success)
                            {
                                var converted = ConvertArguments(resolution.Method!.Parameters, boundArguments);
                                whenNotNull = new BoundInvocationExpression(resolution.Method!, converted, receiver);
                            }
                            else if (resolution.IsAmbiguous)
                            {
                                _diagnostics.ReportCallIsAmbiguous(name, resolution.AmbiguousCandidates, invocation.GetLocation());
                                whenNotNull = ErrorExpression(
                                    reason: BoundExpressionReason.Ambiguous,
                                    candidates: AsSymbolCandidates(resolution.AmbiguousCandidates));
                            }
                            else
                            {
                                ReportSuppressedLambdaDiagnostics(boundArguments);
                                _diagnostics.ReportNoOverloadForMethod(name, boundArguments.Length, invocation.GetLocation());
                                whenNotNull = ErrorExpression(reason: BoundExpressionReason.OverloadResolutionFailed);
                            }
                        }
                    }
                    break;
                }

            default:
                whenNotNull = ErrorExpression(reason: BoundExpressionReason.NotFound);
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

        if (IsErrorExpression(receiver))
            return receiver is BoundErrorExpression boundError
                ? boundError
                : new BoundErrorExpression(receiver.Type ?? Compilation.ErrorTypeSymbol, null, BoundExpressionReason.OtherError);

        var firstErrorArg = argumentExprs.FirstOrDefault(IsErrorExpression);
        if (firstErrorArg is not null)
            return firstErrorArg is BoundErrorExpression errorArg
                ? errorArg
                : new BoundErrorExpression(
                    firstErrorArg.Type ?? Compilation.ErrorTypeSymbol,
                    null,
                    BoundExpressionReason.OtherError);

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

    private BoundExpression BindAssignment(ExpressionSyntax leftSyntax, ExpressionSyntax rightSyntax, SyntaxNode node, SyntaxKind operatorTokenKind)
    {
        if (operatorTokenKind != SyntaxKind.EqualsToken)
            return BindCompoundAssignment(leftSyntax, rightSyntax, node, operatorTokenKind);

        if (leftSyntax is DiscardExpressionSyntax)
        {
            var right = BindExpression(rightSyntax);
            var assignmentType = right.Type ?? Compilation.ErrorTypeSymbol;
            var discardType = assignmentType.TypeKind == TypeKind.Error ? Compilation.ErrorTypeSymbol : assignmentType;
            var pattern = new BoundDiscardPattern(discardType);
            return BoundFactory.CreatePatternAssignmentExpression(assignmentType, pattern, right);
        }

        if (leftSyntax is ElementAccessExpressionSyntax elementAccess)
        {
            var right = BindExpression(rightSyntax);

            if (IsErrorExpression(right))
                return AsErrorExpression(right);

            var receiver = BindExpression(elementAccess.Expression);
            var args = elementAccess.ArgumentList.Arguments.Select(x => BindExpression(x.Expression)).ToArray();

            if (IsErrorExpression(receiver))
                return receiver is BoundErrorExpression boundError
                    ? boundError
                    : new BoundErrorExpression(
                        receiver.Type ?? Compilation.ErrorTypeSymbol,
                        null,
                        BoundExpressionReason.OtherError);

            var firstErrorArg = args.FirstOrDefault(IsErrorExpression);
            if (firstErrorArg is not null)
                return firstErrorArg is BoundErrorExpression errorArg
                    ? errorArg
                    : new BoundErrorExpression(
                        firstErrorArg.Type ?? Compilation.ErrorTypeSymbol,
                        null,
                        BoundExpressionReason.OtherError);

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

                return BoundFactory.CreateArrayAssignmentExpression(
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

            return BoundFactory.CreateIndexerAssignmentExpression(access, right);
        }

        // Fall back to normal variable/property assignment
        var left = BindExpression(leftSyntax);

        if (IsErrorExpression(left))
            return AsErrorExpression(left);

        if (left is BoundLocalAccess localAccess)
        {
            var localSymbol = localAccess.Local;
            var localType = localSymbol.Type;

            var right2 = BindExpression(rightSyntax);

            if (IsErrorExpression(right2))
                return AsErrorExpression(right2);

            if (localType is ByRefTypeSymbol byRefLocalType)
            {
                var converted = ConvertValueForAssignment(right2, byRefLocalType.ElementType, rightSyntax);
                if (converted is BoundErrorExpression)
                    return converted;

                return BoundFactory.CreateByRefAssignmentExpression(localAccess, byRefLocalType.ElementType, converted);
            }

            if (!localSymbol.IsMutable)
            {
                _diagnostics.ReportThisValueIsNotMutable(leftSyntax.GetLocation());
                return ErrorExpression(reason: BoundExpressionReason.NotFound);
            }

            if (right2 is BoundEmptyCollectionExpression)
            {
                return BoundFactory.CreateLocalAssignmentExpression(localSymbol, new BoundEmptyCollectionExpression(localSymbol.Type));
            }

            if (localType.TypeKind != TypeKind.Error &&
                ShouldAttemptConversion(right2))
            {
                if (!IsAssignable(localType, right2.Type!, out var conversion))
                {
                    _diagnostics.ReportCannotAssignFromTypeToType(
                        right2.Type!.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        localType.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        rightSyntax.GetLocation());
                    return new BoundErrorExpression(localType, null, BoundExpressionReason.TypeMismatch);
                }

                right2 = ApplyConversion(right2, localType, conversion, rightSyntax);
            }

            return BoundFactory.CreateLocalAssignmentExpression(localSymbol, right2);
        }
        else if (left is BoundParameterAccess parameterAccess)
        {
            var parameterSymbol = parameterAccess.Parameter;
            var parameterType = parameterSymbol.Type;

            if (!parameterSymbol.IsMutable)
            {
                _diagnostics.ReportThisValueIsNotMutable(leftSyntax.GetLocation());
                return ErrorExpression(reason: BoundExpressionReason.NotFound);
            }

            var right2 = BindExpression(rightSyntax);

            if (IsErrorExpression(right2))
                return AsErrorExpression(right2);

            if (parameterType is ByRefTypeSymbol byRefParameterType &&
                parameterSymbol.RefKind is RefKind.Ref or RefKind.Out)
            {
                var converted = ConvertValueForAssignment(right2, byRefParameterType.ElementType, rightSyntax);
                if (converted is BoundErrorExpression)
                    return converted;

                return BoundFactory.CreateByRefAssignmentExpression(parameterAccess, byRefParameterType.ElementType, converted);
            }

            if (parameterType.TypeKind != TypeKind.Error &&
                ShouldAttemptConversion(right2))
            {
                if (!IsAssignable(parameterType, right2.Type!, out var conversion))
                {
                    _diagnostics.ReportCannotAssignFromTypeToType(
                        right2.Type!.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        parameterType.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        rightSyntax.GetLocation());
                    return new BoundErrorExpression(parameterType, null, BoundExpressionReason.TypeMismatch);
                }

                right2 = ApplyConversion(right2, parameterType, conversion, rightSyntax);
            }

            return BoundFactory.CreateParameterAssignmentExpression(parameterSymbol, right2);
        }
        else if (left.Symbol is IFieldSymbol fieldSymbol)
        {
            if (fieldSymbol.IsConst)
            {
                _diagnostics.ReportThisValueIsNotMutable(leftSyntax.GetLocation());
                return new BoundErrorExpression(fieldSymbol.Type, null, BoundExpressionReason.NotFound);
            }

            var receiver = GetReceiver(left);

            var right2 = BindExpression(rightSyntax);

            if (IsErrorExpression(right2))
                return AsErrorExpression(right2);

            if (!CanAssignToField(fieldSymbol, receiver, leftSyntax))
                return new BoundErrorExpression(fieldSymbol.Type, fieldSymbol, BoundExpressionReason.NotFound);

            if (right2 is BoundEmptyCollectionExpression)
            {
                return CreateFieldAssignmentExpression(receiver, fieldSymbol, BoundFactory.CreateEmptyCollectionExpression(fieldSymbol.Type));
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

            return CreateFieldAssignmentExpression(receiver, fieldSymbol, right2);
        }
        else if (left.Symbol is IPropertySymbol propertySymbol)
        {
            SourceFieldSymbol? backingField = null;

            var receiver = GetReceiver(left);

            if (propertySymbol.SetMethod is null)
            {
                if (!TryGetWritableAutoPropertyBackingField(propertySymbol, left, out backingField))
                {
                    _diagnostics.ReportPropertyOrIndexerCannotBeAssignedIsReadOnly(propertySymbol.Name, leftSyntax.GetLocation());
                    return ErrorExpression(reason: BoundExpressionReason.NotFound);
                }
            }

            var right2 = BindExpression(rightSyntax);

            if (IsErrorExpression(right2))
                return AsErrorExpression(right2);

            if (right2 is BoundEmptyCollectionExpression)
            {
                var empty = new BoundEmptyCollectionExpression(propertySymbol.Type);

                if (backingField is not null)
                {
                    if (!CanAssignToField(backingField, receiver, leftSyntax))
                        return new BoundErrorExpression(backingField.Type, backingField, BoundExpressionReason.NotFound);

                    return CreateFieldAssignmentExpression(receiver, backingField, empty);
                }

                return BoundFactory.CreatePropertyAssignmentExpression(receiver, propertySymbol, empty);
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
                if (!CanAssignToField(backingField, receiver, leftSyntax))
                    return new BoundErrorExpression(backingField.Type, backingField, BoundExpressionReason.NotFound);

                return CreateFieldAssignmentExpression(receiver, backingField, right2);
            }

            return BoundFactory.CreatePropertyAssignmentExpression(receiver, propertySymbol, right2);
        }

        return ErrorExpression(reason: BoundExpressionReason.NotFound);
    }

    private BoundExpression BindAssignmentExpression(AssignmentExpressionSyntax syntax)
    {
        return BindAssignment(syntax.Left, syntax.Right, syntax, syntax.OperatorToken.Kind);
    }

    private BoundExpression ConvertValueForAssignment(BoundExpression value, ITypeSymbol targetType, SyntaxNode syntax)
    {
        if (targetType.TypeKind == TypeKind.Error || value.Type is null || !ShouldAttemptConversion(value))
            return value;

        if (!IsAssignable(targetType, value.Type, out var conversion))
        {
            _diagnostics.ReportCannotAssignFromTypeToType(
                value.Type.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                targetType.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                syntax.GetLocation());
            return new BoundErrorExpression(targetType, null, BoundExpressionReason.TypeMismatch);
        }

        return ApplyConversion(value, targetType, conversion, syntax);
    }

    private BoundStatement BindAssignmentStatement(AssignmentStatementSyntax syntax)
    {
        var bound = BindAssignment(syntax.Left, syntax.Right, syntax, syntax.OperatorToken.Kind);
        return bound is BoundAssignmentExpression assignment
            ? new BoundAssignmentStatement(assignment)
            : new BoundExpressionStatement(bound);
    }

    private BoundExpression BindAssignment(ExpressionOrPatternSyntax leftSyntax, ExpressionSyntax rightSyntax, SyntaxNode node, SyntaxKind operatorTokenKind)
    {
        if (leftSyntax is ExpressionSyntax leftExpression)
            return BindAssignment(leftExpression, rightSyntax, node, operatorTokenKind);

        var right = BindExpression(rightSyntax);
        if (leftSyntax is PatternSyntax patternSyntax)
        {
            if (operatorTokenKind != SyntaxKind.EqualsToken)
            {
                _diagnostics.ReportLeftOfAssignmentMustBeAVariablePropertyOrIndexer(node.GetLocation());
                return ErrorExpression(reason: BoundExpressionReason.UnsupportedOperation);
            }

            return BindPatternAssignment(patternSyntax, right, node);
        }

        _diagnostics.ReportLeftOfAssignmentMustBeAVariablePropertyOrIndexer(node.GetLocation());
        return ErrorExpression(right.Type, reason: BoundExpressionReason.NotFound);
    }

    private BoundExpression BindPatternAssignment(PatternSyntax patternSyntax, BoundExpression right, SyntaxNode node)
    {
        var valueType = right.Type ?? Compilation.ErrorTypeSymbol;
        var boundPattern = BindPatternForAssignment(patternSyntax, valueType, node);

        if (boundPattern.Reason == BoundExpressionReason.UnsupportedOperation)
        {
            return ErrorExpression(reason: BoundExpressionReason.UnsupportedOperation);
        }

        var assignmentType = right.Type ?? boundPattern.Type ?? Compilation.ErrorTypeSymbol;
        return BoundFactory.CreatePatternAssignmentExpression(assignmentType, boundPattern, right);
    }

    private string GetPatternTypeDisplay(ITypeSymbol type)
    {
        if (type is LiteralTypeSymbol literal)
            return literal.UnderlyingType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);

        return type.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat);
    }

    private BoundPattern BindPatternForAssignment(PatternSyntax patternSyntax, ITypeSymbol valueType, SyntaxNode node)
    {
        valueType ??= Compilation.ErrorTypeSymbol;

        switch (patternSyntax)
        {
            case VariablePatternSyntax variablePattern:
                return BindVariablePatternForAssignment(variablePattern, valueType);
            case TuplePatternSyntax tuplePattern:
                return BindTuplePatternForAssignment(tuplePattern, valueType);
            case DiscardPatternSyntax:
                return new BoundDiscardPattern(valueType.TypeKind == TypeKind.Error ? Compilation.ErrorTypeSymbol : valueType);
            case DeclarationPatternSyntax declaration:
                return BindDeclarationPatternForAssignment(declaration, valueType, node);
            default:
                _diagnostics.ReportLeftOfAssignmentMustBeAVariablePropertyOrIndexer(node.GetLocation());
                return new BoundDiscardPattern(Compilation.ErrorTypeSymbol, BoundExpressionReason.UnsupportedOperation);
        }
    }

    private BoundPattern BindVariablePatternForAssignment(VariablePatternSyntax pattern, ITypeSymbol valueType)
    {
        var isMutable = pattern.BindingKeyword.IsKind(SyntaxKind.VarKeyword);
        return BindVariableDesignationForAssignment(pattern.Designation, valueType, isMutable);
    }

    private BoundPattern BindDeclarationPatternForAssignment(
        DeclarationPatternSyntax pattern,
        ITypeSymbol valueType,
        SyntaxNode node)
    {
        if (pattern.Type is IdentifierNameSyntax identifier &&
            pattern.Designation is SingleVariableDesignationSyntax { Identifier.IsMissing: true })
        {
            var name = identifier.Identifier.ValueText;

            if (string.IsNullOrEmpty(name))
            {
                _diagnostics.ReportLeftOfAssignmentMustBeAVariablePropertyOrIndexer(node.GetLocation());
                return new BoundDiscardPattern(Compilation.ErrorTypeSymbol, BoundExpressionReason.UnsupportedOperation);
            }

            if (name == "_")
                return new BoundDiscardPattern(valueType.TypeKind == TypeKind.Error ? Compilation.ErrorTypeSymbol : valueType);

            ILocalSymbol? local = null;
            if (_locals.TryGetValue(name, out var existingLocal))
            {
                local = existingLocal.Symbol;
            }
            else if (LookupSymbol(name) is ILocalSymbol scopedLocal)
            {
                local = scopedLocal;
            }

            if (local is null)
            {
                _diagnostics.ReportTheNameDoesNotExistInTheCurrentContext(name, identifier.GetLocation());
                return new BoundDiscardPattern(Compilation.ErrorTypeSymbol, BoundExpressionReason.NotFound);
            }

            if (!local.IsMutable)
            {
                _diagnostics.ReportThisValueIsNotMutable(identifier.GetLocation());
                return new BoundDeclarationPattern(
                    local.Type ?? Compilation.ErrorTypeSymbol,
                    new BoundSingleVariableDesignator(local),
                    BoundExpressionReason.UnsupportedOperation);
            }

            var targetType = local.Type ?? Compilation.ErrorTypeSymbol;
            var sourceType = valueType.UnwrapLiteralType() ?? valueType;

            if (targetType.TypeKind != TypeKind.Error &&
                sourceType.TypeKind != TypeKind.Error &&
                !IsAssignable(targetType, sourceType, out _))
            {
                _diagnostics.ReportCannotAssignFromTypeToType(
                    GetPatternTypeDisplay(sourceType),
                    targetType.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    identifier.GetLocation());

                return new BoundDeclarationPattern(
                    targetType,
                    new BoundSingleVariableDesignator(local),
                    BoundExpressionReason.TypeMismatch);
            }

            return new BoundDeclarationPattern(targetType, new BoundSingleVariableDesignator(local));
        }

        _diagnostics.ReportLeftOfAssignmentMustBeAVariablePropertyOrIndexer(node.GetLocation());
        return new BoundDiscardPattern(Compilation.ErrorTypeSymbol, BoundExpressionReason.UnsupportedOperation);
    }

    private BoundPattern BindTuplePatternForAssignment(TuplePatternSyntax pattern, ITypeSymbol valueType)
    {
        var elements = pattern.Elements;
        var elementCount = elements.Count;

        var elementTypes = ImmutableArray<ITypeSymbol>.Empty;
        if (valueType.TypeKind != TypeKind.Error)
            elementTypes = GetTupleElementTypes(valueType);

        if (elementTypes.IsDefaultOrEmpty)
        {
            if (elementCount > 0 && valueType.TypeKind != TypeKind.Error)
            {
                _diagnostics.ReportTupleDeconstructionRequiresTupleType(
                    GetPatternTypeDisplay(valueType),
                    pattern.GetLocation());
            }

            var builder = ImmutableArray.CreateBuilder<ITypeSymbol>(elementCount);
            for (var i = 0; i < elementCount; i++)
                builder.Add(Compilation.ErrorTypeSymbol);
            elementTypes = builder.ToImmutable();
        }
        else if (elementTypes.Length != elementCount)
        {
            _diagnostics.ReportTupleDeconstructionElementCountMismatch(
                elementCount,
                elementTypes.Length,
                pattern.GetLocation());

            if (elementTypes.Length < elementCount)
            {
                var builder = ImmutableArray.CreateBuilder<ITypeSymbol>(elementCount);
                builder.AddRange(elementTypes);
                while (builder.Count < elementCount)
                    builder.Add(Compilation.ErrorTypeSymbol);
                elementTypes = builder.ToImmutable();
            }
            else
            {
                elementTypes = elementTypes.Take(elementCount).ToImmutableArray();
            }
        }

        var boundElements = ImmutableArray.CreateBuilder<BoundPattern>(elementCount);

        for (var i = 0; i < elementCount; i++)
        {
            var elementSyntax = elements[i];
            var elementType = elementTypes.Length > i ? elementTypes[i] : Compilation.ErrorTypeSymbol;
            var boundElement = BindPatternForAssignment(elementSyntax.Pattern, elementType, elementSyntax.Pattern);
            boundElements.Add(boundElement);
        }

        var tupleElements = new List<(string? name, ITypeSymbol type)>(boundElements.Count);
        foreach (var element in boundElements)
        {
            var elementType = element.Type ?? Compilation.ErrorTypeSymbol;
            tupleElements.Add((null, elementType));
        }

        var tupleType = Compilation.CreateTupleTypeSymbol(tupleElements);
        return new BoundTuplePattern(tupleType, boundElements.ToImmutable());
    }

    private BoundPattern BindVariableDesignationForAssignment(
        VariableDesignationSyntax designation,
        ITypeSymbol valueType,
        bool isMutable)
    {
        valueType ??= Compilation.ErrorTypeSymbol;

        switch (designation)
        {
            case SingleVariableDesignationSyntax single:
                return BindSingleVariableDesignationForAssignment(single, valueType, isMutable);
            case ParenthesizedVariableDesignationSyntax parenthesized:
                return BindParenthesizedDesignationForAssignment(parenthesized, valueType, isMutable);
            case TypedVariableDesignationSyntax typed:
                return BindTypedDesignationForAssignment(typed, valueType, isMutable);
            default:
                _diagnostics.ReportLeftOfAssignmentMustBeAVariablePropertyOrIndexer(designation.GetLocation());
                return new BoundDiscardPattern(Compilation.ErrorTypeSymbol, BoundExpressionReason.UnsupportedOperation);
        }
    }

    private BoundPattern BindTypedDesignationForAssignment(
        TypedVariableDesignationSyntax typedDesignation,
        ITypeSymbol incomingType,
        bool isMutable)
    {
        var declaredType = ResolveType(typedDesignation.TypeAnnotation.Type);
        declaredType = EnsureTypeAccessible(declaredType, typedDesignation.TypeAnnotation.Type.GetLocation());

        if (incomingType.TypeKind != TypeKind.Error &&
            declaredType.TypeKind != TypeKind.Error &&
            !IsAssignable(declaredType, incomingType, out _))
        {
            _diagnostics.ReportCannotAssignFromTypeToType(
                incomingType.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                declaredType.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                typedDesignation.TypeAnnotation.Type.GetLocation());

            declaredType = Compilation.ErrorTypeSymbol;
        }

        return BindVariableDesignationForAssignment(typedDesignation.Designation, declaredType, isMutable);
    }

    private BoundPattern BindSingleVariableDesignationForAssignment(
        SingleVariableDesignationSyntax single,
        ITypeSymbol valueType,
        bool isMutable)
    {
        var identifier = single.Identifier;

        var normalizedType = TypeSymbolNormalization.NormalizeForInference(valueType);
        if (identifier.IsMissing || identifier.ValueText == "_")
            return new BoundDiscardPattern(normalizedType.TypeKind == TypeKind.Error ? Compilation.ErrorTypeSymbol : normalizedType);

        var type = normalizedType.TypeKind == TypeKind.Error ? Compilation.ErrorTypeSymbol : normalizedType;
        type = EnsureTypeAccessible(type, identifier.GetLocation());

        var local = DeclarePatternLocal(single, identifier.ValueText, isMutable, type);
        var designator = new BoundSingleVariableDesignator(local);

        return new BoundDeclarationPattern(type, designator);
    }

    private BoundPattern BindParenthesizedDesignationForAssignment(
        ParenthesizedVariableDesignationSyntax parenthesized,
        ITypeSymbol valueType,
        bool isMutable)
    {
        var variables = parenthesized.Variables;
        var elementCount = variables.Count;

        var elementTypes = ImmutableArray<ITypeSymbol>.Empty;
        if (valueType.TypeKind != TypeKind.Error)
            elementTypes = GetTupleElementTypes(valueType);

        if (elementTypes.IsDefaultOrEmpty)
        {
            if (elementCount > 0 && valueType.TypeKind != TypeKind.Error)
            {
                _diagnostics.ReportTupleDeconstructionRequiresTupleType(
                    GetPatternTypeDisplay(valueType),
                    parenthesized.GetLocation());
            }

            var builder = ImmutableArray.CreateBuilder<ITypeSymbol>(elementCount);
            for (var i = 0; i < elementCount; i++)
                builder.Add(Compilation.ErrorTypeSymbol);
            elementTypes = builder.ToImmutable();
        }
        else if (elementTypes.Length != elementCount)
        {
            _diagnostics.ReportTupleDeconstructionElementCountMismatch(
                elementCount,
                elementTypes.Length,
                parenthesized.GetLocation());

            if (elementTypes.Length < elementCount)
            {
                var builder = ImmutableArray.CreateBuilder<ITypeSymbol>(elementCount);
                builder.AddRange(elementTypes);
                while (builder.Count < elementCount)
                    builder.Add(Compilation.ErrorTypeSymbol);
                elementTypes = builder.ToImmutable();
            }
            else
            {
                elementTypes = elementTypes.Take(elementCount).ToImmutableArray();
            }
        }

        var boundElements = ImmutableArray.CreateBuilder<BoundPattern>(elementCount);

        for (var i = 0; i < elementCount; i++)
        {
            var variable = variables[i];
            var elementType = elementTypes.Length > i ? elementTypes[i] : Compilation.ErrorTypeSymbol;
            var boundElement = BindVariableDesignationForAssignment(variable, elementType, isMutable);
            boundElements.Add(boundElement);
        }

        var tupleElements = new List<(string? name, ITypeSymbol type)>(boundElements.Count);
        foreach (var element in boundElements)
        {
            var elementType = element.Type ?? Compilation.ErrorTypeSymbol;
            tupleElements.Add((null, elementType));
        }

        var tupleType = Compilation.CreateTupleTypeSymbol(tupleElements);
        return new BoundTuplePattern(tupleType, boundElements.ToImmutable());
    }

    private ILocalSymbol DeclarePatternLocal(
        SyntaxNode designationSyntax,
        string name,
        bool isMutable,
        ITypeSymbol type)
    {
        var isShadowingExistingInScope = false;

        if (_locals.TryGetValue(name, out var existing) && existing.Depth == _scopeDepth)
        {
            var existingSyntax = existing.Symbol.DeclaringSyntaxReferences.FirstOrDefault()?.GetSyntax();
            if (existingSyntax?.Parent == designationSyntax.Parent)
            {
                _diagnostics.ReportVariableAlreadyDefined(name, designationSyntax.GetLocation());
                return existing.Symbol;
            }

            isShadowingExistingInScope = true;
        }

        if (!isShadowingExistingInScope && LookupSymbol(name) is ILocalSymbol or IParameterSymbol or IFieldSymbol)
            isShadowingExistingInScope = true;

        if (isShadowingExistingInScope)
            _diagnostics.ReportVariableShadowsPreviousDeclaration(name, designationSyntax.GetLocation());

        return CreateLocalSymbol(designationSyntax, name, isMutable, type);
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

    private BoundAssignmentExpression CreateFieldAssignmentExpression(
        BoundExpression? receiver,
        IFieldSymbol fieldSymbol,
        BoundExpression value)
    {
        return BoundFactory.CreateFieldAssignmentExpression(receiver, fieldSymbol, value);
    }

    private bool CanAssignToField(IFieldSymbol fieldSymbol, BoundExpression? receiver, SyntaxNode syntax)
    {
        if (fieldSymbol.IsMutable)
            return true;

        if (_containingSymbol is IMethodSymbol methodSymbol)
        {
            if (fieldSymbol.IsStatic)
            {
                if (methodSymbol.MethodKind == MethodKind.StaticConstructor &&
                    SymbolEqualityComparer.Default.Equals(methodSymbol.ContainingType, fieldSymbol.ContainingType))
                {
                    return true;
                }
            }
            else if (methodSymbol.MethodKind == MethodKind.Constructor &&
                     !methodSymbol.IsStatic &&
                     SymbolEqualityComparer.Default.Equals(methodSymbol.ContainingType, fieldSymbol.ContainingType) &&
                     IsConstructorSelfReceiver(methodSymbol, receiver))
            {
                return true;
            }
            else if (methodSymbol.MethodKind == MethodKind.NamedConstructor &&
                  SymbolEqualityComparer.Default.Equals(methodSymbol.ContainingType, fieldSymbol.ContainingType))
            {
                return true;
            }
        }

        _diagnostics.ReportReadOnlyFieldCannotBeAssignedTo(syntax.GetLocation());
        return false;
    }

    protected bool IsAssignable(ITypeSymbol targetType, ITypeSymbol sourceType, out Conversion conversion)
    {
        if (targetType.ContainsErrorType() || sourceType.ContainsErrorType())
        {
            conversion = new Conversion(isImplicit: true, isIdentity: true);
            return true;
        }

        conversion = Compilation.ClassifyConversion(sourceType, targetType);
        return conversion.Exists && conversion.IsImplicit;
    }

    private static bool ShouldAttemptConversion(BoundExpression expression)
    {
        return expression is BoundMethodGroupExpression ||
            expression.Type is { } type && !type.ContainsErrorType();
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
        var containingType = method.ContainingType?.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat);
        var parameterTypes = method.Parameters
            .Select(p => p.Type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat))
            .ToArray();
        var parameterList = string.Join(", ", parameterTypes);
        var name = containingType is null or { Length: 0 }
            ? method.Name
            : $"{containingType}.{method.Name}";

        return parameterTypes.Length == 0 ? name : $"{name}({parameterList})";
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

    protected BoundExpression[] ConvertArguments(ImmutableArray<IParameterSymbol> parameters, IReadOnlyList<BoundArgument> arguments)
    {
        var converted = new BoundExpression[parameters.Length];

        if (!OverloadResolver.TryMapArguments(parameters, arguments, treatAsExtension: false, out var mappedArguments))
        {
            mappedArguments = new BoundArgument?[parameters.Length];

            var limit = Math.Min(arguments.Count, parameters.Length);
            for (int i = 0; i < limit; i++)
            {
                mappedArguments[i] = arguments[i];
            }
        }

        for (int i = 0; i < parameters.Length; i++)
        {
            var parameter = parameters[i];
            var argument = mappedArguments[i];

            if (argument is null)
            {
                converted[i] = CreateOptionalArgument(parameter);
                continue;
            }

            var boundArgument = argument.Value;
            var expression = boundArgument.Expression;

            if (parameter.Type is INamedTypeSymbol delegateType && expression is BoundLambdaExpression lambdaArgument)
            {
                var rebound = ReplayLambda(lambdaArgument, delegateType);
                if (rebound is null)
                {
                    lambdaArgument.Unbound?.ReportSuppressedDiagnostics(_diagnostics);
                    converted[i] = new BoundErrorExpression(parameter.Type, null, BoundExpressionReason.TypeMismatch);
                    continue;
                }

                expression = rebound;
            }

            if (parameter.RefKind is RefKind.Ref or RefKind.Out or RefKind.In)
            {
                converted[i] = expression;
                continue;
            }

            if (!ShouldAttemptConversion(expression) ||
                parameter.Type.TypeKind == TypeKind.Error ||
                expression.Type is null)
            {
                converted[i] = expression;
                continue;
            }

            var syntaxNode = boundArgument.Syntax switch
            {
                ArgumentSyntax argumentSyntax => argumentSyntax.Expression,
                SyntaxNode node => node,
                _ => null
            };

            if (!IsAssignable(parameter.Type, expression.Type, out var conversion))
            {
                var location = syntaxNode?.GetLocation() ?? parameter.Locations.FirstOrDefault();

                if (location is not null)
                {
                    _diagnostics.ReportCannotConvertFromTypeToType(
                        expression.Type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        parameter.Type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        location);
                }

                converted[i] = new BoundErrorExpression(parameter.Type, null, BoundExpressionReason.TypeMismatch);
                continue;
            }

            converted[i] = ApplyConversion(expression, parameter.Type, conversion, syntaxNode);
        }

        return converted;
    }

    protected BoundExpression CreateOptionalArgument(IParameterSymbol parameter)
    {
        if (!parameter.HasExplicitDefaultValue)
            return new BoundErrorExpression(parameter.Type, null, BoundExpressionReason.ArgumentBindingFailed);

        var parameterType = parameter.Type;
        if (parameterType.TypeKind == TypeKind.Error)
            return new BoundErrorExpression(parameterType, null, BoundExpressionReason.ArgumentBindingFailed);

        var value = parameter.ExplicitDefaultValue;

        if (value is null)
            return BoundFactory.NullLiteral(parameterType);

        if (!TryCreateOptionalLiteral(parameterType, value, out var literal))
        {
            ReportOptionalParameterDefaultValueCannotConvert(parameter, parameterType);
            return new BoundErrorExpression(parameterType, null, BoundExpressionReason.ArgumentBindingFailed);
        }

        if (SymbolEqualityComparer.Default.Equals(literal.Type, parameterType))
            return literal;

        var conversion = Compilation.ClassifyConversion(literal.Type!, parameterType);
        if (!conversion.Exists || !conversion.IsImplicit)
        {
            ReportOptionalParameterDefaultValueCannotConvert(parameter, parameterType);
            return new BoundErrorExpression(parameterType, null, BoundExpressionReason.ArgumentBindingFailed);
        }

        return ApplyConversion(literal, parameterType, conversion);
    }

    private void ReportOptionalParameterDefaultValueCannotConvert(IParameterSymbol parameter, ITypeSymbol parameterType)
    {
        var parameterTypeDisplay = parameterType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);
        var parameterName = parameter.Name ?? string.Empty;
        var location = parameter.Locations.FirstOrDefault() ?? Location.None;

        _diagnostics.ReportOptionalParameterDefaultValueCannotConvert(parameterName, parameterTypeDisplay, location);
    }

    private bool TryCreateOptionalLiteral(ITypeSymbol parameterType, object value, out BoundLiteralExpression literal)
    {
        if (parameterType.TypeKind == TypeKind.Enum &&
            TryConvertEnumLiteralValue(value, out var enumValue))
        {
            literal = new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, enumValue, parameterType);
            return true;
        }

        if (!ConstantValueEvaluator.TryConvert(parameterType, value, out var converted))
        {
            literal = null!;
            return false;
        }

        var literalType = GetOptionalLiteralType(parameterType, converted);
        if (literalType is null)
        {
            literal = null!;
            return false;
        }

        literal = new BoundLiteralExpression(GetOptionalLiteralKind(converted!), converted!, literalType);
        return true;
    }

    private ITypeSymbol? GetOptionalLiteralType(ITypeSymbol parameterType, object value)
    {
        return value switch
        {
            bool => Compilation.GetSpecialType(SpecialType.System_Boolean),
            string => Compilation.GetSpecialType(SpecialType.System_String),
            char => Compilation.GetSpecialType(SpecialType.System_Char),
            sbyte => Compilation.GetSpecialType(SpecialType.System_SByte),
            byte => Compilation.GetSpecialType(SpecialType.System_Byte),
            short => Compilation.GetSpecialType(SpecialType.System_Int16),
            ushort => Compilation.GetSpecialType(SpecialType.System_UInt16),
            int => Compilation.GetSpecialType(SpecialType.System_Int32),
            uint => Compilation.GetSpecialType(SpecialType.System_UInt32),
            long => Compilation.GetSpecialType(SpecialType.System_Int64),
            ulong => Compilation.GetSpecialType(SpecialType.System_UInt64),
            float => Compilation.GetSpecialType(SpecialType.System_Single),
            double => Compilation.GetSpecialType(SpecialType.System_Double),
            decimal => Compilation.GetSpecialType(SpecialType.System_Decimal),
            DateTime => Compilation.GetSpecialType(SpecialType.System_DateTime),
            Enum => parameterType,
            _ => parameterType
        };
    }

    private static bool TryConvertEnumLiteralValue(object value, out object converted)
    {
        switch (value)
        {
            case Enum enumValue:
                converted = Convert.ToInt64(enumValue);
                return true;
            case byte or sbyte or short or ushort or int or uint or long or ulong:
                converted = value;
                return true;
            default:
                converted = null!;
                return false;
        }
    }

    private static BoundLiteralExpressionKind GetOptionalLiteralKind(object value)
    {
        return value switch
        {
            bool b => b ? BoundLiteralExpressionKind.TrueLiteral : BoundLiteralExpressionKind.FalseLiteral,
            string => BoundLiteralExpressionKind.StringLiteral,
            char => BoundLiteralExpressionKind.CharLiteral,
            _ => BoundLiteralExpressionKind.NumericLiteral
        };
    }

    private BoundExpression[] ConvertInvocationArguments(
        IMethodSymbol method,
        BoundArgument[] invocationArguments,
        BoundExpression? extensionReceiver,
        SyntaxNode receiverSyntax,
        out BoundExpression? convertedExtensionReceiver)
    {
        convertedExtensionReceiver = null;

        if (method.IsExtensionMethod && IsExtensionReceiver(extensionReceiver))
        {
            var parameters = method.Parameters;
            convertedExtensionReceiver = ConvertSingleArgument(
                extensionReceiver!,
                parameters[0],
                receiverSyntax);

            if (parameters.Length == 1)
                return Array.Empty<BoundExpression>();

            return ConvertArguments(parameters.RemoveAt(0), invocationArguments);
        }

        return ConvertArguments(method.Parameters, invocationArguments);
    }

    private BoundExpression ConvertSingleArgument(BoundExpression argument, IParameterSymbol parameter, SyntaxNode syntax)
    {
        if (parameter.RefKind is RefKind.Ref or RefKind.Out or RefKind.In)
            return argument;

        if (!ShouldAttemptConversion(argument) ||
            parameter.Type.TypeKind == TypeKind.Error ||
            argument.Type is null)
        {
            return argument;
        }

        if (!IsAssignable(parameter.Type, argument.Type, out var conversion))
        {
            _diagnostics.ReportCannotConvertFromTypeToType(
                argument.Type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                parameter.Type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                syntax.GetLocation());

            return new BoundErrorExpression(parameter.Type, null, BoundExpressionReason.TypeMismatch);
        }

        return ApplyConversion(argument, parameter.Type, conversion, syntax);
    }

    protected static bool AreArgumentsCompatibleWithMethod(
        IMethodSymbol method,
        int argumentCount,
        BoundExpression? receiver,
        BoundArgument[]? arguments = null)
    {
        var providedCount = argumentCount;

        if (method.IsExtensionMethod && IsExtensionReceiver(receiver))
            providedCount++;

        if (!SupportsArgumentCount(method.Parameters, providedCount))
            return false;

        if (arguments is null)
            return true;

        return OverloadResolver.TryMapArguments(
            method.Parameters,
            arguments,
            method.IsExtensionMethod && IsExtensionReceiver(receiver),
            out _);
    }

    protected static bool SupportsArgumentCount(ImmutableArray<IParameterSymbol> parameters, int providedCount)
    {
        if (providedCount > parameters.Length)
            return false;

        var required = GetRequiredParameterCount(parameters);
        return providedCount >= required;
    }

    protected static int GetRequiredParameterCount(ImmutableArray<IParameterSymbol> parameters)
    {
        var required = parameters.Length;
        while (required > 0 && parameters[required - 1].HasExplicitDefaultValue)
            required--;

        return required;
    }

    private static bool IsExtensionReceiver(BoundExpression? receiver)
    {
        return receiver is not null and not BoundTypeExpression and not BoundNamespaceExpression;
    }

    private readonly struct LambdaRebindKey : IEquatable<LambdaRebindKey>
    {
        public LambdaRebindKey(LambdaExpressionSyntax syntax, INamedTypeSymbol delegateType)
        {
            Syntax = syntax;
            DelegateType = delegateType;
        }

        public LambdaExpressionSyntax Syntax { get; }
        public INamedTypeSymbol DelegateType { get; }

        public bool Equals(LambdaRebindKey other)
        {
            return ReferenceEquals(Syntax, other.Syntax) &&
                   SymbolEqualityComparer.Default.Equals(DelegateType, other.DelegateType);
        }

        public override bool Equals(object? obj)
        {
            return obj is LambdaRebindKey other && Equals(other);
        }

        public override int GetHashCode()
        {
            var hash = ReferenceEqualityComparer.Instance.GetHashCode(Syntax);
            hash = (hash * 397) ^ SymbolEqualityComparer.Default.GetHashCode(DelegateType);
            return hash;
        }
    }

    private static SyntaxNode? GetInvocationReceiverSyntax(InvocationExpressionSyntax invocation)
    {
        return invocation.Expression switch
        {
            MemberAccessExpressionSyntax memberAccess => memberAccess.Expression,
            MemberBindingExpressionSyntax binding when invocation.Parent is ConditionalAccessExpressionSyntax conditional && conditional.WhenNotNull == invocation
                => conditional.Expression,
            _ => null
        };
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

                if (block._labelsByName.TryGetValue(name, out var label) && seen.Add(label))
                    yield return label;
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

            if (current is LambdaBinder lambdaBinder)
            {
                foreach (var param in lambdaBinder.GetParameters())
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

                foreach (var label in block._labelsByName.Values)
                {
                    if (string.IsNullOrEmpty(label.Name))
                        continue;

                    if (seen.Add(label.Name))
                        yield return label;
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

        var methodBinder = functionBinder.GetMethodBodyBinder();

        if (function.Body is not null)
        {
            var blockBinder = (BlockBinder)SemanticModel.GetBinder(function.Body, methodBinder);
            blockBinder.BindBlockStatement(function.Body);
        }
        else if (function.ExpressionBody is not null)
        {
            var expressionBinder = (BlockBinder)SemanticModel.GetBinder(function.ExpressionBody, methodBinder);
            var expression = expressionBinder.BindExpression(function.ExpressionBody.Expression, allowReturn: true);
            var returnType = symbol.ReturnType;
            var unitType = Compilation.GetSpecialType(SpecialType.System_Unit);
            var statements = new List<BoundStatement>(capacity: 1);

            if (symbol is SourceMethodSymbol sourceMethod &&
                sourceMethod.RequiresAsyncReturnTypeInference &&
                !sourceMethod.AsyncReturnTypeInferenceComplete)
            {
                var inferredReturnType = AsyncReturnTypeUtilities.InferAsyncReturnType(Compilation, expression.Type);
                sourceMethod.SetReturnType(inferredReturnType);
                sourceMethod.CompleteAsyncReturnTypeInference();
                returnType = sourceMethod.ReturnType;
            }

            if (SymbolEqualityComparer.Default.Equals(returnType, unitType) || returnType.SpecialType == SpecialType.System_Void)
            {
                var statement = expressionBinder.ExpressionToStatement(expression);
                statements.Add(statement);
            }
            else
            {
                var converted = expression;
                var skipReturnConversions = symbol switch
                {
                    SourceMethodSymbol { HasAsyncReturnTypeError: true } => true,
                    SourceMethodSymbol { ShouldDeferAsyncReturnDiagnostics: true } => true,
                    _ => false,
                };

                if (!skipReturnConversions && converted.Type is not null && ShouldAttemptConversion(converted) &&
                    returnType.TypeKind != TypeKind.Error)
                {
                    if (symbol.IsAsync && returnType.SpecialType == SpecialType.System_Threading_Tasks_Task)
                    {
                        var methodDisplay = symbol.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat);
                        expressionBinder.Diagnostics.ReportAsyncTaskReturnCannotHaveExpression(
                            methodDisplay,
                            function.ExpressionBody.Expression.GetLocation());
                    }
                    else
                    {
                        var targetType = returnType;

                        if (symbol.IsAsync &&
                            AsyncReturnTypeUtilities.ExtractAsyncResultType(Compilation, returnType) is { } awaitedType)
                        {
                            targetType = awaitedType;
                        }

                        if (!expressionBinder.IsAssignable(targetType, converted.Type, out var conversion))
                        {
                            expressionBinder.Diagnostics.ReportCannotConvertFromTypeToType(
                                converted.Type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                                targetType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                                function.ExpressionBody.Expression.GetLocation());
                        }
                        else
                        {
                            converted = expressionBinder.ApplyConversion(converted, targetType, conversion, function.ExpressionBody.Expression);
                        }
                    }
                }

                statements.Add(new BoundReturnStatement(converted));
            }

            var boundBlock = new BoundBlockStatement(statements);

            if (symbol is SourceMethodSymbol { IsAsync: true } asyncMethod)
            {
                var containsAwait = AsyncLowerer.ContainsAwait(boundBlock) ||
                    Compilation.ContainsAwaitExpressionOutsideNestedFunctions(function.ExpressionBody);
                asyncMethod.SetContainsAwait(containsAwait);

                if (!containsAwait)
                {
                    var description = AsyncDiagnosticUtilities.GetAsyncMemberDescription(asyncMethod);
                    var location = AsyncDiagnosticUtilities.GetAsyncKeywordLocation(asyncMethod, function.ExpressionBody);
                    _diagnostics.ReportAsyncLacksAwait(description, location);
                }
            }

            SemanticModel.CacheBoundNode(function.ExpressionBody, boundBlock);
        }

        return new BoundFunctionStatement(symbol); // Possibly include body here if needed
    }
}
