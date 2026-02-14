using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;

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
    private readonly HashSet<ISymbol> _nonNullSymbols = new(SymbolEqualityComparer.Default);
    private readonly Stack<ITypeSymbol?> _targetTypeStack = new();
    private int _scopeDepth;
    private bool _allowReturnsInExpression;
    private bool _allowReturnsInBlockExpressionsOnly;
    private int _loopDepth;
    private int _expressionContextDepth;
    private int _tempCounter;
    private int _objectInitializerDepth;
    private int _withInitializerDepth;
    private int _unsafeBlockDepth;

    private bool IsInObjectInitializer => _objectInitializerDepth > 0;
    private bool IsInWithInitializer => _withInitializerDepth > 0;

    private bool IsInInitOnlyAssignmentContext =>
        IsInObjectInitializer || IsInWithInitializer;

    protected override bool IsInUnsafeContext => _unsafeBlockDepth > 0 || base.IsInUnsafeContext;

    private static bool IsInitOnly(IPropertySymbol property)
        => property.SetMethod?.MethodKind == MethodKind.InitOnly;

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
        var isUseDeclaration = decl.Parent is UseDeclarationStatementSyntax;
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
        var shouldDispose = isUseDeclaration;

        ITypeSymbol type = Compilation.ErrorTypeSymbol;
        BoundExpression? boundInitializer = null;
        ITypeSymbol? initializerValueType = null;
        ITypeSymbol? annotatedType = null;
        var typeLocation = variableDeclarator.TypeAnnotation?.Type.GetLocation()
            ?? bindingKeyword.GetLocation();
        object? constantValue = null;
        if (variableDeclarator.TypeAnnotation is not null)
            annotatedType = ResolveTypeSyntaxOrError(variableDeclarator.TypeAnnotation.Type);
        if (initializer is not null)
        {
            // Initializers evaluate to values, but block expressions can still be used
            // as scoped early-exit helpers. Allow early-exit statements inside block
            // expressions only, while keeping inline expression arms (`if`/`match`)
            // under expression-context restrictions.
            boundInitializer = BindExpressionWithTargetType(
                initializer.Value,
                annotatedType,
                allowReturn: false,
                allowReturnInBlockExpressionsOnly: true);
            initializerValueType = boundInitializer?.Type;
        }

        if (initializer is null)
        {
            if (annotatedType is not null)
                type = annotatedType;

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
            type = annotatedType ?? ResolveTypeSyntaxOrError(variableDeclarator.TypeAnnotation.Type);
        }

        var constantValueComputed = false;

        if (type.TypeKind != TypeKind.Error &&
            initializer is not null &&
            boundInitializer is not null &&
            ShouldAttemptConversion(boundInitializer))
        {
            boundInitializer = BindLambdaToDelegateIfNeeded(boundInitializer, type);
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

        if (isUseDeclaration)
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

    protected override bool EnsureMemberAccessible(ISymbol symbol, Location location, string symbolKind)
    {
        if (base.EnsureMemberAccessible(symbol, location, symbolKind))
            return true;

        var display = symbol is ITypeSymbol typeSymbol
            ? typeSymbol.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat)
            : symbol.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat);

        _diagnostics.ReportSymbolIsInaccessible(symbolKind, display, location);
        return false;
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
            var bound = BindTypeSyntaxAsExpression(argument.Type);
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
            IEventSymbol => "event",
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
            UseDeclarationStatementSyntax useDeclaration => BindUseDeclarationStatement(useDeclaration),
            AssignmentStatementSyntax assignmentStatement => BindAssignmentStatement(assignmentStatement),
            MatchStatementSyntax matchStatement => BindMatchStatement(matchStatement),
            ExpressionStatementSyntax expressionStmt => BindExpressionStatement(expressionStmt),
            IfStatementSyntax ifStmt => BindIfStatement(ifStmt),
            WhileStatementSyntax whileStmt => BindWhileStatement(whileStmt),
            TryStatementSyntax tryStmt => BindTryStatement(tryStmt),
            FunctionStatementSyntax function => BindFunction(function),
            ReturnStatementSyntax returnStatement => BindReturnStatement(returnStatement),
            ThrowStatementSyntax throwStatement => BindThrowStatement(throwStatement),
            BlockStatementSyntax blockStmt => BindBlockStatement(blockStmt),
            UnsafeStatementSyntax unsafeStatement => BindUnsafeStatement(unsafeStatement),
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

    private BoundStatement BindUnsafeStatement(UnsafeStatementSyntax unsafeStatement)
    {
        _unsafeBlockDepth++;
        try
        {
            return BindBlockStatement(unsafeStatement.Block);
        }
        finally
        {
            _unsafeBlockDepth--;
        }
    }

    private BoundStatement BindLocalDeclarationStatement(LocalDeclarationStatementSyntax localDeclaration)
    {
        var declaration = localDeclaration.Declaration;

        if ((declaration.BindingKeyword.IsKind(SyntaxKind.LetKeyword) || declaration.BindingKeyword.IsKind(SyntaxKind.ValKeyword)) &&
            declaration.Declarators.Count > 0 &&
            IsDiscardDeclarator(declaration.Declarators[0]))
        {
            return BindDiscardDeclarator(declaration.Declarators[0], isUseDeclaration: false);
        }

        var boundDeclarators = ImmutableArray.CreateBuilder<BoundVariableDeclarator>(declaration.Declarators.Count);

        foreach (var declarator in declaration.Declarators)
        {
            boundDeclarators.Add(BindLocalDeclaration(declarator));
        }

        return new BoundLocalDeclarationStatement(boundDeclarators.ToImmutable(), isUsing: false);
    }

    private BoundStatement BindUseDeclarationStatement(UseDeclarationStatementSyntax useDeclaration)
    {
        var declaration = useDeclaration.Declaration;

        if (declaration.Declarators.Count > 0 &&
            IsDiscardDeclarator(declaration.Declarators[0]))
        {
            return BindDiscardDeclarator(declaration.Declarators[0], isUseDeclaration: true);
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
        bool isUseDeclaration)
    {
        var initializer = variableDeclarator.Initializer;

        if (initializer is null)
        {
            _diagnostics.ReportLocalVariableMustBeInitialized("_", variableDeclarator.Identifier.GetLocation());
            return new BoundExpressionStatement(BoundFactory.UnitExpression());
        }

        var boundInitializer = BindExpression(
            initializer.Value,
            allowReturn: false,
            allowReturnInBlockExpressionsOnly: true);

        if (variableDeclarator.TypeAnnotation is not null)
        {
            var annotatedType = ResolveTypeSyntaxOrError(variableDeclarator.TypeAnnotation.Type);
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

        if (isUseDeclaration)
        {
            _diagnostics.ReportDiscardExpressionNotAllowed(variableDeclarator.Identifier.GetLocation());
        }

        return new BoundExpressionStatement(boundInitializer);
    }

    public BoundExpression BindExpression(ExpressionSyntax syntax, bool allowReturn, bool allowReturnInBlockExpressionsOnly = false)
    {
        var previousAllowReturn = _allowReturnsInExpression;
        var previousAllowReturnInBlocksOnly = _allowReturnsInBlockExpressionsOnly;
        _allowReturnsInExpression = allowReturn;
        _allowReturnsInBlockExpressionsOnly = allowReturnInBlockExpressionsOnly;
        try
        {
            return BindExpression(syntax);
        }
        finally
        {
            _allowReturnsInExpression = previousAllowReturn;
            _allowReturnsInBlockExpressionsOnly = previousAllowReturnInBlocksOnly;
        }
    }

    public TargetTypeScope PushTargetType(ITypeSymbol? targetType)
    {
        _targetTypeStack.Push(targetType);
        return new TargetTypeScope(this);
    }

    private BoundExpression BindExpressionWithTargetType(
        ExpressionSyntax syntax,
        ITypeSymbol? targetType,
        bool allowReturn = true,
        bool allowReturnInBlockExpressionsOnly = false)
    {
        if (targetType is null)
            return BindExpression(syntax, allowReturn, allowReturnInBlockExpressionsOnly);

        using var _ = PushTargetType(targetType);
        return BindExpression(syntax, allowReturn, allowReturnInBlockExpressionsOnly);
    }

    public override BoundExpression BindExpression(ExpressionSyntax syntax)
    {
        if (TryGetCachedBoundNode(syntax) is BoundExpression cached)
            return cached;

        var boundNode = syntax switch
        {
            LiteralExpressionSyntax literal => BindLiteralExpression(literal),
            IdentifierNameSyntax identifier => BindIdentifierName(identifier),
            TypeSyntax type => BindTypeSyntaxAsExpression(type),
            BinaryExpressionSyntax binary => BindBinaryExpression(binary),
            NullCoalesceExpressionSyntax coalesce => BindNullCoalesceExpression(coalesce),
            RangeExpressionSyntax rangeExpression => BindRangeExpression(rangeExpression),
            InvocationExpressionSyntax invocation => BindInvocationExpression(invocation),
            ObjectCreationExpressionSyntax invocation => BindObjectCreationExpression(invocation),
            WithExpressionSyntax withExpression => BindWithExpression(withExpression),
            MemberAccessExpressionSyntax memberAccess => BindMemberAccessExpression(memberAccess),
            MemberBindingExpressionSyntax memberBinding => BindMemberBindingExpression(memberBinding),
            ConditionalAccessExpressionSyntax conditionalAccess => BindConditionalAccessExpression(conditionalAccess),
            ElementAccessExpressionSyntax elementAccess => BindElementAccessExpression(elementAccess),
            AssignmentExpressionSyntax assignment => BindAssignmentExpression(assignment),
            CollectionExpressionSyntax collection => BindCollectionExpression(collection),
            ParenthesizedExpressionSyntax parenthesizedExpression => BindParenthesizedExpression(parenthesizedExpression),
            CastExpressionSyntax castExpression => BindConversionExpression(castExpression),
            AsExpressionSyntax asExpression => BindAsExpression(asExpression),
            DefaultExpressionSyntax defaultExpression => BindDefaultExpression(defaultExpression),
            TypeOfExpressionSyntax typeOfExpression => BindTypeOfExpression(typeOfExpression),
            SizeOfExpressionSyntax sizeOfExpression => BindSizeOfExpression(sizeOfExpression),
            NameOfExpressionSyntax nameOfExpression => BindNameOfExpression(nameOfExpression),
            TupleExpressionSyntax tupleExpression => BindTupleExpression(tupleExpression),
            IfExpressionSyntax ifExpression => BindIfExpression(ifExpression),
            BlockSyntax block => BindBlock(block, allowReturn: _allowReturnsInExpression || _allowReturnsInBlockExpressionsOnly),
            IsPatternExpressionSyntax isPatternExpression => BindIsPatternExpression(isPatternExpression),
            MatchExpressionSyntax matchExpression => BindMatchExpression(matchExpression),
            TryExpressionSyntax tryExpression => BindTryExpression(tryExpression),
            ReturnExpressionSyntax returnExpression => BindReturnExpression(returnExpression),
            ThrowExpressionSyntax throwExpression => BindThrowExpression(throwExpression),
            PropagateExpressionSyntax propagateExpression => BindPropagateExpression(propagateExpression),
            LambdaExpressionSyntax lambdaExpression => BindLambdaExpression(lambdaExpression),
            InterpolatedStringExpressionSyntax interpolated => BindInterpolatedStringExpression(interpolated),
            UnaryExpressionSyntax unaryExpression => BindUnaryExpression(unaryExpression),
            PostfixUnaryExpressionSyntax postfixUnary => BindPostfixUnaryExpression(postfixUnary),
            IndexExpressionSyntax indexExpression => BindIndexExpression(indexExpression),
            SelfExpressionSyntax selfExpression => BindSelfExpression(selfExpression),
            DiscardExpressionSyntax discardExpression => BindDiscardExpression(discardExpression),
            UnitExpressionSyntax unitExpression => BindUnitExpression(unitExpression),
            ExpressionSyntax.Missing missing => BindMissingExpression(missing),
            _ => throw new NotSupportedException($"Unsupported expression: {syntax.Kind}")
        };

        // NOTE: Might want to revert
        CacheBoundNode(syntax, boundNode);

        return boundNode;
    }

    private BoundExpression BindNullCoalesceExpression(NullCoalesceExpressionSyntax coalesce)
    {
        var left = BindExpression(coalesce.Left);

        if (left is BoundErrorExpression)
            return left;

        var leftType = left.Type ?? Compilation.ErrorTypeSymbol;

        // If the left is nullable, the result is typically the non-nullable left type
        // combined with the right type.
        var leftNonNullable = leftType.GetPlainType();
        var right = BindExpressionWithTargetType(coalesce.Right, leftNonNullable);

        if (right is BoundErrorExpression)
            return right;

        var rightType = right.Type ?? Compilation.ErrorTypeSymbol;
        var rightNeverCompletes = IsEarlyExitExpression(right, coalesce.Right);

        var resultType = rightNeverCompletes
            ? leftNonNullable
            : TypeSymbolNormalization.NormalizeUnion(new[] { leftNonNullable, rightType });

        // Ensure the RHS can be used as the result type.
        if (!rightNeverCompletes)
        {
            right = PrepareRightForAssignment(right, resultType, coalesce.Right);
            if (right is BoundErrorExpression)
                return right;
        }

        // NOTE: We keep `left` as-is (possibly nullable). Codegen will implement the
        // short-circuit null test and produce `resultType`.
        return new BoundNullCoalesceExpression(left, right, resultType);
    }

    private bool IsEarlyExitExpression(BoundExpression expression, ExpressionSyntax syntax)
    {
        if (expression is BoundReturnExpression or BoundThrowExpression)
            return true;

        if (syntax is not BlockSyntax block)
            return false;

        var controlFlow = SemanticModel.AnalyzeControlFlowInternal(block, analyzeJumpPoints: false);
        return controlFlow is { Succeeded: true, EndPointIsReachable: false };
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
            SyntaxKind.CaretEqualsToken => SyntaxKind.CaretToken,
            SyntaxKind.QuestionQuestionEqualsToken => SyntaxKind.QuestionQuestionToken,
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
                var expected = target.TupleElements[i].Type;
                var boundExpr = BindExpressionWithTargetType(arg.Expression, expected);
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

        if (operand is BoundErrorExpression)
            return operand;

        switch (unaryExpression.Kind)
        {
            case SyntaxKind.AddressOfExpression:
                return BindAddressOfExpression(operand, unaryExpression);

            case SyntaxKind.DereferenceExpression:
                return BindDereferenceExpression(operand, unaryExpression);

            case SyntaxKind.LogicalNotExpression:
            case SyntaxKind.BitwiseNotExpression:
            case SyntaxKind.UnaryMinusExpression:
            case SyntaxKind.UnaryPlusExpression:
                {
                    var opKind = unaryExpression.OperatorToken.Kind;
                    var operandType = operand.Type ?? Compilation.ErrorTypeSymbol;
                    var userDefined = BindUserDefinedUnaryOperator(opKind, operand, unaryExpression.OperatorToken.GetLocation(), unaryExpression.Expression, unaryExpression);
                    if (userDefined is not null)
                        return userDefined;

                    if (BoundUnaryOperator.TryLookup(Compilation, opKind, operandType, out var op))
                        return new BoundUnaryExpression(op, operand);

                    var operatorText = SyntaxFacts.GetSyntaxTokenText(opKind) ?? opKind.ToString();
                    _diagnostics.ReportOperatorCannotBeAppliedToOperandOfType(
                        operatorText,
                        operandType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        unaryExpression.OperatorToken.GetLocation());
                    return ErrorExpression(reason: BoundExpressionReason.NotFound);
                }

            default:
                throw new NotSupportedException("Unsupported unary expression");
        }
    }

    private BoundExpression BindPostfixUnaryExpression(PostfixUnaryExpressionSyntax postfixUnary)
    {
        var binaryOperatorKind = postfixUnary.OperatorToken.Kind == SyntaxKind.PlusPlusToken
            ? SyntaxKind.PlusToken
            : SyntaxKind.MinusToken;

        return BindIncrementOrDecrement(postfixUnary.Expression, postfixUnary.OperatorToken, binaryOperatorKind, isPostfix: true);
    }

    private BoundExpression BindIndexExpression(IndexExpressionSyntax indexExpression)
    {
        var value = BindExpression(indexExpression.Expression);

        if (IsErrorExpression(value))
            return AsErrorExpression(value);

        var intType = Compilation.GetSpecialType(SpecialType.System_Int32);

        if (intType.TypeKind != TypeKind.Error && ShouldAttemptConversion(value))
        {
            var sourceType = value.Type ?? Compilation.ErrorTypeSymbol;

            if (!IsAssignable(intType, sourceType, out var conversion))
            {
                _diagnostics.ReportCannotConvertFromTypeToType(
                    sourceType.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    intType.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    indexExpression.Expression.GetLocation());

                return new BoundErrorExpression(intType, null, BoundExpressionReason.TypeMismatch);
            }

            value = ApplyConversion(value, intType, conversion, indexExpression.Expression);
        }

        return new BoundIndexExpression(value, isFromEnd: true, GetIndexType());
    }

    private BoundExpression BindRangeExpression(RangeExpressionSyntax rangeExpression)
    {
        var start = BindRangeEndpoint(rangeExpression.LeftExpression);
        if (start is BoundErrorExpression errorStart)
            return errorStart;

        var end = BindRangeEndpoint(rangeExpression.RightExpression);
        if (end is BoundErrorExpression errorEnd)
            return errorEnd;

        return new BoundRangeExpression(
            start as BoundIndexExpression,
            end as BoundIndexExpression,
            GetRangeType());
    }

    private BoundExpression? BindRangeEndpoint(ExpressionSyntax? endpointSyntax)
    {
        if (endpointSyntax is null)
            return null;

        var bound = BindExpression(endpointSyntax);

        if (IsErrorExpression(bound))
            return bound;

        if (bound is BoundIndexExpression index)
            return index;

        var intType = Compilation.GetSpecialType(SpecialType.System_Int32);

        if (intType.TypeKind != TypeKind.Error && ShouldAttemptConversion(bound))
        {
            var sourceType = bound.Type ?? Compilation.ErrorTypeSymbol;

            if (!IsAssignable(intType, sourceType, out var conversion))
            {
                _diagnostics.ReportCannotConvertFromTypeToType(
                    sourceType.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    intType.ToDisplayStringForTypeMismatchDiagnostic(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    endpointSyntax.GetLocation());

                return new BoundErrorExpression(intType, null, BoundExpressionReason.TypeMismatch);
            }

            bound = ApplyConversion(bound, intType, conversion, endpointSyntax);
        }

        return new BoundIndexExpression(bound, isFromEnd: false, GetIndexType());
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
                operatorToken.Kind,
                isPostfix,
                value => BoundFactory.CreateArrayAssignmentExpression(arrayAccess, value)),
            BoundIndexerAccessExpression indexerAccess => BindIncrementCore(
                operandSyntax,
                operand,
                indexerAccess.Type ?? Compilation.ErrorTypeSymbol,
                binaryOperatorKind,
                operatorToken.Kind,
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
            binaryOperatorKind == SyntaxKind.PlusToken ? SyntaxKind.PlusPlusToken : SyntaxKind.MinusMinusToken,
            isPostfix,
            value => localType is RefTypeSymbol refType
                ? BoundFactory.CreateByRefAssignmentExpression(localAccess, refType.ElementType, value)
                : BoundFactory.CreateLocalAssignmentExpression(localSymbol, localAccess, value));
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
            binaryOperatorKind == SyntaxKind.PlusToken ? SyntaxKind.PlusPlusToken : SyntaxKind.MinusMinusToken,
            isPostfix,
            value => parameterSymbol.RefKind is RefKind.Ref or RefKind.Out
                ? BoundFactory.CreateByRefAssignmentExpression(parameterAccess, parameterSymbol.GetByRefElementType(), value)
                : BoundFactory.CreateParameterAssignmentExpression(parameterSymbol, parameterAccess, value));
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
            binaryOperatorKind == SyntaxKind.PlusToken ? SyntaxKind.PlusPlusToken : SyntaxKind.MinusMinusToken,
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
                binaryOperatorKind == SyntaxKind.PlusToken ? SyntaxKind.PlusPlusToken : SyntaxKind.MinusMinusToken,
                isPostfix,
                value => CreateFieldAssignmentExpression(memberAccess.Receiver, backingField, value));
        }

        return BindIncrementCore(
            operandSyntax,
            memberAccess,
            propertySymbol.Type,
            binaryOperatorKind,
            binaryOperatorKind == SyntaxKind.PlusToken ? SyntaxKind.PlusPlusToken : SyntaxKind.MinusMinusToken,
            isPostfix,
            value => BoundFactory.CreatePropertyAssignmentExpression(memberAccess.Receiver, propertySymbol, value));
    }

    private BoundExpression BindIncrementCore(
        ExpressionSyntax operandSyntax,
        BoundExpression valueExpression,
        ITypeSymbol targetType,
        SyntaxKind binaryOperatorKind,
        SyntaxKind incrementOperatorKind,
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

        var updatedValue = BindUserDefinedUnaryOperator(
            incrementOperatorKind,
            tempAccess,
            operandSyntax.GetLocation(),
            operandSyntax,
            operandSyntax);

        if (updatedValue is null)
        {
            var stepLiteral = CreateStepLiteral();
            var preparedStep = PrepareRightForAssignment(stepLiteral, targetType, operandSyntax);

            if (preparedStep is BoundErrorExpression)
                return preparedStep;

            updatedValue = BindBinaryExpression(binaryOperatorKind, tempAccess, preparedStep, operandSyntax.GetLocation());
        }
        else if (updatedValue is BoundErrorExpression)
        {
            return updatedValue;
        }

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
        return type is RefTypeSymbol refType ? refType.ElementType : type;
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
            return ErrorExpression(reason: BoundExpressionReason.UnsupportedOperation);

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

    private BoundExpression BindDereferenceExpression(BoundExpression operand, UnaryExpressionSyntax syntax)
    {
        if (operand is BoundErrorExpression)
            return operand;

        if (!IsUnsafeEnabled)
        {
            _diagnostics.ReportPointerOperationRequiresUnsafe(syntax.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.UnsupportedOperation);
        }

        var operandType = operand.Type ?? Compilation.ErrorTypeSymbol;
        var elementType = operandType switch
        {
            IPointerTypeSymbol pointer => pointer.PointedAtType,
            IAddressTypeSymbol address => address.ReferencedType,
            RefTypeSymbol refType => refType.ElementType,
            _ => null,
        };

        if (elementType is null)
        {
            _diagnostics.ReportDereferenceRequiresPointerOrByRef(
                operandType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                syntax.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.TypeMismatch);
        }

        return new BoundDereferenceExpression(operand, elementType);
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

    private BoundExpression BindConversionExpression(CastExpressionSyntax castExpression)
    {
        var expression = BindExpression(castExpression.Expression);
        var targetType = ResolveTypeSyntaxOrError(castExpression.Type);

        if (HasExpressionErrors(expression))
            return expression;
        if (targetType.ContainsErrorType())
            return ErrorExpression(targetType, reason: BoundExpressionReason.NotFound);

        var conversion = Compilation.ClassifyConversion(expression.Type!, targetType);
        if (!conversion.Exists)
        {
            _diagnostics.ReportCannotConvertFromTypeToType(
                expression.Type!.ToDisplayStringForDiagnostics(SymbolDisplayFormat.MinimallyQualifiedFormat),
                targetType.ToDisplayStringForDiagnostics(SymbolDisplayFormat.MinimallyQualifiedFormat),
                castExpression.GetLocation());
            return new BoundErrorExpression(targetType, null, BoundExpressionReason.TypeMismatch);
        }

        return new BoundConversionExpression(expression, targetType, conversion);
    }

    private BoundExpression BindTypeOfExpression(TypeOfExpressionSyntax typeOfExpression)
    {
        var operandType = ResolveTypeSyntaxOrError(typeOfExpression.Type);

        if (operandType.ContainsErrorType())
            return ErrorExpression(operandType, reason: BoundExpressionReason.NotFound);

        var systemType = Compilation.GetSpecialType(SpecialType.System_Type);

        return new BoundTypeOfExpression(operandType, systemType);
    }

    private BoundExpression BindSizeOfExpression(SizeOfExpressionSyntax sizeOfExpression)
    {
        var operandType = ResolveTypeSyntaxOrError(sizeOfExpression.Type);

        if (operandType.ContainsErrorType())
            return ErrorExpression(operandType, reason: BoundExpressionReason.NotFound);

        var intType = Compilation.GetSpecialType(SpecialType.System_Int32);

        return new BoundTypeOfExpression(operandType, intType);
    }

    private BoundExpression BindNameOfExpression(NameOfExpressionSyntax nameOfExpression)
    {
        // `nameof` is a compile-time-only operation. We still bind the operand so normal
        // name lookup, overload resolution and diagnostics apply, but the operand is not
        // evaluated at runtime.

        var operand = nameOfExpression.Operand;

        ISymbol? symbol = ResolveNameOfSymbol(nameOfExpression.Operand);

        if (symbol is null)
        {
            // Report the most helpful missing name. For qualified/member-access chains we
            // report the left-most identifier so users see "Foo" rather than "Foo.Bar.Baz".
            var leftMost = GetLeftMostNameNode(operand);

            var nameText = leftMost switch
            {
                IdentifierNameSyntax id => id.Identifier.ValueText,
                GenericNameSyntax gen => gen.Identifier.ValueText,
                _ => leftMost.ToString()
            };

            var location = leftMost.GetLocation() ?? nameOfExpression.GetLocation();

            _diagnostics.ReportTheNameDoesNotExistInTheCurrentContext(nameText, location);

            return ErrorExpression(null, symbol, reason: BoundExpressionReason.NotFound);
        }

        static SyntaxNode GetLeftMostNameNode(ExpressionSyntax operand)
        {
            // Handles:
            //  - nameof(x)
            //  - nameof(List<int>)
            //  - nameof(Console.WriteLine)
            //  - nameof(System.Console.WriteLine)
            //  - nameof(Foo.Console.WriteLine)

            SyntaxNode current = operand;

            // Walk down expression member access: A.B.C => A
            while (current is MemberAccessExpressionSyntax ma)
                current = ma.Expression;

            // Walk down qualified names: A.B.C => A
            while (current is QualifiedNameSyntax qn)
                current = qn.Left;

            // Some names may still be wrapped (e.g. parenthesized) though `nameof` operand is restricted.
            while (current is ParenthesizedExpressionSyntax paren)
                current = paren.Expression;

            return current;
        }

        var stringType = Compilation.GetSpecialType(SpecialType.System_String);
        return new BoundNameOfExpression(symbol, stringType);
    }

    private ISymbol? ResolveNameOfSymbol(ExpressionSyntax operand)
    {
        using var nonReportingScope = Diagnostics.CreateNonReportingScope();

        // `nameof` is special: we want to resolve *members* (method groups, fields, properties, events)
        // even when the parser produced a name node (QualifiedName) rather than a member-access node.
        // Prefer expression resolution first, then fall back to type resolution.

        // 1) If the operand is a qualified name, interpret it as a member-access expression chain.
        //    This makes `nameof(System.Console.WriteLine)` resolve to `WriteLine`, not `Console`.
        if (operand is QualifiedNameSyntax qn)
        {
            if (TryBindTypeSyntaxAsMemberAccessExpression(qn, out var boundExpression)
                && boundExpression is not BoundErrorExpression)
            {
                // For nameof we want the *symbol* represented by the final segment, not the expression's resulting type.
                // - Method groups: pick the selected method (or first candidate)
                // - Member access: the accessed symbol
                // - Type/namespace expressions: the type/namespace symbol
                if (boundExpression is BoundMethodGroupExpression mg)
                    return mg.SelectedMethod ?? mg.Methods.FirstOrDefault();

                var info = boundExpression.GetSymbolInfo();
                if (info.Symbol is not null)
                    return info.Symbol;

                if (boundExpression is BoundTypeExpression bt)
                    return bt.Type;

                if (boundExpression is BoundNamespaceExpression nsExpr)
                    return nsExpr.Namespace;
            }
        }

        // 2) Try expression binding (locals, parameters, and member access).
        {
            var sym = ResolveExpression(operand);
            if (sym is not null)
                return sym;
        }

        // 3) Fall back to type resolution for pure type operands like `List<int>`.
        if (operand is TypeSyntax typeSyntax)
        {
            if (TryBindTypeSyntaxAsMemberAccessExpression(typeSyntax, out var resolvedExpression))
            {
                return resolvedExpression.GetSymbolInfo().Symbol;
            }
        }

        return null;
    }

    private static ExpressionSyntax? ConvertQualifiedNameToMemberAccess(QualifiedNameSyntax qn)
    {
        // Convert A.B.C (QualifiedName chain) into ((A).B).C (member access chain).
        ExpressionSyntax? leftExpr = qn.Left switch
        {
            QualifiedNameSyntax nested => ConvertQualifiedNameToMemberAccess(nested),
            ExpressionSyntax e => e,
            _ => null
        };

        if (leftExpr is null)
            return null;

        // Right is usually IdentifierNameSyntax / GenericNameSyntax (SimpleNameSyntax).
        if (qn.Right is not SimpleNameSyntax right)
            return null;

        return SyntaxFactory.MemberAccessExpression(
            SyntaxKind.SimpleMemberAccessExpression,
            leftExpr,
            qn.DotToken,
            right);
    }

    private ISymbol? ResolveExpression(ExpressionSyntax exprSyntax)
    {
        // Bind the expression purely for symbol lookup and diagnostics.
        var bound = BindExpression(exprSyntax, allowReturn: false);

        // Method-group binding doesn't always populate SymbolInfo.Symbol (it can be ambiguous).
        // For `nameof(Console.WriteLine)` we still want the member name even if overloaded.
        if (bound is BoundMethodGroupExpression mg0)
            return mg0.SelectedMethod ?? mg0.Methods.FirstOrDefault();

        var info = bound.GetSymbolInfo();
        if (info.Symbol is not null)
            return info.Symbol;

        // Dotted chain: `nameof(Console.WriteLine)` or `nameof(Collections.Generic.List)`
        // Walk the member-access chain and try prefixes as `System.<prefix>` type names.
        if (exprSyntax is MemberAccessExpressionSyntax or QualifiedNameSyntax)
        {
            var parts = CollectDottedNameParts(exprSyntax);
            if (parts.Length >= 2)
            {
                // `parts` includes both the left-most identifier and the final member name.
                // Try prefixes excluding the final member, longest-first.
                for (int prefixLen = parts.Length - 1; prefixLen >= 1; prefixLen--)
                {
                    var typeName = "System." + string.Join(".", parts.Take(prefixLen));
                    var systemType = Compilation.GetTypeByMetadataName(typeName);
                    if (systemType is null || systemType.TypeKind == TypeKind.Error)
                        continue;

                    // Re-bind the remaining segments as member accesses on that type.
                    BoundExpression current = new BoundTypeExpression(systemType);

                    for (int i = prefixLen; i < parts.Length; i++)
                    {
                        var seg = parts[i];
                        var nameSyntax = SyntaxFactory.IdentifierName(seg);

                        var member = BindMemberAccessOnReceiver(
                            current,
                            nameSyntax,
                            // Prefer methods on the final segment so `Console.WriteLine` binds as a method group.
                            preferMethods: i == parts.Length - 1,
                            allowEventAccess: true,
                            suppressNullWarning: true,
                            receiverTypeForLookup: current.Type,
                            forceExtensionReceiver: true);

                        if (member is BoundMethodGroupExpression mg)
                            return mg.SelectedMethod ?? mg.Methods.FirstOrDefault();

                        var memberInfo = member.GetSymbolInfo();
                        if (memberInfo.Symbol is not null)
                        {
                            if (i == parts.Length - 1)
                                return memberInfo.Symbol;

                            // Continue chaining using a value-producing bound expression.
                            current = member;
                            continue;
                        }

                        // If we can't bind a segment, abandon this prefix.
                        current = null!;
                        break;
                    }

                    // If we successfully walked the chain, the loop would have returned.
                }
            }
        }

        return null;

        static SyntaxToken[] CollectDottedNameParts(ExpressionSyntax expr)
        {
            // Collect names from either an expression member-access chain `A.B.C`
            // or a qualified-name chain `A.B.C`.

            var names = new List<SyntaxToken>();

            void AddQualified(NameSyntax name)
            {
                switch (name)
                {
                    case QualifiedNameSyntax q:
                        AddQualified(q.Left);
                        AddQualified(q.Right);
                        break;
                    case IdentifierNameSyntax id:
                        names.Add(id.Identifier);
                        break;
                    case GenericNameSyntax gen:
                        names.Add(gen.Identifier);
                        break;
                    default:
                        //names.Add(name.ToString());
                        break;
                }
            }

            void AddExpression(ExpressionSyntax e)
            {
                switch (e)
                {
                    case MemberAccessExpressionSyntax ma:
                        AddExpression(ma.Expression);
                        // ma.Name is a SimpleNameSyntax
                        names.Add(ma.Name.Identifier);
                        break;
                    case IdentifierNameSyntax id:
                        names.Add(id.Identifier);
                        break;
                    case GenericNameSyntax gen:
                        names.Add(gen.Identifier);
                        break;
                    case QualifiedNameSyntax q:
                        AddQualified(q);
                        break;
                    default:
                        //names.Add(e.ToString());
                        break;
                }
            }

            AddExpression(expr);
            return names.ToArray();
        }
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
            var resolution = ResolveMethodGroupOverloadForDelegate(compatibleMethods, invoke, methodGroup.Receiver);

            if (resolution.Success)
            {
                selectedMethod = resolution.Method;
            }
            else if (resolution.IsAmbiguous)
            {
                selectedMethod = null;
                computedReason = BoundExpressionReason.Ambiguous;
            }
            else
            {
                selectedMethod = null;
                computedReason = BoundExpressionReason.OverloadResolutionFailed;
            }
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

    private OverloadResolutionResult ResolveMethodGroupOverloadForDelegate(
        ImmutableArray<IMethodSymbol> methods,
        IMethodSymbol invoke,
        BoundExpression? receiver)
    {
        var arguments = new BoundArgument[invoke.Parameters.Length];
        for (var i = 0; i < invoke.Parameters.Length; i++)
        {
            var parameter = invoke.Parameters[i];
            var expression = new BoundDefaultValueExpression(parameter.Type);
            arguments[i] = new BoundArgument(expression, parameter.RefKind, name: null);
        }

        return OverloadResolver.ResolveOverload(methods, arguments, Compilation, receiver: receiver);
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
        var targetType = ResolveTypeSyntaxOrError(asExpression.Type);

        if (HasExpressionErrors(expression))
            return expression;
        if (targetType.ContainsErrorType())
            return ErrorExpression(targetType.MakeNullable(), reason: BoundExpressionReason.NotFound);

        if (expression.Type!.IsValueType || targetType.IsValueType)
        {
            _diagnostics.ReportCannotConvertFromTypeToType(
                expression.Type!.ToDisplayStringForDiagnostics(SymbolDisplayFormat.MinimallyQualifiedFormat),
                targetType.ToDisplayStringForDiagnostics(SymbolDisplayFormat.MinimallyQualifiedFormat),
                asExpression.GetLocation());
            var errorType = targetType.MakeNullable();
            return new BoundErrorExpression(errorType, null, BoundExpressionReason.TypeMismatch);
        }

        var conversion = Compilation.ClassifyConversion(expression.Type!, targetType);
        if (!conversion.Exists || conversion.IsNumeric || conversion.IsUserDefined)
        {
            _diagnostics.ReportCannotConvertFromTypeToType(
                expression.Type!.ToDisplayStringForDiagnostics(SymbolDisplayFormat.MinimallyQualifiedFormat),
                targetType.ToDisplayStringForDiagnostics(SymbolDisplayFormat.MinimallyQualifiedFormat),
                asExpression.GetLocation());
            var errorType = targetType.MakeNullable();
            return new BoundErrorExpression(errorType, null, BoundExpressionReason.TypeMismatch);
        }

        var resultType = targetType.MakeNullable();
        return new BoundAsExpression(expression, resultType, conversion);
    }

    private BoundStatement BindMatchStatement(MatchStatementSyntax matchStatement)
    {
        var (scrutinee, arms) = BindMatchCommon(
            matchStatement,
            matchStatement.Expression,
            matchStatement.Arms,
            allowReturnInArmExpressions: true,
            requireArmValue: false);
        return new BoundMatchStatement(scrutinee, arms);
    }

    private BoundExpression BindMatchExpression(MatchExpressionSyntax matchExpression)
    {
        var (scrutinee, arms) = BindMatchCommon(
            matchExpression,
            matchExpression.Expression,
            matchExpression.Arms,
            allowReturnInArmExpressions: _allowReturnsInExpression,
            requireArmValue: true);
        var contributingArmTypes = arms
            .Select(arm => arm.Expression)
            .Where(static expression => !IsAbruptMatchArmExpression(expression))
            .Select(expression => expression.Type ?? Compilation.ErrorTypeSymbol)
            .ToArray();

        // If every arm is abrupt (e.g., all return/throw), retain legacy behavior.
        if (contributingArmTypes.Length == 0)
        {
            contributingArmTypes = arms
                .Select(arm => arm.Expression.Type ?? Compilation.ErrorTypeSymbol)
                .ToArray();
        }

        var resultType = TypeSymbolNormalization.NormalizeUnion(contributingArmTypes);

        return new BoundMatchExpression(scrutinee, arms, resultType);

        static bool IsAbruptMatchArmExpression(BoundExpression expression)
        {
            return expression switch
            {
                BoundReturnExpression => true,
                BoundThrowExpression => true,
                BoundRequiredResultExpression { Operand: BoundReturnExpression } => true,
                BoundRequiredResultExpression { Operand: BoundThrowExpression } => true,
                _ => false,
            };
        }
    }

    private (BoundExpression Scrutinee, ImmutableArray<BoundMatchArm> Arms) BindMatchCommon(
        SyntaxNode matchSyntax,
        ExpressionSyntax scrutineeSyntax,
        SyntaxList<MatchArmSyntax> armSyntaxes,
        bool allowReturnInArmExpressions,
        bool requireArmValue)
    {
        var scrutinee = BindExpression(scrutineeSyntax);

        var armBuilder = ImmutableArray.CreateBuilder<BoundMatchArm>();

        foreach (var arm in armSyntaxes)
        {
            _scopeDepth++;
            var depth = _scopeDepth;

            var pattern = BindPattern(arm.Pattern, scrutinee.Type);

            // ✅ Make locals introduced by the pattern visible to `when` + arm expression
            RegisterPatternDesignatorLocals(pattern, depth);

            BoundExpression? guard = null;
            if (arm.WhenClause is { } whenClause)
                guard = BindExpression(whenClause.Condition);

            var expression = BindExpression(arm.Expression, allowReturn: allowReturnInArmExpressions);
            if (requireArmValue)
                expression = expression.RequireValue();

            foreach (var name in _locals.Where(kvp => kvp.Value.Depth == depth).Select(kvp => kvp.Key).ToList())
                _locals.Remove(name);

            _scopeDepth--;

            armBuilder.Add(new BoundMatchArm(pattern, guard, expression));
        }

        var arms = armBuilder.ToImmutable();

        EnsureMatchArmPatternsValid(scrutinee, matchSyntax, armSyntaxes, arms);
        EnsureMatchArmOrder(matchSyntax, armSyntaxes, scrutinee, arms);
        EnsureMatchExhaustive(matchSyntax, armSyntaxes, scrutinee, arms);
        return (scrutinee, arms);

        // Local helper: take locals carried by the bound pattern and register them in this arm scope.
        void RegisterPatternDesignatorLocals(BoundPattern boundPattern, int depth)
        {
            foreach (var designator in boundPattern.GetDesignators())
            {
                if (designator is BoundSingleVariableDesignator single)
                {
                    var local = single.Local;
                    _locals[local.Name] = (local, depth);
                }
            }
        }
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
        var resultDefinition = Compilation.GetTypeByMetadataName("System.Result`2") as INamedTypeSymbol;
        if (resultDefinition is null)
            return ErrorExpression();

        INamedTypeSymbol? resultType = null;

        if (expressionType is INamedTypeSymbol nt && (nt.ConstructedFrom?.Equals(resultDefinition, SymbolEqualityComparer.Default) ?? false))
        {
            resultType = (INamedTypeSymbol?)expressionType;
        }
        else
        {
            resultType = (INamedTypeSymbol?)resultDefinition.Construct(expressionType, exceptionType);
        }

        var union = resultType.TryGetDiscriminatedUnion();
        if (union is null)
            return ErrorExpression();

        var okCase = union.Cases.FirstOrDefault(@case => @case.Name == "Ok");
        var errorCase = union.Cases.FirstOrDefault(@case => @case.Name == "Error");
        if (okCase is null || errorCase is null)
            return ErrorExpression();

        var okConstructor = okCase.Constructors.FirstOrDefault(ctor => ctor.Parameters.Length == okCase.ConstructorParameters.Length);
        var errorConstructor = errorCase.Constructors.FirstOrDefault(ctor => ctor.Parameters.Length == errorCase.ConstructorParameters.Length);
        if (okConstructor is null || errorConstructor is null)
            return ErrorExpression();

        var boundTry = new BoundTryExpression(expression, exceptionType, resultType, okConstructor, errorConstructor);

        if (tryExpression.QuestionToken.Kind != SyntaxKind.None)
        {
            return BindPropagateExpressionCore(boundTry, tryExpression.QuestionToken, tryExpression);
        }

        return boundTry;
    }

    private BoundExpression BindReturnExpression(ReturnExpressionSyntax returnExpression)
    {
        var returnValue = BindReturnValue(returnExpression.Expression, returnExpression);

        var targetType = GetTargetType(returnExpression);
        if (targetType is NullableTypeSymbol nullableTargetType)
            targetType = nullableTargetType.UnderlyingType;

        targetType ??= _containingSymbol switch
        {
            IMethodSymbol method => GetReturnTargetType(method),
            _ => null,
        };

        targetType ??= Compilation.UnitTypeSymbol;

        return new BoundReturnExpression(returnValue, targetType);
    }

    private BoundExpression BindThrowExpression(ThrowExpressionSyntax throwExpression)
    {
        var exceptionBase = Compilation.GetTypeByMetadataName("System.Exception")
            ?? Compilation.ErrorTypeSymbol;

        var expression = BindExpression(throwExpression.Expression);
        expression = BindThrowValueExpression(expression, throwExpression.Expression, exceptionBase);

        var targetType = GetTargetType(throwExpression);
        var resultType = targetType is not null && targetType.TypeKind != TypeKind.Error
            ? targetType
            : Compilation.UnitTypeSymbol;

        return new BoundThrowExpression(expression, resultType);
    }

    private BoundExpression BindPropagateExpression(PropagateExpressionSyntax propagateExpression)
    {
        // Bind the operand in a "pure" expression context: explicit `return` is not allowed here.
        var operand = BindExpression(propagateExpression.Expression, allowReturn: false);

        return BindPropagateExpressionCore(operand, propagateExpression.QuestionToken, propagateExpression);
    }

    private BoundExpression BindPropagateExpressionCore(
        BoundExpression operand,
        SyntaxToken questionToken,
        SyntaxNode callSyntax)
    {
        if (operand is BoundErrorExpression)
            return operand;

        var operandType = UnwrapAlias(operand.Type ?? Compilation.ErrorTypeSymbol);
        if (operandType.TypeKind == TypeKind.Error)
            return ErrorExpression(reason: BoundExpressionReason.TypeMismatch);

        if (operandType is not INamedTypeSymbol operandNamed || !TryGetPropagationInfo(operandNamed, out var operandInfo))
        {
            _diagnostics.ReportOperatorCannotBeAppliedToOperandOfType(
                "?",
                operandType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                questionToken.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.TypeMismatch);
        }

        if (!TryGetEnclosingCarrierReturnType(out var enclosingReturnType))
        {
            _diagnostics.ReportOperatorCannotBeAppliedToOperandOfType(
                "?",
                operandType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                questionToken.GetLocation());

            if (enclosingReturnType is not null && enclosingReturnType.TypeKind != TypeKind.Error)
            {
                _diagnostics.ReportCannotConvertFromTypeToType(
                    enclosingReturnType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    operandInfo.UnionType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    questionToken.GetLocation());
            }

            return ErrorExpression(reason: BoundExpressionReason.UnsupportedOperation);
        }

        if (!TryGetPropagationInfo(enclosingReturnType, out var enclosingInfo) ||
            enclosingInfo.Kind != operandInfo.Kind)
        {
            _diagnostics.ReportOperatorCannotBeAppliedToOperandOfType(
                "?",
                operandType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                questionToken.GetLocation());

            _diagnostics.ReportCannotConvertFromTypeToType(
                operandInfo.UnionType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                enclosingReturnType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                questionToken.GetLocation());

            return ErrorExpression(reason: BoundExpressionReason.UnsupportedOperation);
        }

        var okType = operandInfo.OkPayloadType;
        var errorType = operandInfo.ErrorPayloadType;

        // If the Ok case exposes the payload via a `Value` property, record it.
        IPropertySymbol? okValueProperty = null;
        if (operandInfo.OkCaseType is INamedTypeSymbol okCaseNamed)
        {
            okValueProperty = okCaseNamed.GetMembers("Value").OfType<IPropertySymbol>().FirstOrDefault();
        }

        var enclosingErrorConstructor = enclosingInfo.ErrorCase.Constructors.FirstOrDefault(ctor =>
            ctor.Parameters.Length == enclosingInfo.ErrorCase.ConstructorParameters.Length);
        if (enclosingErrorConstructor is null)
            return ErrorExpression(reason: BoundExpressionReason.UnsupportedOperation);

        var errorConversion = default(Conversion);
        if (operandInfo.ErrorPayloadType is not null && enclosingInfo.ErrorPayloadType is not null)
        {
            errorConversion = Compilation.ClassifyConversion(operandInfo.ErrorPayloadType, enclosingInfo.ErrorPayloadType);
            if (!errorConversion.Exists)
            {
                _diagnostics.ReportCannotConvertFromTypeToType(
                    operandInfo.ErrorPayloadType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    enclosingInfo.ErrorPayloadType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    questionToken.GetLocation());
            }
            else if (errorConversion.IsImplicit &&
                     !errorConversion.IsIdentity &&
                     errorConversion.IsUserDefined &&
                     errorConversion.MethodSymbol is { } conversionMethod)
            {
                _diagnostics.ReportResultPropagationImplicitErrorConversion(
                    operandInfo.ErrorPayloadType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    enclosingInfo.ErrorPayloadType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    conversionMethod.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    questionToken.GetLocation());
            }
        }

        // Best-effort lookup for `UnwrapError()`; this can be an extension method.
        // We only record it if it resolves unambiguously; lowering can still fall back to other mechanisms.
        IMethodSymbol? unwrapErrorMethod = null;

        if (operandInfo.ErrorPayloadType is not null && operand.Type is { } operandType2 && operandType2.TypeKind != TypeKind.Error)
        {
            // We bind "operand.UnwrapError" as if it was a normal member access.
            // This will include extension methods because operand is a valid extension receiver.
            var receiverForLookup = operand;

            var member = BindMemberAccessOnReceiver(
                receiverForLookup,
                SyntaxFactory.IdentifierName(SyntaxFactory.Identifier("UnwrapError")),
                preferMethods: true,
                allowEventAccess: false,
                suppressNullWarning: true,
                receiverTypeForLookup: operandType,
                forceExtensionReceiver: true);

            if (member is BoundMethodGroupExpression mg)
            {
                // Resolve overload for UnwrapError() (no args)
                var resolution = OverloadResolver.ResolveOverload(
                    mg.Methods,
                    [],
                    Compilation,
                    receiver: receiverForLookup,
                    canBindLambda: EnsureLambdaCompatible,
                    callSyntax: callSyntax);

                if (resolution.Success)
                    unwrapErrorMethod = resolution.Method;
            }
        }

        // Lowering/codegen will branch, extract the error payload when available, convert if needed, and early-return.
        return new BoundPropagateExpression(
            operand,
            okType,
            errorType,
            enclosingInfo.UnionType,
            enclosingErrorConstructor,
            okCaseName: operandInfo.OkCaseName,
            errorCaseName: operandInfo.ErrorCaseName,
            errorCaseHasPayload: operandInfo.ErrorCaseHasPayload,
            okCaseType: operandInfo.OkCaseType,
            okValueProperty: okValueProperty,
            unwrapErrorMethod: unwrapErrorMethod,
            errorConversion: errorConversion);
    }

    private bool TryGetEnclosingCarrierReturnType(out INamedTypeSymbol? enclosingReturnType)
    {
        enclosingReturnType = null;

        ITypeSymbol? declaredReturnType = null;
        var enclosingIsAsync = false;
        for (Binder? current = this; current is not null; current = current.ParentBinder)
        {
            if (current.ContainingSymbol is IMethodSymbol method)
            {
                declaredReturnType = method.ReturnType;
                enclosingIsAsync = method.IsAsync;
                break;
            }

            if (current.ContainingSymbol is ILambdaSymbol lambda)
            {
                declaredReturnType = lambda.ReturnType;
                enclosingIsAsync = lambda.IsAsync;
                break;
            }
        }

        if (declaredReturnType is null)
            return false;

        var effectiveReturnType = UnwrapAlias(declaredReturnType);

        if (enclosingIsAsync)
        {
            effectiveReturnType = AsyncReturnTypeUtilities.ExtractAsyncResultType(Compilation, effectiveReturnType)
                ?? effectiveReturnType;
        }

        enclosingReturnType = effectiveReturnType as INamedTypeSymbol;
        return enclosingReturnType is not null;
    }

    private enum PropagationKind
    {
        Result,
        Option
    }

    private sealed record PropagationInfo(
        PropagationKind Kind,
        INamedTypeSymbol UnionType,
        IDiscriminatedUnionSymbol Union,
        IDiscriminatedUnionCaseSymbol OkCase,
        IDiscriminatedUnionCaseSymbol ErrorCase,
        ITypeSymbol OkPayloadType,
        ITypeSymbol? ErrorPayloadType,
        ITypeSymbol OkCaseType,
        string OkCaseName,
        string ErrorCaseName,
        bool ErrorCaseHasPayload);

    private static bool TryGetPropagationInfo(INamedTypeSymbol typeSymbol, out PropagationInfo info)
    {
        info = null!;
        var union = typeSymbol.TryGetDiscriminatedUnion();
        if (union is null)
            return false;

        if (typeSymbol.Name == "Result")
        {
            var okCase = union.Cases.FirstOrDefault(@case => @case.Name == "Ok");
            var errorCase = union.Cases.FirstOrDefault(@case => @case.Name == "Error");
            if (okCase is null || errorCase is null)
                return false;

            if (okCase.ConstructorParameters.Length != 1 || errorCase.ConstructorParameters.Length != 1)
                return false;

            info = new PropagationInfo(
                PropagationKind.Result,
                typeSymbol,
                union,
                okCase,
                errorCase,
                okCase.ConstructorParameters[0].Type,
                errorCase.ConstructorParameters[0].Type,
                okCase,
                okCase.Name,
                errorCase.Name,
                ErrorCaseHasPayload: true);
            return true;
        }

        if (typeSymbol.Name == "Option")
        {
            var okCase = union.Cases.FirstOrDefault(@case => @case.Name == "Some");
            var errorCase = union.Cases.FirstOrDefault(@case => @case.Name == "None");
            if (okCase is null || errorCase is null)
                return false;

            if (okCase.ConstructorParameters.Length != 1 || errorCase.ConstructorParameters.Length != 0)
                return false;

            info = new PropagationInfo(
                PropagationKind.Option,
                typeSymbol,
                union,
                okCase,
                errorCase,
                okCase.ConstructorParameters[0].Type,
                ErrorPayloadType: null,
                okCase,
                okCase.Name,
                errorCase.Name,
                ErrorCaseHasPayload: false);
            return true;
        }

        return false;
    }

    private void EnsureMatchArmPatternsValid(
        BoundExpression scrutinee,
        SyntaxNode matchSyntax,
        SyntaxList<MatchArmSyntax> armSyntaxes,
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
            var patternSyntax = armSyntaxes[i].Pattern;
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

                    // `null` is parsed as a type in some contexts (e.g. `null => ...`).
                    // Treat it as a constant null pattern: it is valid whenever the scrutinee can be null.
                    if (patternType is NullTypeSymbol)
                    {
                        if (CanBeNull(scrutineeType))
                            return;

                        _diagnostics.ReportMatchExpressionArmPatternInvalid(
                            "null",
                            scrutineeType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                            patternSyntax.GetLocation());
                        return;
                    }

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
                    // Null patterns are unreachable when the scrutinee is non-nullable.
                    // `IsNullable` covers both reference and value types.
                    if (constant.Expression is null && constant.ConstantValue is null && !CanBeNull(scrutineeType))
                    {
                        _diagnostics.ReportMatchExpressionArmPatternInvalid(
                            "null",
                            scrutineeType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                            patternSyntax.GetLocation());
                        return;
                    }
                    // Literal-backed constant pattern
                    if (constant.LiteralType is not null)
                    {
                        var underlyingType = UnwrapAlias(constant.LiteralType.UnderlyingType);

                        if (underlyingType.TypeKind == TypeKind.Error)
                            return;

                        if (PatternCanMatch(scrutineeType, underlyingType))
                            return;

                        var patternDisplay = GetMatchPatternDisplay(constant.LiteralType);
                        var location = patternSyntax.GetLocation();

                        _diagnostics.ReportMatchExpressionArmPatternInvalid(
                            patternDisplay,
                            scrutineeType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                            location);
                        return;
                    }

                    // Expression-backed value pattern
                    if (constant.Expression is not null)
                    {
                        var valueType = constant.Expression.Type ?? Compilation.ErrorTypeSymbol;
                        valueType = UnwrapAlias(valueType);

                        if (valueType.TypeKind == TypeKind.Error)
                            return;

                        if (PatternCanMatch(scrutineeType, valueType))
                            return;

                        var patternDisplay = GetMatchPatternDisplay(valueType);
                        var location = patternSyntax.GetLocation();

                        _diagnostics.ReportMatchExpressionArmPatternInvalid(
                            patternDisplay,
                            scrutineeType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                            location);
                        return;
                    }

                    // Defensive fallback: should never happen
                    return;
                }
            case BoundCasePattern casePattern:
                {
                    var scrutineeUnion = scrutineeType.TryGetDiscriminatedUnion()
                        ?? scrutineeType.TryGetDiscriminatedUnionCase()?.Union;

                    var caseUnion = UnwrapAlias(casePattern.CaseSymbol.Union);

                    if (scrutineeUnion is null || !AreSameUnionPatternTarget(UnwrapAlias(scrutineeUnion), caseUnion))
                    {
                        var patternDisplay = $"for case '{casePattern.CaseSymbol.Name}'";
                        _diagnostics.ReportMatchExpressionArmPatternInvalid(
                            patternDisplay,
                            scrutineeType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                            patternSyntax.GetLocation());
                        return;
                    }

                    if (patternSyntax is MemberPatternSyntax caseSyntax && caseSyntax.ArgumentList is { } argumentList)
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
            case BoundPositionalPattern tuplePattern:
                {
                    if (patternSyntax is PositionalPatternSyntax collectionSyntax &&
                        collectionSyntax.OpenParenToken.IsKind(SyntaxKind.OpenBracketToken))
                    {
                        if (!TryGetCollectionPatternElementType(scrutineeType, out var elementType))
                            return;

                        var patternElements = tuplePattern.Elements;
                        var elementCount = Math.Min(patternElements.Length, collectionSyntax.Elements.Count);

                        for (var i = 0; i < elementCount; i++)
                            EnsureMatchArmPatternValid(elementType, collectionSyntax.Elements[i].Pattern, patternElements[i]);

                        return;
                    }

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

                    if (patternSyntax is PositionalPatternSyntax tupleSyntax)
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

    private static bool AreSameUnionPatternTarget(ITypeSymbol left, ITypeSymbol right)
    {
        if (ReferenceEquals(left, right))
            return true;

        if (left is not INamedTypeSymbol leftNamed || right is not INamedTypeSymbol rightNamed)
            return false;

        leftNamed = leftNamed.OriginalDefinition as INamedTypeSymbol ?? leftNamed;
        rightNamed = rightNamed.OriginalDefinition as INamedTypeSymbol ?? rightNamed;

        if (!string.Equals(leftNamed.Name, rightNamed.Name, StringComparison.Ordinal))
            return false;

        if (leftNamed.TypeParameters.Length != rightNamed.TypeParameters.Length)
            return false;

        var leftNs = leftNamed.ContainingNamespace?.ToDisplayString();
        var rightNs = rightNamed.ContainingNamespace?.ToDisplayString();
        return string.Equals(leftNs, rightNs, StringComparison.Ordinal);
    }

    private static bool ArePatternTypesEquivalent(ITypeSymbol left, ITypeSymbol right)
    {
        if (ReferenceEquals(left, right))
            return true;

        left = UnwrapAlias(left);
        right = UnwrapAlias(right);

        if (left.SpecialType != SpecialType.None && left.SpecialType == right.SpecialType)
            return true;

        if (left is INamedTypeSymbol leftNamed && right is INamedTypeSymbol rightNamed)
        {
            leftNamed = leftNamed.OriginalDefinition as INamedTypeSymbol ?? leftNamed;
            rightNamed = rightNamed.OriginalDefinition as INamedTypeSymbol ?? rightNamed;

            if (!string.Equals(leftNamed.Name, rightNamed.Name, StringComparison.Ordinal))
                return false;

            if (leftNamed.TypeParameters.Length != rightNamed.TypeParameters.Length)
                return false;

            var leftNs = leftNamed.ContainingNamespace?.ToDisplayString();
            var rightNs = rightNamed.ContainingNamespace?.ToDisplayString();
            return string.Equals(leftNs, rightNs, StringComparison.Ordinal);
        }

        return false;
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

        if (patternType is NullTypeSymbol)
            return CanBeNull(scrutineeType);

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
                AreSameUnionPatternTarget(UnwrapAlias(targetUnion), UnwrapAlias(caseType.Union)))
            {
                return true;
            }
        }

        return IsAssignable(patternType, scrutineeType, out _) ||
               IsAssignable(scrutineeType, patternType, out _);

    }

    private bool CanBeNull(ITypeSymbol type)
    {
        type = UnwrapAlias(type);

        if (type.TypeKind == TypeKind.Error)
            return true;

        // Direct null type.
        if (type is NullTypeSymbol)
            return true;

        // Explicit nullable wrapper.
        if (type.IsNullable)
            return true;

        // Union can be null if any member can be null.
        if (type is ITypeUnionSymbol union)
        {
            foreach (var member in union.Types)
            {
                if (CanBeNull(member))
                    return true;
            }
        }

        return false;
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
        SyntaxNode matchSyntax,
        SyntaxList<MatchArmSyntax> armSyntaxes,
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
                    armSyntaxes[i].Pattern.GetLocation());
                continue;
            }

            if (arm.Guard is not null)
                continue;

            if (IsCatchAllPattern(scrutineeType, arm.Pattern))
                seenCatchAll = true;
        }
    }

    private void EnsureMatchExhaustive(
        SyntaxNode matchSyntax,
        SyntaxList<MatchArmSyntax> armSyntaxes,
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
            EnsureBooleanMatchExhaustive(matchSyntax, arms, catchAllIndex);
            return;
        }

        var discriminatedUnion = scrutineeType.TryGetDiscriminatedUnion()
            ?? scrutineeType.TryGetDiscriminatedUnionCase()?.Union;

        if (discriminatedUnion is not null)
        {
            EnsureDiscriminatedUnionMatchExhaustive(matchSyntax, armSyntaxes, arms, discriminatedUnion, catchAllIndex);
            return;
        }

        if (scrutineeType is INamedTypeSymbol { TypeKind: TypeKind.Enum } enumType)
        {
            EnsureEnumMatchExhaustive(matchSyntax, armSyntaxes, arms, enumType, catchAllIndex);
            return;
        }

        if (scrutineeType is not ITypeUnionSymbol union)
        {
            // Special-case: nullable scrutinee can be exhaustive without an explicit `_` arm
            // when it has a guaranteed `null` arm and a guaranteed declaration arm that
            // matches the underlying non-null type.
            if (scrutineeType.IsNullable)
            {
                var underlying = scrutineeType.GetPlainType();

                var hasNullArm = false;
                var hasUnderlyingArm = false;

                for (var i = 0; i < arms.Length; i++)
                {
                    var arm = arms[i];

                    // Keep exhaustiveness reasoning consistent with the rest of this method:
                    // only arms whose guard guarantees match participate.
                    var guardGuaranteesMatch = BoundNodeFacts.MatchArmGuardGuaranteesMatch(arm.Guard);
                    if (!guardGuaranteesMatch)
                        continue;

                    if (IsNullConstantPattern(arm.Pattern))
                    {
                        hasNullArm = true;
                        continue;
                    }

                    if (arm.Pattern is BoundDeclarationPattern decl)
                    {
                        var declared = UnwrapAlias(decl.DeclaredType);

                        if (declared.TypeKind != TypeKind.Error && underlying.TypeKind != TypeKind.Error &&
                            ArePatternTypesEquivalent(declared, underlying))
                        {
                            hasUnderlyingArm = true;
                        }
                    }

                    if (hasNullArm && hasUnderlyingArm)
                        return;
                }
            }

            if (catchAllIndex >= 0)
                return;

            _diagnostics.ReportMatchExpressionNotExhaustive(
                "_",
                matchSyntax.GetLocation());
            return;
        }

        var remaining = new HashSet<ITypeSymbol>(
            GetUnionMembers(union),
            TypeSymbolReferenceComparer.Instance);

        var literalCoverage = CreateLiteralCoverage(remaining);

        HashSet<ITypeSymbol>? guaranteedRemaining = null;
        Dictionary<ITypeSymbol, HashSet<object?>>? guaranteedLiteralCoverage = null;
        if (catchAllIndex >= 0)
        {
            guaranteedRemaining = new HashSet<ITypeSymbol>(remaining, TypeSymbolReferenceComparer.Instance);
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
                            ReportRedundantCatchAll(armSyntaxes, catchAllIndex);
                            reportedRedundantCatchAll = true;
                        }
                    }
                    else if (i == catchAllIndex && guaranteedRemaining is not null && guaranteedRemaining.Count == 0)
                    {
                        ReportRedundantCatchAll(armSyntaxes, catchAllIndex);
                        reportedRedundantCatchAll = true;
                    }
                }

                return;
            }

            if (catchAllIndex >= 0 && !reportedRedundantCatchAll && i == catchAllIndex && guaranteedRemaining is not null && guaranteedRemaining.Count == 0)
            {
                ReportRedundantCatchAll(armSyntaxes, catchAllIndex);
                reportedRedundantCatchAll = true;
            }
        }

        if (literalCoverage is not null && literalCoverage.Count > 0)
        {
            foreach (var (type, constants) in literalCoverage)
            {
                if (remaining.Contains(type))
                {
                    ReportMissingLiteralCoverage(matchSyntax, type, constants);
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
                matchSyntax.GetLocation());
        }

        static bool IsNullConstantPattern(BoundPattern pattern)
        {
            // Preferred/normalized form: literal-backed null constant pattern
            if (pattern is BoundConstantPattern { Expression: null, ConstantValue: null })
                return true;

            // Defensive: treat expression-backed `NullType` as null too
            if (pattern is BoundConstantPattern { Expression: BoundTypeExpression { Type: NullTypeSymbol } })
                return true;

            return false;
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

    private void ReportRedundantCatchAll(SyntaxList<MatchArmSyntax> armSyntaxes, int catchAllIndex)
    {
        var patternLocation = armSyntaxes[catchAllIndex].Pattern.GetLocation();
        _diagnostics.ReportMatchExpressionCatchAllRedundant(patternLocation);
    }

    private bool IsCatchAllPattern(ITypeSymbol scrutineeType, BoundPattern pattern)
    {
        // Value-testing patterns are never catch-all.
        // This keeps reachability/exhaustiveness conservative when patterns depend on runtime values.
        if (pattern is BoundConstantPattern)
            return false;

        if (pattern is BoundRelationalPattern)
            return false;

        switch (pattern)
        {
            case BoundDiscardPattern:
                return true;
            case BoundDeclarationPattern declaration:
                {
                    var declaredType = UnwrapAlias(declaration.DeclaredType);

                    // A declaration pattern is catch-all regardless of whether it binds a name or discards.
                    // (e.g. `string? x` should still cover all values of `string?`.)
                    if (SymbolEqualityComparer.Default.Equals(declaredType, scrutineeType))
                        return true;

                    return declaredType.SpecialType == SpecialType.System_Object;
                }
            case BoundOrPattern orPattern:
                return IsCatchAllPattern(scrutineeType, orPattern.Left) ||
                       IsCatchAllPattern(scrutineeType, orPattern.Right);
            case BoundPositionalPattern tuplePattern:
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
        SyntaxNode matchSyntax,
        SyntaxList<MatchArmSyntax> armSyntaxes,
        ImmutableArray<BoundMatchArm> arms,
        IDiscriminatedUnionSymbol union,
        int catchAllIndex)
    {
        var remaining = new HashSet<IDiscriminatedUnionCaseSymbol>(union.Cases, SymbolReferenceComparer<IDiscriminatedUnionCaseSymbol>.Instance);

        HashSet<IDiscriminatedUnionCaseSymbol>? guaranteedRemaining = null;
        if (catchAllIndex >= 0)
            guaranteedRemaining = new HashSet<IDiscriminatedUnionCaseSymbol>(remaining, SymbolReferenceComparer<IDiscriminatedUnionCaseSymbol>.Instance);

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
                            ReportRedundantCatchAll(armSyntaxes, catchAllIndex);
                            reportedRedundantCatchAll = true;
                        }
                    }
                    else if (i == catchAllIndex && guaranteedRemaining is not null && guaranteedRemaining.Count == 0)
                    {
                        ReportRedundantCatchAll(armSyntaxes, catchAllIndex);
                        reportedRedundantCatchAll = true;
                    }
                }

                return;
            }

            if (catchAllIndex >= 0 && !reportedRedundantCatchAll && i == catchAllIndex && guaranteedRemaining is not null && guaranteedRemaining.Count == 0)
            {
                ReportRedundantCatchAll(armSyntaxes, catchAllIndex);
                reportedRedundantCatchAll = true;
            }
        }

        var missingCase = remaining.FirstOrDefault();

        if (missingCase is not null)
        {
            _diagnostics.ReportMatchExpressionNotExhaustive(
                missingCase.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                matchSyntax.GetLocation());
        }
    }

    private void EnsureEnumMatchExhaustive(
        SyntaxNode matchSyntax,
        SyntaxList<MatchArmSyntax> armSyntaxes,
        ImmutableArray<BoundMatchArm> arms,
        INamedTypeSymbol enumType,
        int catchAllIndex)
    {
        var remaining = new HashSet<IFieldSymbol>(GetEnumMembers(enumType), SymbolReferenceComparer<IFieldSymbol>.Instance);

        if (remaining.Count == 0)
            return;

        for (var i = 0; i < arms.Length; i++)
        {
            var arm = arms[i];
            var guardGuaranteesMatch = BoundNodeFacts.MatchArmGuardGuaranteesMatch(arm.Guard);

            if (!guardGuaranteesMatch)
                continue;

            RemoveCoveredEnumMembers(remaining, enumType, arm.Pattern);

            if (remaining.Count == 0)
                return;
        }

        if (catchAllIndex >= 0)
            return;

        foreach (var missing in remaining.Select(field => field.Name).OrderBy(name => name, StringComparer.Ordinal))
        {
            _diagnostics.ReportMatchExpressionNotExhaustive(
                missing,
                matchSyntax.GetLocation());
        }
    }

    private bool CasePatternCoversAllArguments(BoundCasePattern casePattern)
    {
        var parameters = casePattern.CaseSymbol.ConstructorParameters;
        var argumentCount = Math.Min(parameters.Length, casePattern.Arguments.Length);

        for (var i = 0; i < argumentCount; i++)
        {
            if (!IsTotalPattern(parameters[i].Type, casePattern.Arguments[i]))
                return false;
        }

        return true;
    }

    private bool IsTotalPattern(ITypeSymbol inputType, BoundPattern pattern)
    {
        inputType = UnwrapAlias(inputType);

        switch (pattern)
        {
            case BoundDiscardPattern:
                return true;
            case BoundConstantPattern constant:
                return inputType.SpecialType == SpecialType.System_Unit &&
                       constant.Expression is BoundUnitExpression;
            case BoundDeclarationPattern declaration:
                {
                    var declaredType = UnwrapAlias(declaration.DeclaredType);

                    if (declaredType.TypeKind == TypeKind.Error || inputType.TypeKind == TypeKind.Error)
                        return true;

                    return IsAssignable(declaredType, inputType, out _);
                }
            case BoundPositionalPattern tuplePattern:
                {
                    var elementTypes = GetTupleElementTypes(inputType);

                    if (elementTypes.Length == 0)
                    {
                        return inputType.SpecialType == SpecialType.System_Unit &&
                               tuplePattern.Elements.Length == 0;
                    }

                    if (elementTypes.Length != tuplePattern.Elements.Length)
                        return false;

                    for (var i = 0; i < tuplePattern.Elements.Length; i++)
                    {
                        if (!IsTotalPattern(elementTypes[i], tuplePattern.Elements[i]))
                            return false;
                    }

                    return true;
                }
            case BoundPropertyPattern propertyPattern:
                {
                    if (CanBeNull(inputType))
                        return false;

                    if (propertyPattern.NarrowedType is not null &&
                        !IsAssignable(propertyPattern.NarrowedType, inputType, out _))
                    {
                        return false;
                    }

                    foreach (var property in propertyPattern.Properties)
                    {
                        if (!IsTotalPattern(property.Type, property.Pattern))
                            return false;
                    }

                    return true;
                }
            case BoundOrPattern orPattern:
                return IsTotalPattern(inputType, orPattern.Left) ||
                       IsTotalPattern(inputType, orPattern.Right);
            case BoundAndPattern andPattern:
                return IsTotalPattern(inputType, andPattern.Left) &&
                       IsTotalPattern(inputType, andPattern.Right);
            default:
                return false;
        }
    }

    private static bool TryGetLiteralBoolConstant(BoundPattern pattern, out bool value)
    {
        value = default;

        if (pattern is not BoundConstantPattern constant)
            return false;

        // Expression-backed constant patterns may depend on runtime values and must not
        // participate in compile-time exhaustiveness reasoning.
        if (constant.Expression is not null)
            return false;

        if (constant.LiteralType is not LiteralTypeSymbol literal)
            return false;

        if (literal.ConstantValue is not bool b)
            return false;

        value = b;
        return true;
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
                    // Expression-backed value patterns may depend on runtime values and must not
                    // participate in compile-time coverage reasoning.
                    if (constant.Expression is not null)
                        break;

                    // Defensive: literal-backed patterns should always have a literal type.
                    if (constant.LiteralType is null)
                        break;

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
            case BoundPositionalPattern tuplePattern:
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
                        ArePatternTypesEquivalent(declaredType, UnwrapAlias((ITypeSymbol)union)))
                    {
                        remaining.Clear();
                        break;
                    }

                    var declarationUnion = declaredType.TryGetDiscriminatedUnion()
                        ?? declaredType.TryGetDiscriminatedUnionCase()?.Union;

                    if (declarationUnion is not null &&
                        AreSameUnionPatternTarget(UnwrapAlias(declarationUnion), UnwrapAlias(union)))
                    {
                        remaining.Clear();
                    }

                    break;
                }
            case BoundCasePattern casePattern:
                if (AreSameUnionPatternTarget(UnwrapAlias(casePattern.CaseSymbol.Union), UnwrapAlias(union)) &&
                    CasePatternCoversAllArguments(casePattern))
                {
                    remaining.Remove(casePattern.CaseSymbol);
                }
                break;
            case BoundOrPattern orPattern:
                RemoveCoveredCases(remaining, orPattern.Left, union);
                RemoveCoveredCases(remaining, orPattern.Right, union);
                break;
        }
    }

    private static IEnumerable<IFieldSymbol> GetEnumMembers(INamedTypeSymbol enumType)
    {
        var normalizedEnum = (INamedTypeSymbol)UnwrapAlias(enumType);

        return normalizedEnum
            .GetMembers()
            .OfType<IFieldSymbol>()
            .Where(field =>
                field.IsConst &&
                ArePatternTypesEquivalent(UnwrapAlias(field.Type), normalizedEnum));
    }

    private void RemoveCoveredEnumMembers(
        HashSet<IFieldSymbol> remaining,
        INamedTypeSymbol enumType,
        BoundPattern pattern)
    {
        enumType = (INamedTypeSymbol)UnwrapAlias(enumType);

        if (IsCatchAllPattern(enumType, pattern))
        {
            remaining.Clear();
            return;
        }

        switch (pattern)
        {
            case BoundConstantPattern constant:
                if (TryGetEnumField(constant.Expression, enumType, out var field))
                    remaining.Remove(field);

                break;
            case BoundOrPattern orPattern:
                RemoveCoveredEnumMembers(remaining, enumType, orPattern.Left);
                RemoveCoveredEnumMembers(remaining, enumType, orPattern.Right);
                break;
        }
    }

    private static bool TryGetEnumField(BoundExpression? expression, INamedTypeSymbol enumType, out IFieldSymbol field)
    {
        field = null!;

        if (expression is null)
            return false;

        switch (expression)
        {
            case BoundFieldAccess fieldAccess:
                return TryBindEnumField(fieldAccess.Field, enumType, out field);
            case BoundMemberAccessExpression { Member: IFieldSymbol memberField }:
                return TryBindEnumField(memberField, enumType, out field);
            default:
                return false;
        }
    }

    private static bool TryBindEnumField(IFieldSymbol candidate, INamedTypeSymbol enumType, out IFieldSymbol field)
    {
        field = null!;

        if (!candidate.IsConst)
            return false;

        if (!SymbolEqualityComparer.Default.Equals(UnwrapAlias(candidate.Type), enumType))
            return false;

        field = candidate;
        return true;
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
        SyntaxNode matchSyntax,
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
            _diagnostics.ReportMatchExpressionNotExhaustive("true", matchSyntax.GetLocation());

        if ((remaining & BooleanCoverage.False) != 0)
            _diagnostics.ReportMatchExpressionNotExhaustive("false", matchSyntax.GetLocation());
    }

    private BooleanCoverage GetBooleanCoverage(BoundPattern pattern)
    {
        switch (pattern)
        {
            case BoundDiscardPattern:
                return BooleanCoverage.All;

            case BoundDeclarationPattern declaration when IsBooleanType(declaration.DeclaredType):
                return BooleanCoverage.All;

            case BoundConstantPattern constant when TryGetLiteralBoolConstant(constant, out var value):
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
                coverage ??= new Dictionary<ITypeSymbol, HashSet<object?>>(TypeSymbolReferenceComparer.Instance);
                coverage[member] = new HashSet<object?>();
            }
        }

        return coverage;
    }

    private static Dictionary<ITypeSymbol, HashSet<object?>>? CloneLiteralCoverage(Dictionary<ITypeSymbol, HashSet<object?>>? coverage)
    {
        if (coverage is null)
            return null;

        var clone = new Dictionary<ITypeSymbol, HashSet<object?>>(TypeSymbolReferenceComparer.Instance);

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
        SyntaxNode matchSyntax,
        ITypeSymbol type,
        HashSet<object?> constants)
    {
        var targetType = UnwrapAlias(type);

        if (targetType.SpecialType == SpecialType.System_Boolean)
        {
            if (!constants.Contains(true))
                _diagnostics.ReportMatchExpressionNotExhaustive("true", matchSyntax.GetLocation());

            if (!constants.Contains(false))
                _diagnostics.ReportMatchExpressionNotExhaustive("false", matchSyntax.GetLocation());
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

        if (!HasExpressionErrors(condition))
        {
            var boolType = Compilation.GetSpecialType(SpecialType.System_Boolean);
            var conversion = Compilation.ClassifyConversion(condition.Type, boolType);
            if (!conversion.Exists)
            {
                _diagnostics.ReportCannotConvertFromTypeToType(condition.Type, boolType, ifExpression.Condition.GetLocation());
            }
        }

        var thenBinder = SemanticModel.GetBinder(ifExpression, this);
        var previousAllowReturnsInBlockExpressionsOnly = _allowReturnsInBlockExpressionsOnly;
        _allowReturnsInBlockExpressionsOnly = false;
        BoundExpression thenExpr;
        try
        {
            thenExpr = thenBinder is BlockBinder bb
                ? bb.BindExpression(ifExpression.Expression, allowReturn: _allowReturnsInExpression, allowReturnInBlockExpressionsOnly: false)
                : thenBinder.BindExpression(ifExpression.Expression);
        }
        finally
        {
            _allowReturnsInBlockExpressionsOnly = previousAllowReturnsInBlockExpressionsOnly;
        }

        if (ifExpression.ElseClause is null)
        {
            _diagnostics.ReportIfExpressionRequiresElse(ifExpression.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.OtherError);
        }

        var elseBinder = SemanticModel.GetBinder(ifExpression.ElseClause, this);
        previousAllowReturnsInBlockExpressionsOnly = _allowReturnsInBlockExpressionsOnly;
        _allowReturnsInBlockExpressionsOnly = false;
        BoundExpression elseExpr;
        try
        {
            elseExpr = elseBinder is BlockBinder ebb
                ? ebb.BindExpression(ifExpression.ElseClause.Expression, allowReturn: _allowReturnsInExpression, allowReturnInBlockExpressionsOnly: false)
                : elseBinder.BindExpression(ifExpression.ElseClause.Expression);
        }
        finally
        {
            _allowReturnsInBlockExpressionsOnly = previousAllowReturnsInBlockExpressionsOnly;
        }

        var thenType = thenExpr.Type ?? Compilation.ErrorTypeSymbol;
        var elseType = elseExpr.Type ?? Compilation.ErrorTypeSymbol;

        var resultType = TypeSymbolNormalization.NormalizeUnion(new[] { thenType, elseType });

        var targetType = GetTargetType(ifExpression);
        if (targetType is NullableTypeSymbol nullableTargetType)
            targetType = nullableTargetType.UnderlyingType;

        if (targetType is not null &&
            targetType.TypeKind != TypeKind.Error &&
            IsAssignable(targetType, thenType, out _) &&
            IsAssignable(targetType, elseType, out _))
        {
            resultType = targetType;
        }

        thenExpr = ConvertIfNeeded(resultType, thenExpr, ifExpression.Expression);
        elseExpr = ConvertIfNeeded(resultType, elseExpr, ifExpression.ElseClause.Expression);

        return new BoundIfExpression(condition, thenExpr, elseExpr);

        BoundExpression ConvertIfNeeded(ITypeSymbol target, BoundExpression expression, ExpressionSyntax syntax)
        {
            if (target.TypeKind == TypeKind.Error || !ShouldAttemptConversion(expression))
                return expression;

            var sourceType = expression.Type;
            if (sourceType is null || SymbolEqualityComparer.Default.Equals(sourceType, target))
                return expression;

            if (!IsAssignable(target, sourceType, out var conversion))
            {
                _diagnostics.ReportCannotConvertFromTypeToType(
                    sourceType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    target.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    syntax.GetLocation());
                return new BoundErrorExpression(target, null, BoundExpressionReason.TypeMismatch);
            }

            return ApplyConversion(expression, target, conversion, syntax);
        }
    }

    private ITypeSymbol? GetTargetType(SyntaxNode node)
    {
        // If the node is the callee of an invocation (e.g. `.Some` in `.Some(value)`),
        // recover contextual target typing from the invocation's parent context.
        if (node.Parent is InvocationExpressionSyntax invocation &&
            ReferenceEquals(invocation.Expression, node))
        {
            var invocationTargetType = GetTargetType(invocation);
            if (invocationTargetType is not null)
                return invocationTargetType;
        }

        // Target type from `return <expr>`.
        if (node.Parent is ReturnStatementSyntax)
        {
            // Walk up to nearest containing method symbol.
            var container = ContainingSymbol;
            while (container is { } && container is not IMethodSymbol)
                container = container.ContainingSymbol;

            if (container is IMethodSymbol m)
                return m.ReturnType;
        }

        // Target type from binary equality/inequality: `x == .Member` / `.Member == x`.
        if (node.Parent is BinaryExpressionSyntax binary &&
            (binary.OperatorToken.Kind is SyntaxKind.EqualsEqualsToken or SyntaxKind.NotEqualsExpression))
        {
            var other = ReferenceEquals(binary.Left, node) ? binary.Right : binary.Left;

            // Avoid recursion if the other side is also target-typed.
            if (other is not MemberBindingExpressionSyntax)
            {
                var otherBound = BindExpression(other);
                if (otherBound.Type is { } otherType && otherType.TypeKind != TypeKind.Error)
                    return otherType;
            }
        }

        return _targetTypeStack.Count > 0
            ? _targetTypeStack.Peek()
            : null;
    }

    private ITypeSymbol? GetConstructorArgumentTargetTypeFromSyntax(
        TypeSyntax typeSyntax,
        SeparatedSyntaxList<ArgumentSyntax> arguments,
        ArgumentSyntax argument)
    {
        if (BindTypeSyntaxAsExpression(typeSyntax) is not BoundTypeExpression typeExpression ||
            typeExpression.Type is not INamedTypeSymbol typeSymbol)
        {
            return null;
        }

        return GetConstructorArgumentTargetType(typeSymbol, arguments, argument);
    }

    private ITypeSymbol? GetConstructorArgumentTargetType(
        INamedTypeSymbol typeSymbol,
        SeparatedSyntaxList<ArgumentSyntax> arguments,
        ArgumentSyntax argument)
    {
        var argumentIndex = -1;
        for (var i = 0; i < arguments.Count; i++)
        {
            if (arguments[i] == argument)
            {
                argumentIndex = i;
                break;
            }
        }

        if (argumentIndex < 0)
            return null;

        var argumentCount = arguments.Count;
        var argumentExpression = argument.Expression;
        var argumentName = argument.NameColon?.Name.Identifier.ValueText;

        var placeholderArguments = new BoundArgument[argumentCount];
        for (var i = 0; i < argumentCount; i++)
        {
            var syntax = arguments[i];
            var name = syntax.NameColon?.Name.Identifier.ValueText;
            placeholderArguments[i] = new BoundArgument(BoundFactory.NullLiteral(), RefKind.None, name, syntax);
        }

        var constructors = typeSymbol.Constructors
            .Where(m => AreArgumentsCompatibleWithMethod(m, argumentCount, receiver: null, placeholderArguments))
            .ToImmutableArray();

        RecordLambdaTargets(argumentExpression, constructors, argumentIndex, extensionReceiverImplicit: false);

        ITypeSymbol? parameterType = null;

        foreach (var constructor in constructors)
        {
            IParameterSymbol? parameter = null;
            if (!string.IsNullOrEmpty(argumentName))
                parameter = constructor.Parameters.FirstOrDefault(p => p.Name == argumentName);
            else if (argumentIndex < constructor.Parameters.Length)
                parameter = constructor.Parameters[argumentIndex];

            if (parameter is null)
                continue;

            if (parameterType is null)
            {
                parameterType = parameter.Type;
                continue;
            }

            if (!SymbolEqualityComparer.Default.Equals(parameterType, parameter.Type))
            {
                parameterType = null;
                break;
            }
        }

        return parameterType;
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

    protected BoundExpression BindTypeSyntaxAsExpression(TypeSyntax syntax)
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

        if (syntax is IdentifierNameSyntax id)
        {
            return BindTypeName(id.Identifier.ValueText, id.GetLocation(), []);
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

        if (syntax is NullableTypeSyntax nullableTypeSyntax)
        {
            var type = BindTypeSyntaxAsExpression(nullableTypeSyntax.ElementType);
            return new BoundTypeExpression(type.Type.MakeNullable());
        }

        if (syntax is PointerTypeSyntax pointerTypeSyntax)
        {
            if (!IsUnsafeEnabled)
            {
                _diagnostics.ReportPointerTypeRequiresUnsafe(pointerTypeSyntax.GetLocation());
                return ErrorExpression(reason: BoundExpressionReason.TypeMismatch);
            }

            if (BindTypeSyntaxAsExpression(pointerTypeSyntax.ElementType) is not BoundTypeExpression elementTypeExpression)
                return ErrorExpression(reason: BoundExpressionReason.TypeMismatch);

            var elementType = elementTypeExpression.Type;

            if (elementType.ContainsErrorType())
                return new BoundTypeExpression(elementType);

            var pointerType = Compilation.CreatePointerTypeSymbol(elementType);
            return new BoundTypeExpression(pointerType);
        }

        if (syntax is ArrayTypeSyntax arrayTypeSyntax)
        {
            if (BindTypeSyntaxAsExpression(arrayTypeSyntax.ElementType) is not BoundTypeExpression elementTypeExpression)
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
                if (BindTypeSyntaxAsExpression(element.Type) is not BoundTypeExpression bt)
                    return ErrorExpression(reason: BoundExpressionReason.TypeMismatch);

                boundElements.Add((element.NameColon?.Name.ToString(), bt.Type));
            }

            var tupleType = Compilation.CreateTupleTypeSymbol(boundElements);

            return new BoundTypeExpression(tupleType);
        }

        if (syntax is UnionTypeSyntax unionSyntax)
        {
            var types = new List<ITypeSymbol>();

            foreach (var type in unionSyntax.Types)
            {
                if (BindTypeSyntaxAsExpression(type) is not BoundTypeExpression bt)
                    return ErrorExpression(reason: BoundExpressionReason.TypeMismatch);

                types.Add(bt.Type);
            }

            var unionType = new TypeUnionSymbol(types, null!, null, null, [], null);

            return new BoundTypeExpression(unionType);
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
            var left = BindTypeSyntaxAsExpression(qualified.Left);

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
                if (args.Any(argument => IsRangeType(argument.Type)))
                {
                    _diagnostics.ReportLeftOfAssignmentMustBeAVariablePropertyOrIndexer(node.GetLocation());
                    return new BoundErrorExpression(receiver.Type!, null, BoundExpressionReason.NotFound);
                }

                var arrayRightExpression = BindExpressionWithTargetType(rightSyntax, arrayType.ElementType);

                if (IsErrorExpression(arrayRightExpression))
                    return AsErrorExpression(arrayRightExpression);

                var arrayAccess = new BoundArrayAccessExpression(receiver, args, arrayType.ElementType);
                var arrayRight = BindCompoundAssignmentValue(arrayAccess, arrayRightExpression, arrayType.ElementType, binaryOperator.Value, rightSyntax);
                return BoundFactory.CreateArrayAssignmentExpression(arrayAccess, arrayRight);
            }

            var indexer = ResolveIndexer(receiver.Type!, args, elementAccess.ArgumentList.Arguments, requireSetter: true, out var convertedArguments);

            if (indexer is null || indexer.SetMethod is null)
            {
                _diagnostics.ReportLeftOfAssignmentMustBeAVariablePropertyOrIndexer(node.GetLocation());
                return new BoundErrorExpression(receiver.Type!, null, BoundExpressionReason.NotFound);
            }

            var indexerRightExpression = BindExpressionWithTargetType(rightSyntax, indexer.Type);

            if (IsErrorExpression(indexerRightExpression))
                return AsErrorExpression(indexerRightExpression);

            var indexerAccess = new BoundIndexerAccessExpression(receiver, convertedArguments, indexer);
            var indexerRight = BindCompoundAssignmentValue(indexerAccess, indexerRightExpression, indexer.Type, binaryOperator.Value, rightSyntax);
            return BoundFactory.CreateIndexerAssignmentExpression(indexerAccess, indexerRight);
        }

        var left = BindExpressionAllowingEvent(leftSyntax);

        if (IsErrorExpression(left))
            return AsErrorExpression(left);

        if (left.Symbol is IEventSymbol eventSymbol)
        {
            if (operatorTokenKind is not (SyntaxKind.PlusEqualsToken or SyntaxKind.MinusEqualsToken))
            {
                _diagnostics.ReportEventCanOnlyBeUsedWithPlusOrMinus(eventSymbol.Name, leftSyntax.GetLocation());
                return ErrorExpression(reason: BoundExpressionReason.NotFound);
            }

            var right = BindExpressionWithTargetType(rightSyntax, eventSymbol.Type);

            if (IsErrorExpression(right))
                return AsErrorExpression(right);

            var eventType = eventSymbol.Type;
            var converted = ConvertValueForAssignment(right, eventType, rightSyntax);
            if (converted is BoundErrorExpression)
                return converted;

            var receiver = GetReceiver(left);
            var accessor = operatorTokenKind == SyntaxKind.PlusEqualsToken
                ? eventSymbol.AddMethod
                : eventSymbol.RemoveMethod;

            if (accessor is null)
                return ErrorExpression(reason: BoundExpressionReason.NotFound);

            if (!EnsureMemberAccessible(accessor, leftSyntax.GetLocation(), GetSymbolKindForDiagnostic(accessor)))
                return ErrorExpression(reason: BoundExpressionReason.Inaccessible);

            return new BoundInvocationExpression(accessor, [converted], receiver);
        }

        if (left is BoundConditionalAccessExpression conditionalAccess &&
            conditionalAccess.WhenNotNull is BoundMemberAccessExpression { Member: IEventSymbol conditionalEventSymbol } eventAccess)
        {
            if (operatorTokenKind is not (SyntaxKind.PlusEqualsToken or SyntaxKind.MinusEqualsToken))
            {
                _diagnostics.ReportEventCanOnlyBeUsedWithPlusOrMinus(conditionalEventSymbol.Name, leftSyntax.GetLocation());
                return ErrorExpression(reason: BoundExpressionReason.NotFound);
            }

            var right = BindExpressionWithTargetType(rightSyntax, conditionalEventSymbol.Type);

            if (IsErrorExpression(right))
                return AsErrorExpression(right);

            var eventType = conditionalEventSymbol.Type;
            var converted = ConvertValueForAssignment(right, eventType, rightSyntax);
            if (converted is BoundErrorExpression)
                return converted;

            var accessor = operatorTokenKind == SyntaxKind.PlusEqualsToken
                ? conditionalEventSymbol.AddMethod
                : conditionalEventSymbol.RemoveMethod;

            if (accessor is null)
                return ErrorExpression(reason: BoundExpressionReason.NotFound);

            if (!EnsureMemberAccessible(accessor, leftSyntax.GetLocation(), GetSymbolKindForDiagnostic(accessor)))
                return ErrorExpression(reason: BoundExpressionReason.Inaccessible);

            var invocation = new BoundInvocationExpression(accessor, [converted], eventAccess.Receiver);
            var resultType = invocation.Type ?? Compilation.ErrorTypeSymbol;
            if (!resultType.IsNullable)
                resultType = resultType.MakeNullable();

            return new BoundConditionalAccessExpression(conditionalAccess.Receiver, invocation, resultType);
        }

        if (left is BoundLocalAccess localAccess)
        {
            var localSymbol = localAccess.Local;
            var localType = localSymbol.Type;
            var rightTargetType = localType is RefTypeSymbol refTypeLocal
                ? refTypeLocal.ElementType
                : localType;
            var right = BindExpressionWithTargetType(rightSyntax, rightTargetType);

            if (IsErrorExpression(right))
                return AsErrorExpression(right);

            if (localType is RefTypeSymbol refTypeLocalType)
            {
                var localByRefRight = BindCompoundAssignmentValue(localAccess, right, refTypeLocalType.ElementType, binaryOperator.Value, rightSyntax);
                if (localByRefRight is BoundErrorExpression)
                    return localByRefRight;

                return BoundFactory.CreateByRefAssignmentExpression(localAccess, refTypeLocalType.ElementType, localByRefRight);
            }

            if (!localSymbol.IsMutable)
            {
                _diagnostics.ReportThisValueIsNotMutable(leftSyntax.GetLocation());
                return ErrorExpression(reason: BoundExpressionReason.NotFound);
            }

            var localRight = BindCompoundAssignmentValue(localAccess, right, localType, binaryOperator.Value, rightSyntax);
            return BoundFactory.CreateLocalAssignmentExpression(localSymbol, localAccess, localRight);
        }
        else if (left is BoundParameterAccess parameterAccess)
        {
            var parameterSymbol = parameterAccess.Parameter;
            var parameterType = parameterSymbol.Type;
            var rightTargetType = parameterSymbol.RefKind is RefKind.Ref or RefKind.Out
                ? parameterSymbol.GetByRefElementType()
                : parameterType;
            var right = BindExpressionWithTargetType(rightSyntax, rightTargetType);

            if (IsErrorExpression(right))
                return AsErrorExpression(right);

            if (!parameterSymbol.IsMutable)
            {
                _diagnostics.ReportThisValueIsNotMutable(leftSyntax.GetLocation());
                return ErrorExpression(reason: BoundExpressionReason.NotFound);
            }

            if (parameterSymbol.RefKind is RefKind.Ref or RefKind.Out)
            {
                var byRefElementType = parameterSymbol.GetByRefElementType();
                var parameterByRefRight = BindCompoundAssignmentValue(parameterAccess, right, byRefElementType, binaryOperator.Value, rightSyntax);
                if (parameterByRefRight is BoundErrorExpression)
                    return parameterByRefRight;

                return BoundFactory.CreateByRefAssignmentExpression(parameterAccess, byRefElementType, parameterByRefRight);
            }

            var parameterRight = BindCompoundAssignmentValue(parameterAccess, right, parameterType, binaryOperator.Value, rightSyntax);
            return BoundFactory.CreateParameterAssignmentExpression(parameterSymbol, parameterAccess, parameterRight);
        }
        else if (left.Symbol is IFieldSymbol fieldSymbol)
        {
            if (fieldSymbol.IsConst)
            {
                _diagnostics.ReportThisValueIsNotMutable(leftSyntax.GetLocation());
                return new BoundErrorExpression(fieldSymbol.Type, null, BoundExpressionReason.NotFound);
            }

            var receiver = GetReceiver(left);

            var right = BindExpressionWithTargetType(rightSyntax, fieldSymbol.Type);

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

            var right = BindExpressionWithTargetType(rightSyntax, propertySymbol.Type);

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

        if (symbol is ITypeSymbol nonNamedType)
        {
            return new BoundTypeExpression(EnsureTypeAccessible(nonNamedType, location));
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

            if (BindTypeSyntaxAsExpression(argument.Type) is not BoundTypeExpression bt)
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
        var contextualTargetType = GetTargetType(syntax);
        var underlying = value switch
        {
            byte => Compilation.GetSpecialType(SpecialType.System_Byte),
            int => Compilation.GetSpecialType(SpecialType.System_Int32),
            long => Compilation.GetSpecialType(SpecialType.System_Int64),
            float => Compilation.GetSpecialType(SpecialType.System_Single),
            double => Compilation.GetSpecialType(SpecialType.System_Double),
            decimal => Compilation.GetSpecialType(SpecialType.System_Decimal),
            bool => Compilation.GetSpecialType(SpecialType.System_Boolean),
            char => Compilation.GetSpecialType(SpecialType.System_Char),
            string => Compilation.GetSpecialType(SpecialType.System_String),
            _ => throw new Exception("Unsupported literal type")
        };

        var preserveLiteralPrecision = ShouldPreserveLiteralPrecision(contextualTargetType);
        ITypeSymbol type = preserveLiteralPrecision
            ? new LiteralTypeSymbol(underlying, value, Compilation)
            : underlying;

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

    private static bool ShouldPreserveLiteralPrecision(ITypeSymbol? targetType)
    {
        if (targetType is null)
            return true;

        return RequiresLiteralPrecision(targetType);
    }

    private static bool RequiresLiteralPrecision(ITypeSymbol type)
    {
        type = UnwrapAlias(type);

        if (type is LiteralTypeSymbol)
            return true;

        if (type is not ITypeUnionSymbol union)
            return false;

        foreach (var element in union.Elements)
        {
            if (RequiresLiteralPrecision(element))
                return true;
        }

        return false;
    }

    private BoundExpression BindDefaultExpression(DefaultExpressionSyntax syntax)
    {
        if (syntax.Type is TypeSyntax explicitType)
        {
            var type = ResolveTypeSyntaxOrError(explicitType);
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
        // Normal interpolated strings: bind directly.
        // Multiline interpolated strings (triple-quoted) should apply indentation trimming in binding/lowering,
        // because the syntax tree must preserve raw spans while the runtime value should match multiline literal semantics.

        var isMultiline = syntax.StringStartToken.Text == "\"\"\"";

        BoundExpression? result = null;

        if (!isMultiline)
        {
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

                result = ConcatOrFirst(result, expr);
            }

            return result ?? new BoundLiteralExpression(
                BoundLiteralExpressionKind.StringLiteral,
                string.Empty,
                Compilation.GetSpecialType(SpecialType.System_String));
        }

        // Multiline: reconstruct a raw template with sentinels for interpolations, trim indentation on the template,
        // then split back into text/interpolation parts.
        const char Sentinel = '\u0001';

        var templateBuilder = new StringBuilder();
        var interpolations = new List<InterpolationSyntax>();

        foreach (var content in syntax.Contents)
        {
            switch (content)
            {
                case InterpolatedStringTextSyntax text:
                    templateBuilder.Append(text.Token.ValueText ?? string.Empty);
                    break;

                case InterpolationSyntax interpolation:
                    interpolations.Add(interpolation);
                    templateBuilder.Append(Sentinel);
                    break;

                default:
                    throw new InvalidOperationException("Unknown interpolated string content");
            }
        }

        var trimmedTemplate = TrimMultilineTemplate(templateBuilder.ToString());
        var parts = SplitBySentinel(trimmedTemplate, Sentinel);

        // Interleave text parts with bound interpolation expressions.
        var interpIndex = 0;
        for (var i = 0; i < parts.Count; i++)
        {
            var textPart = parts[i];
            if (!string.IsNullOrEmpty(textPart))
            {
                var textExpr = new BoundLiteralExpression(
                    BoundLiteralExpressionKind.StringLiteral,
                    textPart,
                    Compilation.GetSpecialType(SpecialType.System_String));

                result = ConcatOrFirst(result, textExpr);
            }

            if (interpIndex < interpolations.Count)
            {
                var interpolationExpr = BindExpression(interpolations[interpIndex].Expression);
                result = ConcatOrFirst(result, interpolationExpr);
                interpIndex++;
            }
        }

        return result ?? new BoundLiteralExpression(
            BoundLiteralExpressionKind.StringLiteral,
            string.Empty,
            Compilation.GetSpecialType(SpecialType.System_String));

        BoundExpression? ConcatOrFirst(BoundExpression? left, BoundExpression right)
        {
            if (left is null)
                return right;

            if (left is BoundErrorExpression)
                return left;

            if (right is BoundErrorExpression)
                return right;

            if (IsErrorOrNull(left) || IsErrorOrNull(right))
                return ErrorExpression(reason: BoundExpressionReason.OtherError);

            var concatMethod = ResolveStringConcatMethod(left, right);
            return concatMethod is null
                ? ErrorExpression(reason: BoundExpressionReason.OtherError)
                : new BoundInvocationExpression(concatMethod, [left, right]);
        }

        static List<string> SplitBySentinel(string s, char sentinel)
        {
            var list = new List<string>();
            var start = 0;

            for (var i = 0; i < s.Length; i++)
            {
                if (s[i] == sentinel)
                {
                    list.Add(i > start ? s.Substring(start, i - start) : string.Empty);
                    start = i + 1;
                }
            }

            list.Add(start < s.Length ? s.Substring(start) : string.Empty);
            return list;
        }

        static string TrimMultilineTemplate(string template)
        {
            if (string.IsNullOrEmpty(template))
                return string.Empty;

            // Normalize CRLF to LF for trimming logic.
            template = template.Replace("\r\n", "\n");

            // Remove the first newline if present (common triple-quoted form).
            if (template.StartsWith("\n", StringComparison.Ordinal))
                template = template.Substring(1);

            // Determine indentation from the final line (the whitespace before the closing delimiter).
            var lastNl = template.LastIndexOf('\n');
            if (lastNl < 0)
                return template;

            var suffix = template.Substring(lastNl + 1);
            if (!suffix.All(static ch => ch == ' ' || ch == '\t'))
                return template;

            var indent = suffix;

            // Drop the final newline + indent line entirely.
            template = template.Substring(0, lastNl);

            if (indent.Length == 0)
                return template;

            // Remove the indentation prefix from each line when present.
            var lines = template.Split('\n');
            for (var i = 0; i < lines.Length; i++)
            {
                var line = lines[i];
                if (line.StartsWith(indent, StringComparison.Ordinal))
                    lines[i] = line.Substring(indent.Length);
            }

            return string.Join("\n", lines);
        }
    }

    private static bool IsErrorOrNull(BoundExpression expression)
    {
        return expression.Type is null || expression.Type.TypeKind == TypeKind.Error;
    }

    private BoundExpression BindIdentifierName(IdentifierNameSyntax syntax, bool allowEventAccess = false)
    {
        var name = syntax.Identifier.ValueText;

        if (string.Equals(name, "field", StringComparison.Ordinal))
        {
            if (TryBindAutoPropertyFieldKeyword(out var fieldAccess))
                return fieldAccess;

            if (IsExtensionPropertyAccessor())
            {
                _diagnostics.ReportTheNameDoesNotExistInTheCurrentContext(name, syntax.Identifier.GetLocation());

                var error = ErrorExpression(reason: BoundExpressionReason.NotFound);
                CacheBoundNode(syntax, error);
                return error;
            }
        }

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

            var n = ErrorExpression(reason: BoundExpressionReason.NotFound);
            CacheBoundNode(syntax, n);
            return n;
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
            case IEventSymbol @event:
                if (!EnsureMemberAccessible(@event, syntax.Identifier.GetLocation(), GetSymbolKindForDiagnostic(@event)))
                    return ErrorExpression(reason: BoundExpressionReason.Inaccessible);

                ReportObsoleteIfNeeded(@event, syntax.Identifier.GetLocation());
                return new BoundMemberAccessExpression(null, @event);
            case ILocalSymbol local:
                var b = new BoundLocalAccess(local);
                return UnwrapNullableIfKnownNonNull(b, local);
            case IParameterSymbol param:
                var p = new BoundParameterAccess(param);
                return UnwrapNullableIfKnownNonNull(p, param);
            case IFieldSymbol field:
                {
                    if (!EnsureMemberAccessible(field, syntax.Identifier.GetLocation(), GetSymbolKindForDiagnostic(field)))
                        return ErrorExpression(reason: BoundExpressionReason.Inaccessible);

                    ReportObsoleteIfNeeded(field, syntax.Identifier.GetLocation());
                    var f = new BoundFieldAccess(field);
                    return UnwrapNullableIfKnownNonNull(f, field);
                }
            case IPropertySymbol prop:
                {
                    if (!EnsureMemberAccessible(prop, syntax.Identifier.GetLocation(), GetSymbolKindForDiagnostic(prop)))
                        return ErrorExpression(reason: BoundExpressionReason.Inaccessible);

                    ReportObsoleteIfNeeded(prop, syntax.Identifier.GetLocation());
                    var p2 = new BoundPropertyAccess(prop);
                    return UnwrapNullableIfKnownNonNull(p2, prop);
                }
            default:
                {
                    var n = ErrorExpression(reason: BoundExpressionReason.NotFound);
                    CacheBoundNode(syntax, n);
                    return n;
                }
        }
    }

    private bool TryBindAutoPropertyFieldKeyword(out BoundFieldAccess fieldAccess)
    {
        fieldAccess = null!;

        if (_containingSymbol is not IMethodSymbol
            {
                MethodKind: MethodKind.PropertyGet or MethodKind.PropertySet or MethodKind.InitOnly,
                ContainingSymbol: SourcePropertySymbol propertySymbol
            })
        {
            return false;
        }

        if (propertySymbol.IsDeclaredInExtension || propertySymbol.BackingField is null)
            return false;

        fieldAccess = new BoundFieldAccess(propertySymbol.BackingField);
        return true;
    }

    private bool IsExtensionPropertyAccessor()
    {
        return _containingSymbol is IMethodSymbol
        {
            MethodKind: MethodKind.PropertyGet or MethodKind.PropertySet or MethodKind.InitOnly,
            ContainingSymbol: IPropertySymbol { IsExtensionProperty: true }
        };
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

        return BindBinaryExpression(
            opKind,
            left,
            right,
            syntax.OperatorToken.GetLocation(),
            syntax.Left,
            syntax.Right,
            syntax);
    }

    private BoundExpression BindBinaryExpression(
        SyntaxKind opKind,
        BoundExpression left,
        BoundExpression right,
        Location? diagnosticLocation = null,
        ExpressionSyntax? leftSyntax = null,
        ExpressionSyntax? rightSyntax = null,
        SyntaxNode? callSyntax = null)
    {
        if (HasExpressionErrors(left) || HasExpressionErrors(right))
            return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.ArgumentBindingFailed);

        if (RequiresUnsafePointerArithmetic(opKind, left.Type, right.Type) && !IsUnsafeEnabled)
        {
            _diagnostics.ReportPointerOperationRequiresUnsafe(diagnosticLocation ?? Location.None);
            return ErrorExpression(reason: BoundExpressionReason.UnsupportedOperation);
        }

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
        var userDefinedExpression = BindUserDefinedBinaryOperator(opKind, left, right, diagnosticLocation, leftSyntax, rightSyntax, callSyntax);
        if (userDefinedExpression is not null)
            return userDefinedExpression;

        // 3. Inbyggda operatorer
        if (BoundBinaryOperator.TryLookup(Compilation, opKind, left.Type, right.Type, out var op))
        {
            return new BoundBinaryExpression(left, op, right);
        }

        // Metadata identity drift can produce distinct enum symbols for the same CLR enum type.
        // Keep common enum flag operations bindable when names/namespace align.
        if (BoundBinaryOperator.TryCreateEnumLikeOperator(Compilation, opKind, left.Type, right.Type, out var enumOp))
            return new BoundBinaryExpression(left, enumOp, right);

        // 4. Fel
        var operatorText = SyntaxFacts.GetSyntaxTokenText(opKind) ?? opKind.ToString();
        _diagnostics.ReportOperatorCannotBeAppliedToOperandsOfTypes(
            operatorText,
            left.Type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
            right.Type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
            diagnosticLocation ?? Location.None);

        return ErrorExpression(reason: BoundExpressionReason.NotFound);
    }

    private static bool RequiresUnsafePointerArithmetic(SyntaxKind opKind, ITypeSymbol? leftType, ITypeSymbol? rightType)
    {
        if (opKind is not (SyntaxKind.PlusToken or SyntaxKind.MinusToken))
            return false;

        return leftType is IPointerTypeSymbol || rightType is IPointerTypeSymbol;
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

            var target = BindPipelineInvocationTargetExpression(invocation.Expression, left);

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

        var propertyTarget = BindPipelineInvocationTargetExpression(syntax.Right, left);

        if (propertyTarget is BoundErrorExpression propertyError)
            return propertyError;

        if (TryBindPipelineImplicitInvocation(propertyTarget, syntax.Left, left, syntax.Right, out var implicitInvocation))
            return implicitInvocation;

        if (propertyTarget is BoundMemberAccessExpression { Member: IPropertySymbol } or BoundPropertyAccess)
            return BindPipelinePropertyAssignment(propertyTarget, syntax.Left, left, syntax.Right);

        _diagnostics.ReportPipeRequiresInvocation(syntax.OperatorToken.GetLocation());
        return ErrorExpression(reason: BoundExpressionReason.NotFound);
    }

    private BoundExpression BindPipelineInvocationTargetExpression(ExpressionSyntax expression, BoundExpression pipelineValue)
    {
        switch (expression)
        {
            case IdentifierNameSyntax identifier:
                {
                    var methodName = identifier.Identifier.ValueText;
                    var hasRegularSymbols = LookupSymbol(methodName) is not null || LookupSymbols(methodName).Any();
                    var hasExtensionCandidates = TryBindPipelineExtensionMethodGroup(methodName, pipelineValue, identifier.Identifier.GetLocation(), out var extensionGroup);

                    if (!hasRegularSymbols)
                        return hasExtensionCandidates
                            ? extensionGroup!
                            : BindIdentifierName(identifier);

                    var regularTarget = BindIdentifierName(identifier);
                    if (regularTarget is BoundMethodGroupExpression regularMethodGroup && hasExtensionCandidates)
                        return MergePipelineMethodGroupCandidates(regularMethodGroup, extensionGroup!.Methods);

                    return regularTarget;
                }
            case GenericNameSyntax generic:
                {
                    var methodName = generic.Identifier.ValueText;
                    var hasRegularSymbols = LookupSymbols(methodName).Any();
                    var hasExtensionCandidates = TryBindPipelineExtensionMethodGroup(methodName, pipelineValue, generic.Identifier.GetLocation(), out var extensionGroup);

                    if (!hasRegularSymbols && hasExtensionCandidates)
                        return extensionGroup!;

                    var regularTarget = BindGenericInvocationTarget(generic);
                    if (regularTarget is BoundMethodGroupExpression regularMethodGroup && hasExtensionCandidates)
                    {
                        return MergePipelineMethodGroupCandidates(regularMethodGroup, extensionGroup!.Methods);
                    }

                    return regularTarget;
                }
            default:
                return BindPipelineTargetExpression(expression);
        }
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

    private bool TryBindPipelineExtensionMethodGroup(
        string methodName,
        BoundExpression pipelineValue,
        Location location,
        out BoundMethodGroupExpression? methodGroup)
    {
        methodGroup = null;

        if (!IsExtensionReceiver(pipelineValue))
            return false;

        var receiverType = pipelineValue.Type.UnwrapLiteralType() ?? pipelineValue.Type;
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
            return false;

        var extensionCandidates = LookupExtensionMethods(methodName, receiverType).ToImmutableArray();
        if (extensionCandidates.IsDefaultOrEmpty)
            return false;

        var boundGroup = BindMethodGroup(null, extensionCandidates, location);
        methodGroup = boundGroup as BoundMethodGroupExpression;
        return methodGroup is not null;
    }

    private BoundMethodGroupExpression MergePipelineMethodGroupCandidates(
        BoundMethodGroupExpression regularMethodGroup,
        ImmutableArray<IMethodSymbol> extensionMethods)
    {
        if (extensionMethods.IsDefaultOrEmpty)
            return regularMethodGroup;

        var merged = ImmutableArray.CreateBuilder<IMethodSymbol>(regularMethodGroup.Methods.Length + extensionMethods.Length);
        var seen = new HashSet<IMethodSymbol>(SymbolEqualityComparer.Default);

        foreach (var method in regularMethodGroup.Methods)
        {
            if (seen.Add(method))
                merged.Add(method);
        }

        foreach (var method in extensionMethods)
        {
            if (seen.Add(method))
                merged.Add(method);
        }

        return CreateMethodGroup(regularMethodGroup.Receiver, merged.ToImmutable());
    }

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

        return BindTypeSyntaxAsExpression(generic);
    }

    private BoundExpression BindPipelineInvocationOnMethodGroup(
        BoundMethodGroupExpression methodGroup,
        SyntaxNode callSyntax,
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
            var resolution = OverloadResolver.ResolveOverload(extensionCandidates, boundArguments, Compilation, pipelineValue, EnsureLambdaCompatible, callSyntax);
            if (resolution.Success)
            {
                var method = resolution.Method!;
                ReportObsoleteIfNeeded(method, callSyntax.GetLocation());
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
                _diagnostics.ReportCallIsAmbiguous(methodName, resolution.AmbiguousCandidates, callSyntax.GetLocation());
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

            var resolution = OverloadResolver.ResolveOverload(staticCandidates, totalArguments, Compilation, canBindLambda: EnsureLambdaCompatible, callSyntax: callSyntax);
            if (resolution.Success)
            {
                var method = resolution.Method!;
                ReportObsoleteIfNeeded(method, callSyntax.GetLocation());
                var convertedArguments = ConvertPipelineStaticInvocationArguments(method, pipelineValue, pipelineSyntax, boundArguments, callSyntax);
                return new BoundInvocationExpression(method, convertedArguments, methodGroup.Receiver);
            }

            if (resolution.IsAmbiguous)
            {
                _diagnostics.ReportCallIsAmbiguous(methodName, resolution.AmbiguousCandidates, callSyntax.GetLocation());
                return ErrorExpression(
                    reason: BoundExpressionReason.Ambiguous,
                    candidates: AsSymbolCandidates(resolution.AmbiguousCandidates));
            }

            ReportSuppressedLambdaDiagnostics(totalArguments);
            _diagnostics.ReportNoOverloadForMethod("method", methodName, totalArguments.Length, callSyntax.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.OverloadResolutionFailed);
        }

        ReportSuppressedLambdaDiagnostics(PrependPipelineArgument(pipelineValue, pipelineSyntax, boundArguments));
        _diagnostics.ReportNoOverloadForMethod("method", methodName, boundArguments.Length + 1, callSyntax.GetLocation());
        return ErrorExpression(reason: BoundExpressionReason.OverloadResolutionFailed);
    }

    private BoundExpression BindPipelineInvocationOnBoundMethod(
        BoundMemberAccessExpression memberExpr,
        SyntaxNode callSyntax,
        ExpressionSyntax pipelineSyntax,
        BoundExpression pipelineValue,
        BoundArgument[] boundArguments)
    {
        if (memberExpr.Member is not IMethodSymbol method)
        {
            _diagnostics.ReportInvalidInvocation(callSyntax.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.NotFound);
        }

        if (method.IsExtensionMethod && IsExtensionReceiver(pipelineValue))
        {
            ReportObsoleteIfNeeded(method, callSyntax.GetLocation());
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
            _diagnostics.ReportInvalidInvocation(callSyntax.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.NotFound);
        }

        var totalCount = boundArguments.Length + 1;
        if (!SupportsArgumentCount(method.Parameters, totalCount))
        {
            ReportSuppressedLambdaDiagnostics(PrependPipelineArgument(pipelineValue, pipelineSyntax, boundArguments));
            _diagnostics.ReportNoOverloadForMethod("method", method.Name, totalCount, callSyntax.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.OverloadResolutionFailed);
        }

        var convertedArguments = ConvertPipelineStaticInvocationArguments(method, pipelineValue, pipelineSyntax, boundArguments, callSyntax);
        ReportObsoleteIfNeeded(method, callSyntax.GetLocation());
        return new BoundInvocationExpression(method, convertedArguments, memberExpr.Receiver);
    }

    private BoundExpression? BindPipelineInvocationOnDelegate(
        BoundExpression target,
        SyntaxNode callSyntax,
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
            _diagnostics.ReportInvalidInvocation(callSyntax.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.NotFound);
        }

        if (!EnsureMemberAccessible(invokeMethod, callSyntax.GetLocation(), GetSymbolKindForDiagnostic(invokeMethod)))
            return ErrorExpression(reason: BoundExpressionReason.Inaccessible);

        var totalCount = boundArguments.Length + 1;
        if (!SupportsArgumentCount(invokeMethod.Parameters, totalCount))
        {
            ReportSuppressedLambdaDiagnostics(PrependPipelineArgument(pipelineValue, pipelineSyntax, boundArguments));
            _diagnostics.ReportNoOverloadForMethod("method", invokeMethod.Name, totalCount, callSyntax.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.OverloadResolutionFailed);
        }

        var convertedArguments = ConvertPipelineStaticInvocationArguments(invokeMethod, pipelineValue, pipelineSyntax, boundArguments, callSyntax);
        return new BoundInvocationExpression(invokeMethod, convertedArguments, target);
    }

    private BoundExpression[] ConvertPipelineStaticInvocationArguments(
        IMethodSymbol method,
        BoundExpression pipelineValue,
        ExpressionSyntax pipelineSyntax,
        BoundArgument[] remainingArguments,
        SyntaxNode callSyntax)
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

    private bool TryBindPipelineImplicitInvocation(
        BoundExpression target,
        ExpressionSyntax pipelineSyntax,
        BoundExpression pipelineValue,
        ExpressionSyntax targetSyntax,
        out BoundExpression result)
    {
        var emptyArguments = Array.Empty<BoundArgument>();

        if (target is BoundMethodGroupExpression methodGroup)
        {
            result = BindPipelineInvocationOnMethodGroup(methodGroup, targetSyntax, pipelineSyntax, pipelineValue, emptyArguments);
            return true;
        }

        if (target is BoundMemberAccessExpression { Member: IMethodSymbol } memberExpr)
        {
            result = BindPipelineInvocationOnBoundMethod(memberExpr, targetSyntax, pipelineSyntax, pipelineValue, emptyArguments);
            return true;
        }

        var delegateInvocation = BindPipelineInvocationOnDelegate(target, targetSyntax, pipelineSyntax, pipelineValue, emptyArguments);
        if (delegateInvocation is not null)
        {
            result = delegateInvocation;
            return true;
        }

        result = null!;
        return false;
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

    private BoundExpression? BindUserDefinedBinaryOperator(
        SyntaxKind opKind,
        BoundExpression left,
        BoundExpression right,
        Location? diagnosticLocation,
        ExpressionSyntax? leftSyntax,
        ExpressionSyntax? rightSyntax,
        SyntaxNode? callSyntax)
    {
        if (!OperatorFacts.TryGetUserDefinedOperatorInfo(opKind, 2, out var operatorInfo))
            return null;

        var leftType = left.Type ?? Compilation.ErrorTypeSymbol;
        var rightType = right.Type ?? Compilation.ErrorTypeSymbol;
        leftType = leftType.UnwrapLiteralType() ?? leftType;
        rightType = rightType.UnwrapLiteralType() ?? rightType;

        if (TryBindLiftedUserDefinedEqualityOperator(
                opKind,
                operatorInfo.MetadataName,
                left,
                right,
                leftType,
                rightType,
                diagnosticLocation,
                leftSyntax,
                rightSyntax,
                callSyntax) is { } liftedEquality)
        {
            return liftedEquality;
        }

        var candidates = GetUserDefinedOperatorCandidates(
            operatorInfo.MetadataName,
            leftType,
            rightType,
            diagnosticLocation);

        if (candidates.IsDefaultOrEmpty)
            return null;

        var arguments = new[]
        {
            new BoundArgument(left, RefKind.None, null, leftSyntax),
            new BoundArgument(right, RefKind.None, null, rightSyntax),
        };

        var resolution = OverloadResolver.ResolveOverload(
            candidates,
            arguments,
            Compilation,
            canBindLambda: EnsureLambdaCompatible,
            callSyntax: callSyntax);

        if (resolution.IsAmbiguous)
        {
            var operatorText = OperatorFacts.GetDisplayText(opKind);
            _diagnostics.ReportCallIsAmbiguous($"operator {operatorText}", resolution.AmbiguousCandidates, diagnosticLocation ?? Location.None);
            return ErrorExpression(
                reason: BoundExpressionReason.Ambiguous,
                candidates: AsSymbolCandidates(resolution.AmbiguousCandidates));
        }

        if (!resolution.Success)
            return null;

        var method = resolution.Method!;
        ReportObsoleteIfNeeded(method, callSyntax?.GetLocation() ?? diagnosticLocation ?? Location.None);
        var converted = ConvertArguments(method.Parameters, arguments);
        return new BoundInvocationExpression(method, converted);
    }

    private BoundExpression? TryBindLiftedUserDefinedEqualityOperator(
        SyntaxKind opKind,
        string metadataName,
        BoundExpression left,
        BoundExpression right,
        ITypeSymbol leftType,
        ITypeSymbol rightType,
        Location? diagnosticLocation,
        ExpressionSyntax? leftSyntax,
        ExpressionSyntax? rightSyntax,
        SyntaxNode? callSyntax)
    {
        if (opKind is not SyntaxKind.EqualsEqualsToken and not SyntaxKind.NotEqualsToken)
            return null;

        var leftNullable = leftType as NullableTypeSymbol;
        var rightNullable = rightType as NullableTypeSymbol;

        var leftIsNullableValue = leftNullable is { UnderlyingType: { IsValueType: true } };
        var rightIsNullableValue = rightNullable is { UnderlyingType: { IsValueType: true } };

        if (!leftIsNullableValue && !rightIsNullableValue)
            return null;

        var candidateLeftType = leftIsNullableValue ? leftNullable!.UnderlyingType : leftType;
        var candidateRightType = rightIsNullableValue ? rightNullable!.UnderlyingType : rightType;

        var candidates = GetUserDefinedOperatorCandidates(
            metadataName,
            candidateLeftType,
            candidateRightType,
            diagnosticLocation);

        if (candidates.IsDefaultOrEmpty)
            return null;

        var unwrappedLeft = leftIsNullableValue
            ? new BoundNullableValueExpression(left, candidateLeftType)
            : left;
        var unwrappedRight = rightIsNullableValue
            ? new BoundNullableValueExpression(right, candidateRightType)
            : right;

        var liftedArguments = new[]
        {
            new BoundArgument(unwrappedLeft, RefKind.None, null, leftSyntax),
            new BoundArgument(unwrappedRight, RefKind.None, null, rightSyntax),
        };

        var resolution = OverloadResolver.ResolveOverload(
            candidates,
            liftedArguments,
            Compilation,
            canBindLambda: EnsureLambdaCompatible,
            callSyntax: callSyntax);

        if (resolution.IsAmbiguous)
        {
            var operatorText = OperatorFacts.GetDisplayText(opKind);
            _diagnostics.ReportCallIsAmbiguous($"operator {operatorText}", resolution.AmbiguousCandidates, diagnosticLocation ?? Location.None);
            return ErrorExpression(
                reason: BoundExpressionReason.Ambiguous,
                candidates: AsSymbolCandidates(resolution.AmbiguousCandidates));
        }

        if (!resolution.Success)
            return null;

        var method = resolution.Method!;
        ReportObsoleteIfNeeded(method, callSyntax?.GetLocation() ?? diagnosticLocation ?? Location.None);
        var convertedArguments = ConvertArguments(method.Parameters, liftedArguments);
        var invocation = (BoundExpression)new BoundInvocationExpression(method, convertedArguments);
        var isInequality = opKind == SyntaxKind.NotEqualsToken;

        if (leftIsNullableValue && rightIsNullableValue)
        {
            var leftHasValue = CreateNullableHasValueCondition(left);
            var rightHasValue = CreateNullableHasValueCondition(right);
            var mismatchResult = CreateBoolLiteral(isInequality);
            var bothNullResult = CreateBoolLiteral(!isInequality);
            var rightBranch = new BoundIfExpression(rightHasValue, mismatchResult, bothNullResult);
            var leftThenBranch = new BoundIfExpression(rightHasValue, invocation, mismatchResult);
            return new BoundIfExpression(leftHasValue, leftThenBranch, rightBranch);
        }

        var hasValue = leftIsNullableValue
            ? CreateNullableHasValueCondition(left)
            : CreateNullableHasValueCondition(right);
        var whenNull = CreateBoolLiteral(isInequality);
        return new BoundIfExpression(hasValue, invocation, whenNull);
    }

    private BoundExpression CreateNullableHasValueCondition(BoundExpression nullableOperand)
    {
        var nullableType = nullableOperand.Type;
        if (nullableType is null)
            return ErrorExpression(reason: BoundExpressionReason.NotFound);

        var nullLiteral = new BoundLiteralExpression(BoundLiteralExpressionKind.NullLiteral, null!, nullableType);
        if (!BoundBinaryOperator.TryLookup(Compilation, SyntaxKind.NotEqualsToken, nullableType, nullableType, out var notEquals))
            return ErrorExpression(reason: BoundExpressionReason.NotFound);

        return new BoundBinaryExpression(nullableOperand, notEquals, nullLiteral);
    }

    private BoundLiteralExpression CreateBoolLiteral(bool value)
    {
        var boolType = Compilation.GetSpecialType(SpecialType.System_Boolean);
        var kind = value ? BoundLiteralExpressionKind.TrueLiteral : BoundLiteralExpressionKind.FalseLiteral;
        return new BoundLiteralExpression(kind, value, boolType);
    }

    private BoundExpression? BindUserDefinedUnaryOperator(
        SyntaxKind opKind,
        BoundExpression operand,
        Location? diagnosticLocation,
        ExpressionSyntax? operandSyntax,
        SyntaxNode? callSyntax)
    {
        if (!OperatorFacts.TryGetUserDefinedOperatorInfo(opKind, 1, out var operatorInfo))
            return null;

        var operandType = operand.Type ?? Compilation.ErrorTypeSymbol;
        operandType = operandType.UnwrapLiteralType() ?? operandType;
        var candidates = GetUserDefinedOperatorCandidates(operatorInfo.MetadataName, operandType, diagnosticLocation);

        if (candidates.IsDefaultOrEmpty)
            return null;

        var arguments = new[]
        {
            new BoundArgument(operand, RefKind.None, null, operandSyntax),
        };

        var resolution = OverloadResolver.ResolveOverload(
            candidates,
            arguments,
            Compilation,
            canBindLambda: EnsureLambdaCompatible,
            callSyntax: callSyntax);

        if (resolution.IsAmbiguous)
        {
            var operatorText = OperatorFacts.GetDisplayText(opKind);
            _diagnostics.ReportCallIsAmbiguous($"operator {operatorText}", resolution.AmbiguousCandidates, diagnosticLocation ?? Location.None);
            return ErrorExpression(
                reason: BoundExpressionReason.Ambiguous,
                candidates: AsSymbolCandidates(resolution.AmbiguousCandidates));
        }

        if (!resolution.Success)
            return null;

        var method = resolution.Method!;
        ReportObsoleteIfNeeded(method, callSyntax?.GetLocation() ?? diagnosticLocation ?? Location.None);
        var converted = ConvertArguments(method.Parameters, arguments);
        return new BoundInvocationExpression(method, converted);
    }

    private ImmutableArray<IMethodSymbol> GetUserDefinedOperatorCandidates(
        string metadataName,
        ITypeSymbol operandType,
        Location? diagnosticLocation)
        => GetUserDefinedOperatorCandidates(metadataName, operandType, operandType, diagnosticLocation);

    private ImmutableArray<IMethodSymbol> GetUserDefinedOperatorCandidates(
        string metadataName,
        ITypeSymbol leftType,
        ITypeSymbol rightType,
        Location? diagnosticLocation)
    {
        var types = new List<ITypeSymbol> { leftType };

        if (!SymbolEqualityComparer.Default.Equals(leftType, rightType))
            types.Add(rightType);

        var candidates = ImmutableArray.CreateBuilder<IMethodSymbol>();
        var seen = new HashSet<IMethodSymbol>(SymbolEqualityComparer.Default);

        foreach (var type in types)
        {
            foreach (var method in type.GetMembers(metadataName).OfType<IMethodSymbol>())
            {
                if (method.MethodKind != MethodKind.UserDefinedOperator)
                    continue;

                if (!method.IsStatic || method.IsExtensionMethod)
                    continue;

                if (seen.Add(method))
                    candidates.Add(method);
            }

            foreach (var method in LookupExtensionStaticMethods(metadataName, type))
            {
                if (method.MethodKind != MethodKind.UserDefinedOperator)
                    continue;

                if (!method.IsStatic || method.IsExtensionMethod)
                    continue;

                if (seen.Add(method))
                    candidates.Add(method);
            }
        }

        if (candidates.Count == 0)
            return ImmutableArray<IMethodSymbol>.Empty;

        return GetAccessibleMethods(
            candidates.ToImmutable(),
            diagnosticLocation ?? Location.None,
            reportIfInaccessible: true);
    }

    private BoundExpression BindInvocationExpression(InvocationExpressionSyntax syntax)
    {
        BoundExpression? receiver;
        string methodName;

        if (syntax.Expression is MemberAccessExpressionSyntax memberAccess)
        {
            var boundMember = BindMemberAccessExpression(memberAccess, preferMethods: true, allowEventAccess: true);

            if (IsErrorExpression(boundMember))
                return boundMember is BoundErrorExpression boundError
                    ? boundError
                    : new BoundErrorExpression(boundMember.Type ?? Compilation.ErrorTypeSymbol, null, BoundExpressionReason.OtherError);

            if (boundMember is BoundMethodGroupExpression methodGroup)
                return BindInvocationOnMethodGroup(methodGroup, syntax);

            if (boundMember is BoundMemberAccessExpression { Member: IEventSymbol eventSymbol } eventAccess)
            {
                receiver = BindEventInvocationReceiver(eventSymbol, eventAccess, syntax.Expression);
                if (IsErrorExpression(receiver))
                    return receiver is BoundErrorExpression boundError
                        ? boundError
                        : new BoundErrorExpression(receiver.Type ?? Compilation.ErrorTypeSymbol, null, BoundExpressionReason.OtherError);

                methodName = "Invoke";
            }
            else if (boundMember is BoundMemberAccessExpression { Member: IMethodSymbol method } memberExpr)
            {
                var argExprs = BindInvocationArguments(syntax.ArgumentList.Arguments, method, memberExpr.Receiver, out var argErrors);

                if (argErrors)
                {
                    var candidates = ImmutableArray.Create(method);
                    return ErrorExpression(
                        method.ReturnType,
                        method,
                        BoundExpressionReason.ArgumentBindingFailed,
                        AsSymbolCandidates(candidates));
                }

                if (AreArgumentsCompatibleWithMethod(method, argExprs.Length, memberExpr.Receiver, argExprs))
                {
                    var convertedArgs = ConvertArguments(method.Parameters, argExprs);
                    ReportObsoleteIfNeeded(method, syntax.Expression.GetLocation());
                    return new BoundInvocationExpression(method, convertedArgs, memberExpr.Receiver);
                }

                receiver = memberExpr.Receiver;
                methodName = method.Name;
            }
            else if (boundMember is BoundTypeExpression { Type: INamedTypeSymbol namedType })
            {
                // If the callee binds to a type, `TypeName(...)` is a constructor invocation.
                // Bind arguments with ctor-parameter target types so member bindings like `.Human` / `.Male` can resolve.
                return BindConstructorInvocation(namedType, syntax, receiverSyntax: syntax.Expression, receiver: null);
            }
            else
            {
                receiver = boundMember;
                methodName = "Invoke";
            }
        }
        else if (syntax.Expression is MemberBindingExpressionSyntax memberBinding)
        {
            var invocationTargetType = GetTargetType(syntax);
            var boundMember = invocationTargetType is not null && invocationTargetType.TypeKind != TypeKind.Error
                ? BindMemberBindingExpression(memberBinding, invocationTargetType, allowEventAccess: true)
                : BindMemberBindingExpression(memberBinding, allowEventAccess: true);

            if (IsErrorExpression(boundMember))
                return boundMember is BoundErrorExpression boundError
                    ? boundError
                    : new BoundErrorExpression(boundMember.Type ?? Compilation.ErrorTypeSymbol, null, BoundExpressionReason.OtherError);

            if (boundMember is BoundMethodGroupExpression methodGroup)
                return BindInvocationOnMethodGroup(methodGroup, syntax);

            if (boundMember is BoundMemberAccessExpression { Member: IEventSymbol eventSymbol } eventAccess)
            {
                receiver = BindEventInvocationReceiver(eventSymbol, eventAccess, syntax.Expression);
                if (IsErrorExpression(receiver))
                    return receiver is BoundErrorExpression boundError
                        ? boundError
                        : new BoundErrorExpression(receiver.Type ?? Compilation.ErrorTypeSymbol, null, BoundExpressionReason.OtherError);

                methodName = "Invoke";
            }
            else if (boundMember is BoundMemberAccessExpression { Member: IMethodSymbol method } memberExpr)
            {
                var argExprs = BindInvocationArguments(syntax.ArgumentList.Arguments, method, memberExpr.Receiver, out var argErrors);

                if (argErrors)
                {
                    var candidates = ImmutableArray.Create(method);
                    return ErrorExpression(
                        method.ReturnType,
                        method,
                        BoundExpressionReason.ArgumentBindingFailed,
                        AsSymbolCandidates(candidates));
                }

                if (AreArgumentsCompatibleWithMethod(method, argExprs.Length, memberExpr.Receiver, argExprs))
                {
                    var convertedArgs = ConvertArguments(method.Parameters, argExprs);
                    ReportObsoleteIfNeeded(method, syntax.Expression.GetLocation());
                    return new BoundInvocationExpression(method, convertedArgs, memberExpr.Receiver);
                }

                receiver = memberExpr.Receiver;
                methodName = method.Name;
            }
            else if (boundMember is BoundTypeExpression { Type: INamedTypeSymbol namedType })
            {
                // If the callee binds to a type, `TypeName(...)` is a constructor invocation.
                // Bind arguments with ctor-parameter target types so member bindings like `.Human` / `.Male` can resolve.
                return BindConstructorInvocation(namedType, syntax, receiverSyntax: syntax.Expression, receiver: null);
            }
            else
            {
                receiver = boundMember;
                methodName = "Invoke";
            }
        }
        else if (syntax.Expression is IdentifierNameSyntax id)
        {
            var boundIdentifier = BindIdentifierName(id, allowEventAccess: true);
            if (IsErrorExpression(boundIdentifier))
                return boundIdentifier is BoundErrorExpression boundError
                    ? boundError
                    : new BoundErrorExpression(boundIdentifier.Type ?? Compilation.ErrorTypeSymbol, null, BoundExpressionReason.OtherError);

            if (boundIdentifier is BoundMethodGroupExpression methodGroup)
                return BindInvocationOnMethodGroup(methodGroup, syntax);

            if (boundIdentifier is BoundMemberAccessExpression { Member: IEventSymbol eventSymbol } eventAccess)
            {
                receiver = BindEventInvocationReceiver(eventSymbol, eventAccess, syntax.Expression);
                if (IsErrorExpression(receiver))
                    return receiver is BoundErrorExpression boundError
                        ? boundError
                        : new BoundErrorExpression(receiver.Type ?? Compilation.ErrorTypeSymbol, null, BoundExpressionReason.OtherError);

                methodName = "Invoke";
            }
            else if (boundIdentifier is BoundLocalAccess or BoundParameterAccess or BoundFieldAccess or BoundPropertyAccess)
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
                var diagnosticCountBeforeInstantiation = _diagnostics.AsEnumerable().Count();
                var instantiated = InstantiateMethodCandidates(symbolCandidates, boundTypeArguments.Value, generic, syntax.GetLocation());
                if (!instantiated.IsDefaultOrEmpty)
                {
                    var methodGroup = CreateMethodGroup(null, instantiated);
                    return BindInvocationOnMethodGroup(methodGroup, syntax);
                }

                var producedInstantiationDiagnostics = _diagnostics.AsEnumerable().Count() > diagnosticCountBeforeInstantiation;
                if (producedInstantiationDiagnostics)
                    return ErrorExpression(reason: BoundExpressionReason.OverloadResolutionFailed);

                _diagnostics.ReportNoOverloadForMethod("method", generic.Identifier.ValueText, syntax.ArgumentList.Arguments.Count, syntax.GetLocation());
                return ErrorExpression(reason: BoundExpressionReason.OverloadResolutionFailed);
            }

            var typeExpr = BindTypeSyntaxAsExpression(generic);
            if (typeExpr is BoundTypeExpression type && type.Type is INamedTypeSymbol namedType)
            {
                // If the callee binds to a type, `TypeName(...)` is a constructor invocation.
                // Bind arguments with ctor-parameter target types so member bindings like `.Human` / `.Male` can resolve.
                return BindConstructorInvocation(namedType, syntax, receiverSyntax: syntax.Expression, receiver: null);
            }

            return ErrorExpression(reason: BoundExpressionReason.NotFound);
        }
        else
        {
            receiver = BindExpression(syntax.Expression);

            if (IsErrorExpression(receiver))
                return receiver is BoundErrorExpression boundError
                    ? boundError
                    : new BoundErrorExpression(receiver.Type ?? Compilation.ErrorTypeSymbol, null, BoundExpressionReason.OtherError);

            // If the callee binds to a type, `TypeName(...)` is a constructor invocation.
            // Bind arguments with ctor-parameter target types so member bindings like `.Human` / `.Male` can resolve.
            if (receiver is BoundTypeExpression typeExpr && typeExpr.Type is INamedTypeSymbol namedType)
            {
                return BindConstructorInvocation(namedType, syntax, receiverSyntax: syntax.Expression, receiver: null);
            }

            methodName = "Invoke";
        }

        return BindInvocationExpressionCore(receiver, methodName, syntax.ArgumentList, syntax.Expression, syntax);
    }

    private BoundExpression BindInvocationExpressionCore(
        BoundExpression? receiver,
        string methodName,
        ArgumentListSyntax argumentList,
        SyntaxNode receiverSyntax,
        SyntaxNode callSyntax,
        bool suppressNullWarning = false)
    {
        if (receiver is not null && HasExpressionErrors(receiver))
            return AsErrorExpression(receiver);

        if (!suppressNullWarning)
            ReportPossibleNullReferenceAccess(receiver, receiverSyntax);

        // Bind invocation arguments AFTER we have a candidate set, so we can provide
        // best-effort target types for target-typed member bindings like `.Human`, `.Male`, `.Ok(...)`, etc.

        // Delegate invocation: receiver.Invoke(...)
        if (receiver is not null)
        {
            var receiverType = receiver.Type.UnwrapLiteralType() ?? receiver.Type;

            if (methodName == "Invoke" &&
                receiverType is INamedTypeSymbol { TypeKind: TypeKind.Delegate } delegateType &&
                delegateType.GetDelegateInvokeMethod() is { } invokeMethod)
            {
                if (!EnsureMemberAccessible(invokeMethod, receiverSyntax.GetLocation(), GetSymbolKindForDiagnostic(invokeMethod)))
                    return ErrorExpression(reason: BoundExpressionReason.Inaccessible);

                var invokeForArgBinding = ImmutableArray.Create(invokeMethod);
                invokeForArgBinding = FilterInvocationCandidatesForArgumentBinding(invokeForArgBinding, argumentList.Arguments);

                var boundArguments = BindInvocationArgumentsWithCandidateTargetTypes(invokeForArgBinding, argumentList.Arguments, out var hasErrors);
                if (hasErrors)
                    return InvocationError(receiver, methodName, BoundExpressionReason.ArgumentBindingFailed);

                if (!AreArgumentsCompatibleWithMethod(invokeMethod, boundArguments.Length, receiver, boundArguments))
                {
                    ReportSuppressedLambdaDiagnostics(boundArguments);
                    _diagnostics.ReportNoOverloadForMethod("method", methodName, boundArguments.Length, callSyntax.GetLocation());
                    return ErrorExpression(reason: BoundExpressionReason.OverloadResolutionFailed);
                }

                var converted = ConvertArguments(invokeMethod.Parameters, boundArguments);
                ReportObsoleteIfNeeded(invokeMethod, callSyntax.GetLocation());
                return new BoundInvocationExpression(invokeMethod, converted, receiver);
            }
        }

        // Namespace receiver: Namespace.TypeName(...)
        if (receiver is BoundNamespaceExpression nsReceiver)
        {
            var typeInNs = nsReceiver.Namespace
                .GetMembers(methodName)
                .OfType<INamedTypeSymbol>()
                .FirstOrDefault();

            if (typeInNs is null)
            {
                _diagnostics.ReportTypeOrNamespaceNameDoesNotExistInTheNamespace(methodName, nsReceiver.Namespace.Name, receiverSyntax.GetLocation());
                return ErrorExpression(reason: BoundExpressionReason.NotFound);
            }

            // Rebind arguments against ctor parameter types so target-typed member bindings can resolve.
            if (callSyntax is InvocationExpressionSyntax inv)
                return BindConstructorInvocation(typeInNs, inv, receiverSyntax, receiver);

            // Fallback: should not normally happen, but preserve behavior.
            var fallbackArgs = BindInvocationArguments(argumentList.Arguments, out var fallbackHasErrors);
            if (fallbackHasErrors)
                return InvocationError(receiver, methodName, BoundExpressionReason.ArgumentBindingFailed);
            return BindConstructorInvocation(typeInNs, fallbackArgs, callSyntax, receiverSyntax, receiver);
        }

        // Static call on type receiver: TypeName.Method(...)
        if (receiver is BoundTypeExpression typeReceiver)
        {
            var candidates = new SymbolQuery(methodName, typeReceiver.Type, IsStatic: true)
                .LookupMethods(this)
                .ToImmutableArray();

            if (!candidates.IsDefaultOrEmpty)
            {
                var accessibleCandidates = GetAccessibleMethods(candidates, receiverSyntax.GetLocation());

                // Bind args against accessible candidates if possible; otherwise bind against the full
                // candidate set so we still get target typing and diagnostics.
                var candidatesForArgBinding = !accessibleCandidates.IsDefaultOrEmpty ? accessibleCandidates : candidates;
                candidatesForArgBinding = FilterInvocationCandidatesForArgumentBinding(candidatesForArgBinding, argumentList.Arguments);

                var boundArguments = BindInvocationArgumentsWithCandidateTargetTypes(candidatesForArgBinding, argumentList.Arguments, out var hasErrors);
                if (hasErrors)
                    return InvocationError(receiver, methodName, BoundExpressionReason.ArgumentBindingFailed);

                if (accessibleCandidates.IsDefaultOrEmpty)
                    return ErrorExpression(reason: BoundExpressionReason.Inaccessible);

                var resolution = OverloadResolver.ResolveOverload(accessibleCandidates, boundArguments, Compilation, canBindLambda: EnsureLambdaCompatible, callSyntax: callSyntax);
                if (resolution.Success)
                {
                    var method = resolution.Method!;
                    ReportObsoleteIfNeeded(method, callSyntax.GetLocation());
                    var convertedArgs = ConvertArguments(method.Parameters, boundArguments);
                    return new BoundInvocationExpression(method, convertedArgs, receiver);
                }

                if (resolution.IsAmbiguous)
                {
                    _diagnostics.ReportCallIsAmbiguous(methodName, resolution.AmbiguousCandidates, callSyntax.GetLocation());
                    return ErrorExpression(
                        reason: BoundExpressionReason.Ambiguous,
                        candidates: AsSymbolCandidates(resolution.AmbiguousCandidates));
                }

                var nestedType = typeReceiver.Type
                    .GetMembers(methodName)
                    .OfType<INamedTypeSymbol>()
                    .FirstOrDefault();

                if (nestedType is not null)
                {
                    if (callSyntax is InvocationExpressionSyntax inv)
                        return BindConstructorInvocation(nestedType, inv, receiverSyntax, receiver);

                    var fallbackArgs = BindInvocationArguments(argumentList.Arguments, out var fallbackHasErrors);
                    if (fallbackHasErrors)
                        return InvocationError(receiver, methodName, BoundExpressionReason.ArgumentBindingFailed);
                    return BindConstructorInvocation(nestedType, fallbackArgs, callSyntax, receiverSyntax, receiver);
                }

                ReportSuppressedLambdaDiagnostics(boundArguments);
                _diagnostics.ReportNoOverloadForMethod("method", methodName, boundArguments.Length, callSyntax.GetLocation());
                return ErrorExpression(reason: BoundExpressionReason.OverloadResolutionFailed);
            }

            var nested = typeReceiver.Type
                .GetMembers(methodName)
                .OfType<INamedTypeSymbol>()
                .FirstOrDefault();

            if (nested is not null)
            {
                if (callSyntax is InvocationExpressionSyntax inv)
                    return BindConstructorInvocation(nested, inv, receiverSyntax, receiver);

                var fallbackArgs = BindInvocationArguments(argumentList.Arguments, out var fallbackHasErrors);
                if (fallbackHasErrors)
                    return InvocationError(receiver, methodName, BoundExpressionReason.ArgumentBindingFailed);
                return BindConstructorInvocation(nested, fallbackArgs, callSyntax, receiverSyntax, receiver);
            }

            _diagnostics.ReportMemberDoesNotContainDefinition(typeReceiver.Type.Name, methodName, receiverSyntax.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.NotFound);
        }

        // Instance receiver call: receiver.Method(...)
        if (receiver is not null)
        {
            var candidates = new SymbolQuery(methodName, receiver.Type, IsStatic: false)
                .LookupMethods(this)
                .ToImmutableArray();

            if (candidates.IsDefaultOrEmpty)
            {
                if (methodName == "Invoke")
                {
                    if (TryGetInvokedMemberName(receiverSyntax, out var memberName))
                        _diagnostics.ReportNonInvocableMember(memberName, receiverSyntax.GetLocation());
                    else
                        _diagnostics.ReportInvalidInvocation(receiverSyntax.GetLocation());
                }
                else
                    _diagnostics.ReportMemberDoesNotContainDefinition(receiver.Type.Name, methodName, receiverSyntax.GetLocation());

                return ErrorExpression(reason: BoundExpressionReason.NotFound);
            }

            var accessibleCandidates = GetAccessibleMethods(candidates, receiverSyntax.GetLocation());
            var candidatesForArgBinding = !accessibleCandidates.IsDefaultOrEmpty ? accessibleCandidates : candidates;
            candidatesForArgBinding = FilterInvocationCandidatesForArgumentBinding(candidatesForArgBinding, argumentList.Arguments);

            var boundArguments = BindInvocationArgumentsWithCandidateTargetTypes(candidatesForArgBinding, argumentList.Arguments, out var hasErrors);
            if (hasErrors)
                return InvocationError(receiver, methodName, BoundExpressionReason.ArgumentBindingFailed);

            if (accessibleCandidates.IsDefaultOrEmpty)
                return ErrorExpression(reason: BoundExpressionReason.Inaccessible);

            var resolution = OverloadResolver.ResolveOverload(accessibleCandidates, boundArguments, Compilation, canBindLambda: EnsureLambdaCompatible, callSyntax: callSyntax);
            if (resolution.Success)
            {
                var method = resolution.Method!;
                ReportObsoleteIfNeeded(method, callSyntax.GetLocation());
                var convertedArgs = ConvertArguments(method.Parameters, boundArguments);
                return new BoundInvocationExpression(method, convertedArgs, receiver);
            }

            if (resolution.IsAmbiguous)
            {
                _diagnostics.ReportCallIsAmbiguous(methodName, resolution.AmbiguousCandidates, callSyntax.GetLocation());
                return ErrorExpression(
                    reason: BoundExpressionReason.Ambiguous,
                    candidates: AsSymbolCandidates(resolution.AmbiguousCandidates));
            }

            _diagnostics.ReportNoOverloadForMethod("method", methodName, boundArguments.Length, callSyntax.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.OverloadResolutionFailed);
        }

        // Global function call: MethodName(...)
        var methodCandidates = new SymbolQuery(methodName)
            .LookupMethods(this)
            .ToImmutableArray();

        if (!methodCandidates.IsDefaultOrEmpty)
        {
            var accessibleMethods = GetAccessibleMethods(methodCandidates, receiverSyntax.GetLocation());
            var candidatesForArgBinding = !accessibleMethods.IsDefaultOrEmpty ? accessibleMethods : methodCandidates;
            candidatesForArgBinding = FilterInvocationCandidatesForArgumentBinding(candidatesForArgBinding, argumentList.Arguments);

            var boundArguments = BindInvocationArgumentsWithCandidateTargetTypes(candidatesForArgBinding, argumentList.Arguments, out var hasErrors);
            if (hasErrors)
                return InvocationError(null, methodName, BoundExpressionReason.ArgumentBindingFailed);

            if (accessibleMethods.IsDefaultOrEmpty)
                return ErrorExpression(reason: BoundExpressionReason.Inaccessible);

            var resolution = OverloadResolver.ResolveOverload(accessibleMethods, boundArguments, Compilation, canBindLambda: EnsureLambdaCompatible, callSyntax: callSyntax);
            if (resolution.Success)
            {
                var method = resolution.Method!;
                ReportObsoleteIfNeeded(method, callSyntax.GetLocation());
                var convertedArgs = ConvertArguments(method.Parameters, boundArguments);
                return new BoundInvocationExpression(method, convertedArgs, null);
            }

            if (resolution.IsAmbiguous)
            {
                _diagnostics.ReportCallIsAmbiguous(methodName, resolution.AmbiguousCandidates, callSyntax.GetLocation());
                return ErrorExpression(
                    reason: BoundExpressionReason.Ambiguous,
                    candidates: AsSymbolCandidates(resolution.AmbiguousCandidates));
            }

            if (LookupType(methodName) is INamedTypeSymbol { TypeKind: not TypeKind.Error } typeFallback)
            {
                // Rebind arguments against ctor parameter types so target-typed member bindings can resolve.
                if (callSyntax is InvocationExpressionSyntax inv)
                    return BindConstructorInvocation(typeFallback, inv, receiverSyntax: receiverSyntax, receiver: null);

                return BindConstructorInvocation(typeFallback, boundArguments, callSyntax, receiverSyntax, receiver: null);
            }

            ReportSuppressedLambdaDiagnostics(boundArguments);
            _diagnostics.ReportNoOverloadForMethod("method", methodName, boundArguments.Length, callSyntax.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.OverloadResolutionFailed);
        }

        if (LookupType(methodName) is INamedTypeSymbol { TypeKind: not TypeKind.Error } typeSymbol)
        {
            if (callSyntax is InvocationExpressionSyntax inv)
                return BindConstructorInvocation(typeSymbol, inv, receiverSyntax: receiverSyntax, receiver: null);

            var fallbackArgs = BindInvocationArguments(argumentList.Arguments, out var fallbackHasErrors);
            if (fallbackHasErrors)
                return InvocationError(null, methodName, BoundExpressionReason.ArgumentBindingFailed);
            return BindConstructorInvocation(typeSymbol, fallbackArgs, callSyntax, receiverSyntax, receiver: null);
        }

        _diagnostics.ReportTheNameDoesNotExistInTheCurrentContext(methodName, receiverSyntax.GetLocation());
        return ErrorExpression(reason: BoundExpressionReason.NotFound);
    }

    private static bool TryGetInvokedMemberName(SyntaxNode receiverSyntax, out string memberName)
    {
        switch (receiverSyntax)
        {
            case IdentifierNameSyntax identifier:
                memberName = identifier.Identifier.ValueText;
                return !string.IsNullOrEmpty(memberName);
            case GenericNameSyntax generic:
                memberName = generic.Identifier.ValueText;
                return !string.IsNullOrEmpty(memberName);
            case MemberAccessExpressionSyntax memberAccess:
                memberName = memberAccess.Name.Identifier.ValueText;
                return !string.IsNullOrEmpty(memberName);
            case MemberBindingExpressionSyntax memberBinding:
                memberName = memberBinding.Name.Identifier.ValueText;
                return !string.IsNullOrEmpty(memberName);
            default:
                memberName = string.Empty;
                return false;
        }
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

    private BoundArgument[] BindInvocationArguments(
        SeparatedSyntaxList<ArgumentSyntax> arguments,
        IMethodSymbol targetMethod,
        BoundExpression? receiver,
        out bool hasErrors)
    {
        if (arguments.Count == 0)
        {
            hasErrors = false;
            return Array.Empty<BoundArgument>();
        }

        var boundArguments = new BoundArgument[arguments.Count];
        var seenErrors = false;
        var extensionReceiverImplicit = targetMethod.IsExtensionMethod && IsExtensionReceiver(receiver);

        for (int i = 0; i < arguments.Count; i++)
        {
            var syntax = arguments[i];
            var targetType = TryGetLambdaParameter(targetMethod, i, extensionReceiverImplicit, out var parameter)
                ? parameter!.Type
                : null;
            var boundArg = BindExpressionWithTargetType(syntax.Expression, targetType);
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

    private void ReportPossibleNullReferenceAccess(BoundExpression? receiver, SyntaxNode receiverSyntax)
    {
        if (receiver is null)
            return;

        if (receiver is BoundTypeExpression or BoundNamespaceExpression)
            return;

        if (receiver.Type is null || !receiver.Type.IsNullable)
            return;

        if (IsReceiverKnownNotNull(receiver))
            return;

        _diagnostics.ReportPossibleNullReferenceAccess(receiverSyntax.GetLocation());
    }

    private BoundExpression UnwrapNullableIfKnownNonNull(BoundExpression expr, ISymbol symbol)
    {
        if (!_nonNullSymbols.Contains(symbol))
            return expr;

        if (expr.Type is NullableTypeSymbol n && n.UnderlyingType.IsValueType)
            return new BoundNullableValueExpression(expr, n.UnderlyingType);

        return expr;
    }

    private void ClearNonNullSymbolsAtDepth(int depth)
    {
        foreach (var local in _locals.Where(kvp => kvp.Value.Depth == depth).Select(kvp => kvp.Value.Symbol))
            _nonNullSymbols.Remove(local);
    }

    private void ClearNullableFlowOnAssignment(BoundAssignmentExpression assignment)
    {
        switch (assignment)
        {
            case BoundLocalAssignmentExpression localAssignment:
                _nonNullSymbols.Remove(localAssignment.Local);
                break;
            case BoundParameterAssignmentExpression parameterAssignment:
                _nonNullSymbols.Remove(parameterAssignment.Parameter);
                break;
        }
    }

    private bool IsReceiverKnownNotNull(BoundExpression receiver)
    {
        if (TryGetFlowSymbol(receiver, out var symbol))
            return _nonNullSymbols.Contains(symbol);

        return false;
    }

    private static bool IsNullLiteral(BoundExpression expression)
    {
        return expression is BoundLiteralExpression { Kind: BoundLiteralExpressionKind.NullLiteral };
    }

    private static BoundExpression UnwrapFlowExpression(BoundExpression expression)
    {
        while (expression is BoundParenthesizedExpression parenthesized)
            expression = parenthesized.Expression;

        return expression;
    }

    private bool TryGetFlowSymbol(BoundExpression expression, out ISymbol symbol)
    {
        expression = UnwrapFlowExpression(expression);

        switch (expression)
        {
            case BoundLocalAccess localAccess:
                symbol = localAccess.Local;
                return true;
            case BoundVariableExpression variableExpression:
                symbol = variableExpression.Variable;
                return true;
            case BoundParameterAccess parameterAccess:
                symbol = parameterAccess.Parameter;
                return true;
            default:
                symbol = null!;
                return false;
        }
    }

    private bool TryGetNullCheckFlow(
        BoundExpression condition,
        out ISymbol symbol,
        out bool nonNullWhenTrue,
        out bool nonNullWhenFalse)
    {
        condition = UnwrapFlowExpression(condition);

        if (condition is BoundBinaryExpression binary &&
            (binary.Operator.OperatorKind == BinaryOperatorKind.Equality || binary.Operator.OperatorKind == BinaryOperatorKind.Inequality))
        {
            if (IsNullLiteral(binary.Left) && TryGetFlowSymbol(binary.Right, out symbol))
            {
                nonNullWhenTrue = binary.Operator.OperatorKind == BinaryOperatorKind.Inequality;
                nonNullWhenFalse = binary.Operator.OperatorKind == BinaryOperatorKind.Equality;
                return true;
            }

            if (IsNullLiteral(binary.Right) && TryGetFlowSymbol(binary.Left, out symbol))
            {
                nonNullWhenTrue = binary.Operator.OperatorKind == BinaryOperatorKind.Inequality;
                nonNullWhenFalse = binary.Operator.OperatorKind == BinaryOperatorKind.Equality;
                return true;
            }
        }

        if (condition is BoundIsPatternExpression isPattern &&
            TryGetFlowSymbol(isPattern.Expression, out symbol) &&
            TryGetNullPatternFlow(isPattern.Pattern, out var patternNonNullWhenTrue))
        {
            nonNullWhenTrue = patternNonNullWhenTrue;
            nonNullWhenFalse = !patternNonNullWhenTrue;
            return true;
        }

        symbol = null!;
        nonNullWhenTrue = false;
        nonNullWhenFalse = false;
        return false;
    }

    private static bool TryGetNullPatternFlow(BoundPattern pattern, out bool nonNullWhenTrue)
    {
        if (pattern is BoundConstantPattern constantPattern && constantPattern.ConstantValue is null)
        {
            nonNullWhenTrue = false;
            return true;
        }

        if (pattern is BoundNotPattern { Pattern: BoundConstantPattern constant } && constant.ConstantValue is null)
        {
            nonNullWhenTrue = true;
            return true;
        }

        nonNullWhenTrue = false;
        return false;
    }

    private static HashSet<ISymbol> IntersectFlowStates(HashSet<ISymbol> left, HashSet<ISymbol> right)
    {
        if (left.Count == 0 || right.Count == 0)
            return new HashSet<ISymbol>(SymbolEqualityComparer.Default);

        var result = new HashSet<ISymbol>(left, SymbolEqualityComparer.Default);
        result.IntersectWith(right);
        return result;
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

        BoundPattern bound = patternSyntax switch
        {
            VariablePatternSyntax variablePattern => BindVariablePatternForAssignment(variablePattern, valueType),
            PositionalPatternSyntax tuplePattern => BindPositionalPatternForAssignment(tuplePattern, valueType),
            DiscardPatternSyntax => new BoundDiscardPattern(valueType.TypeKind == TypeKind.Error ? Compilation.ErrorTypeSymbol : valueType),
            DeclarationPatternSyntax declaration => BindDeclarationPatternForAssignment(declaration, valueType, node),
            _ => Misc(node)
        };

        CacheBoundNode(patternSyntax, bound);
        return bound;
    }

    private BoundPattern Misc(SyntaxNode node)
    {
        _diagnostics.ReportLeftOfAssignmentMustBeAVariablePropertyOrIndexer(node.GetLocation());
        return new BoundDiscardPattern(Compilation.ErrorTypeSymbol, BoundExpressionReason.UnsupportedOperation);
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

    private BoundPattern BindPositionalPatternForAssignment(PositionalPatternSyntax pattern, ITypeSymbol valueType)
    {
        var elements = pattern.Elements;
        var elementCount = elements.Count;

        if (valueType.TypeKind != TypeKind.Error)
        {
            var deconstructMethod = FindDeconstructMethod(valueType, elementCount);
            if (deconstructMethod is not null)
                return BindDeconstructPatternForAssignment(elements, deconstructMethod, valueType);
        }

        var elementTypes = ImmutableArray<ITypeSymbol>.Empty;
        if (valueType.TypeKind != TypeKind.Error)
            elementTypes = GetTupleElementTypes(valueType);

        if (elementTypes.IsDefaultOrEmpty)
        {
            if (elementCount > 0 && valueType.TypeKind != TypeKind.Error)
            {
                _diagnostics.ReportPositionalDeconstructionRequiresDeconstructableType(
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
            _diagnostics.ReportPositionalDeconstructionElementCountMismatch(
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
        return new BoundPositionalPattern(tupleType, boundElements.ToImmutable());
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
        var declaredType = ResolveTypeSyntaxOrError(typedDesignation.TypeAnnotation.Type);
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

    private static bool RequiresByRefType(TypeSyntax typeSyntax, RefKind refKindHint)
        => refKindHint is RefKind.Ref or RefKind.Out or RefKind.In or RefKind.RefReadOnly or RefKind.RefReadOnlyParameter ||
           typeSyntax is ByRefTypeSyntax;

    private ITypeSymbol ResolveTypeSyntaxOrError(TypeSyntax typeSyntax, RefKind refKindHint = RefKind.None)
    {
        var result = BindTypeSyntax(typeSyntax);
        if (!result.Success)
            return BindTypeSyntaxDirect(typeSyntax, refKindHint);

        var resolved = result.ResolvedType;
        if (RequiresByRefType(typeSyntax, refKindHint) && resolved is not RefTypeSymbol)
            return new RefTypeSymbol(resolved);

        return resolved;
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
        CacheBoundNode(single, designator);

        return new BoundDeclarationPattern(type, designator);
    }

    private BoundPattern BindParenthesizedDesignationForAssignment(
        ParenthesizedVariableDesignationSyntax parenthesized,
        ITypeSymbol valueType,
        bool isMutable)
    {
        var variables = parenthesized.Variables;
        var elementCount = variables.Count;

        if (valueType.TypeKind != TypeKind.Error)
        {
            var deconstructMethod = FindDeconstructMethod(valueType, elementCount);
            if (deconstructMethod is not null)
                return BindDeconstructPatternForAssignment(variables, deconstructMethod, valueType, isMutable);
        }

        var elementTypes = ImmutableArray<ITypeSymbol>.Empty;
        if (valueType.TypeKind != TypeKind.Error)
            elementTypes = GetTupleElementTypes(valueType);

        if (elementTypes.IsDefaultOrEmpty)
        {
            if (elementCount > 0 && valueType.TypeKind != TypeKind.Error)
            {
                _diagnostics.ReportPositionalDeconstructionRequiresDeconstructableType(
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
            _diagnostics.ReportPositionalDeconstructionElementCountMismatch(
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
        return new BoundPositionalPattern(tupleType, boundElements.ToImmutable());
    }

    private BoundPattern BindDeconstructPatternForAssignment(
        SeparatedSyntaxList<PositionalPatternElementSyntax> elements,
        IMethodSymbol deconstructMethod,
        ITypeSymbol valueType)
    {
        var fallbackLocation = elements.Count > 0 ? elements[0].GetLocation() : Location.None;
        var parameterOffset = GetDeconstructParameterOffset(deconstructMethod);
        var parameters = deconstructMethod.Parameters;
        var parameterCount = parameters.Length - parameterOffset;
        var boundElements = ImmutableArray.CreateBuilder<BoundPattern>(parameterCount);
        var elementCount = Math.Min(elements.Count, parameterCount);

        for (var i = 0; i < elementCount; i++)
        {
            var elementSyntax = elements[i];
            var expectedType = EnsureTypeAccessible(parameters[i + parameterOffset].Type, elementSyntax.GetLocation());
            var boundElement = BindPatternForAssignment(elementSyntax.Pattern, expectedType, elementSyntax.Pattern);
            boundElements.Add(boundElement);
        }

        for (var i = elementCount; i < parameterCount; i++)
        {
            var parameterType = EnsureTypeAccessible(parameters[i + parameterOffset].Type, fallbackLocation);
            boundElements.Add(new BoundDiscardPattern(parameterType, BoundExpressionReason.TypeMismatch));
        }

        for (var i = elementCount; i < elements.Count; i++)
            _ = BindPatternForAssignment(elements[i].Pattern, Compilation.ErrorTypeSymbol, elements[i].Pattern);

        return new BoundDeconstructPattern(
            inputType: valueType,
            receiverType: GetDeconstructReceiverType(deconstructMethod),
            narrowedType: null,
            deconstructMethod: deconstructMethod,
            arguments: boundElements.ToImmutable());
    }

    private BoundPattern BindDeconstructPatternForAssignment(
        SeparatedSyntaxList<VariableDesignationSyntax> designations,
        IMethodSymbol deconstructMethod,
        ITypeSymbol valueType,
        bool isMutable)
    {
        var fallbackLocation = designations.Count > 0 ? designations[0].GetLocation() : Location.None;
        var parameterOffset = GetDeconstructParameterOffset(deconstructMethod);
        var parameters = deconstructMethod.Parameters;
        var parameterCount = parameters.Length - parameterOffset;
        var boundElements = ImmutableArray.CreateBuilder<BoundPattern>(parameterCount);
        var elementCount = Math.Min(designations.Count, parameterCount);

        for (var i = 0; i < elementCount; i++)
        {
            var variable = designations[i];
            var expectedType = EnsureTypeAccessible(parameters[i + parameterOffset].Type, variable.GetLocation());
            var boundElement = BindVariableDesignationForAssignment(variable, expectedType, isMutable);
            boundElements.Add(boundElement);
        }

        for (var i = elementCount; i < parameterCount; i++)
        {
            var parameterType = EnsureTypeAccessible(parameters[i + parameterOffset].Type, fallbackLocation);
            boundElements.Add(new BoundDiscardPattern(parameterType, BoundExpressionReason.TypeMismatch));
        }

        for (var i = elementCount; i < designations.Count; i++)
            _ = BindVariableDesignationForAssignment(designations[i], Compilation.ErrorTypeSymbol, isMutable);

        return new BoundDeconstructPattern(
            inputType: valueType,
            receiverType: GetDeconstructReceiverType(deconstructMethod),
            narrowedType: null,
            deconstructMethod: deconstructMethod,
            arguments: boundElements.ToImmutable());
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

    private BoundExpression BindExpressionAllowingEvent(ExpressionSyntax syntax)
        => syntax switch
        {
            IdentifierNameSyntax identifier => BindIdentifierName(identifier, allowEventAccess: true),
            MemberAccessExpressionSyntax memberAccess => BindMemberAccessExpression(memberAccess, allowEventAccess: true),
            MemberBindingExpressionSyntax memberBinding => BindMemberBindingExpression(memberBinding, allowEventAccess: true),
            _ => BindExpression(syntax)
        };

    private bool IsWithinEventContainingType(IEventSymbol eventSymbol)
    {
        var containingType = _containingSymbol switch
        {
            IMethodSymbol methodSymbol => methodSymbol.ContainingType,
            INamedTypeSymbol typeSymbol => typeSymbol,
            _ => null
        };

        return containingType is not null &&
            SymbolEqualityComparer.Default.Equals(containingType, eventSymbol.ContainingType);
    }

    private static bool TryGetEventBackingField(IEventSymbol eventSymbol, out SourceFieldSymbol? backingField)
    {
        backingField = eventSymbol is SourceEventSymbol sourceEvent ? sourceEvent.BackingField : null;
        return backingField is not null;
    }

    private BoundExpression BindEventInvocationReceiver(
        IEventSymbol eventSymbol,
        BoundMemberAccessExpression eventAccess,
        SyntaxNode syntax)
    {
        if (!IsWithinEventContainingType(eventSymbol))
        {
            _diagnostics.ReportEventCanOnlyBeUsedWithPlusOrMinus(eventSymbol.Name, syntax.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.NotFound);
        }

        if (!TryGetEventBackingField(eventSymbol, out var backingField))
        {
            _diagnostics.ReportEventCannotBeInvoked(eventSymbol.Name, syntax.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.NotFound);
        }

        return new BoundFieldAccess(eventAccess.Receiver, backingField);
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

    private BoundExpression BindLambdaToDelegateIfNeeded(BoundExpression expression, ITypeSymbol targetType)
    {
        if (expression is not BoundLambdaExpression lambda)
            return expression;

        if (targetType is not INamedTypeSymbol delegateType || delegateType.TypeKind != TypeKind.Delegate)
            return expression;

        return ReplayLambda(lambda, delegateType) ?? expression;
    }

    private ITypeSymbol GetIndexType() =>
        Compilation.GetTypeByMetadataName("System.Index") ?? Compilation.ErrorTypeSymbol;

    private ITypeSymbol GetRangeType() =>
        Compilation.GetTypeByMetadataName("System.Range") ?? Compilation.ErrorTypeSymbol;

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

        return new BoundConversionExpression(expression, targetType, conversion);
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
            var syntaxNode = boundArgument.Syntax switch
            {
                ArgumentSyntax argumentSyntax => argumentSyntax.Expression,
                SyntaxNode node => node,
                _ => null
            };

            // --- BEGIN NEW BLOCK ---
            if (parameter.RefKind is RefKind.Ref or RefKind.Out or RefKind.In)
            {
                converted[i] = expression;
                continue;
            }

            // Shape lambdas to a concrete delegate type when possible.
            // This mirrors local-initializer behavior and enables passing lambdas to `System.Delegate` parameters.
            if (expression is BoundLambdaExpression lambda)
            {
                // First: if the parameter itself is a concrete delegate type, replay the lambda against it.
                expression = BindLambdaToDelegateIfNeeded(expression, parameter.Type);

                // Second: if the parameter is `System.Delegate` / `System.MulticastDelegate`,
                // keep the lambda's inferred delegate type and rely on implicit reference conversion.
                if (IsSystemDelegateLike(parameter.Type) && expression is BoundLambdaExpression stillLambda)
                {
                    var inferred = stillLambda.DelegateType as INamedTypeSymbol;
                    if (inferred is not null && inferred.TypeKind == TypeKind.Delegate)
                    {
                        var rebound = ReplayLambda(stillLambda, inferred);
                        if (rebound is not null)
                        {
                            expression = rebound;
                        }
                    }
                }
            }
            // --- END NEW BLOCK ---

            if (!ShouldAttemptConversion(expression) ||
                parameter.Type.TypeKind == TypeKind.Error ||
                expression.Type is null)
            {
                converted[i] = expression;
                continue;
            }

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
    private static bool IsSystemDelegateLike(ITypeSymbol type)
    {
        if (type is not INamedTypeSymbol named)
            return false;

        var def = named.OriginalDefinition ?? named;

        if (def.ContainingNamespace is null)
            return false;

        // Match System.Delegate / System.MulticastDelegate
        if (!string.Equals(def.ContainingNamespace.ToDisplayString(), "System", StringComparison.Ordinal))
            return false;

        return string.Equals(def.MetadataName, "Delegate", StringComparison.Ordinal) ||
               string.Equals(def.MetadataName, "MulticastDelegate", StringComparison.Ordinal);
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

        if (parameter is PEParameterSymbol { ExplicitDefaultValueIsTypeDefault: true }
            && parameterType.IsValueType
            && parameterType.SpecialType == SpecialType.None
            && parameterType.TypeKind != TypeKind.Enum)
        {
            return new BoundDefaultValueExpression(parameterType);
        }

        if (!TryCreateOptionalLiteral(parameterType, value, out var literal))
        {
            if (parameter is PEParameterSymbol { ExplicitDefaultValueIsTypeDefault: true })
                return new BoundDefaultValueExpression(parameterType);

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

    public readonly struct TargetTypeScope : IDisposable
    {
        private readonly BlockBinder _binder;

        public TargetTypeScope(BlockBinder binder)
        {
            _binder = binder;
        }

        public void Dispose()
        {
            _binder._targetTypeStack.Pop();
        }
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
                    boundElement = targetType is IArrayTypeSymbol arrayTarget
                        ? BindExpressionWithTargetType(exprElem.Expression, arrayTarget.ElementType)
                        : BindExpression(exprElem.Expression);
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

        // Bind to IEnumerable<T> targets by producing a T[] and relying on the implicit conversion
        // from array to IEnumerable<T> (and other compatible interfaces).
        if (targetType is INamedTypeSymbol enumerableTarget &&
            TryGetIEnumerableElementType(enumerableTarget, out var enumerableElementType))
        {
            var arrayElementType = enumerableElementType;
            var arrayType2 = Compilation.CreateArrayTypeSymbol(arrayElementType);

            var converted = ImmutableArray.CreateBuilder<BoundExpression>(elements.Count);

            for (var i = 0; i < elements.Count; i++)
            {
                var element = elements[i];
                var elementSyntax = elementNodes[i];

                if (element is BoundSpreadElement spread)
                {
                    var sourceType = GetSpreadElementType(spread.Expression.Type!);
                    if (!IsAssignable(arrayElementType, sourceType, out _))
                    {
                        _diagnostics.ReportCannotConvertFromTypeToType(
                            sourceType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                            arrayElementType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                            syntax.GetLocation());
                    }

                    converted.Add(element);
                    continue;
                }

                if (arrayElementType.TypeKind != TypeKind.Error &&
                    ShouldAttemptConversion(element))
                {
                    if (!IsAssignable(arrayElementType, element.Type!, out var conversion2))
                    {
                        _diagnostics.ReportCannotConvertFromTypeToType(
                            element.Type!.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                            arrayElementType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                            syntax.GetLocation());
                    }
                    else
                    {
                        converted.Add(ApplyConversion(element, arrayElementType, conversion2, elementSyntax));
                        continue;
                    }
                }

                converted.Add(element);
            }

            BoundExpression arrayExpr = new BoundCollectionExpression(arrayType2, converted.ToImmutable());

            var conversion = Compilation.ClassifyConversion(arrayType2, enumerableTarget);
            if (conversion.Exists && conversion.IsImplicit)
                return ApplyConversion(arrayExpr, enumerableTarget, conversion, syntax);

            return arrayExpr;
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

    private bool TryGetIEnumerableElementType(INamedTypeSymbol targetType, out ITypeSymbol elementType)
    {
        elementType = Compilation.ErrorTypeSymbol;

        // Direct match: IEnumerable<T>
        if (targetType.SpecialType == SpecialType.System_Collections_Generic_IEnumerable_T &&
            targetType.TypeArguments.Length == 1)
        {
            elementType = targetType.TypeArguments[0];
            return true;
        }

        // Check original definition by metadata name (covers symbols that don't set SpecialType)
        var def = targetType.OriginalDefinition ?? targetType;
        if (def.MetadataName == "IEnumerable`1" &&
            def.ContainingNamespace?.ToDisplayString() == "System.Collections.Generic" &&
            targetType.TypeArguments.Length == 1)
        {
            elementType = targetType.TypeArguments[0];
            return true;
        }

        // Interface implementation: look for IEnumerable<T> among all interfaces
        foreach (var iface in targetType.AllInterfaces)
        {
            if (iface.SpecialType == SpecialType.System_Collections_Generic_IEnumerable_T &&
                iface.TypeArguments.Length == 1)
            {
                elementType = iface.TypeArguments[0];
                return true;
            }

            var idef = iface.OriginalDefinition ?? iface;
            if (idef.MetadataName == "IEnumerable`1" &&
                idef.ContainingNamespace?.ToDisplayString() == "System.Collections.Generic" &&
                iface.TypeArguments.Length == 1)
            {
                elementType = iface.TypeArguments[0];
                return true;
            }
        }

        return false;
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
            var enumerable = Compilation.GetTypeByMetadataName("System.Collections.IEnumerable") as INamedTypeSymbol;
            var genericEnumerable = Compilation.GetTypeByMetadataName("System.Collections.Generic.IEnumerable`1") as INamedTypeSymbol;

            bool MatchesEnumerable(INamedTypeSymbol candidate)
            {
                if (candidate.SpecialType == SpecialType.System_Collections_IEnumerable ||
                    candidate.SpecialType == SpecialType.System_Collections_Generic_IEnumerable_T)
                {
                    return true;
                }

                var definition = candidate.OriginalDefinition ?? candidate;
                if (enumerable is not null && SymbolEqualityComparer.Default.Equals(definition, enumerable))
                    return true;
                if (genericEnumerable is not null && SymbolEqualityComparer.Default.Equals(definition, genericEnumerable))
                    return true;

                if (definition.MetadataName == "IEnumerable" &&
                    IsInNamespace(definition.ContainingNamespace, "System.Collections"))
                {
                    return true;
                }

                if (definition.MetadataName == "IEnumerable`1" &&
                    IsInNamespace(definition.ContainingNamespace, "System.Collections.Generic"))
                {
                    return true;
                }

                return false;
            }

            static bool IsInNamespace(INamespaceSymbol? namespaceSymbol, string qualifiedNamespace)
            {
                if (namespaceSymbol is null)
                    return false;

                var remaining = qualifiedNamespace;

                while (!namespaceSymbol.IsGlobalNamespace)
                {
                    var lastDot = remaining.LastIndexOf('.');
                    var segment = lastDot >= 0 ? remaining[(lastDot + 1)..] : remaining;

                    if (!string.Equals(namespaceSymbol.Name, segment, StringComparison.Ordinal))
                        return false;

                    if (lastDot < 0)
                        return namespaceSymbol.ContainingNamespace.IsGlobalNamespace;

                    remaining = remaining[..lastDot];
                    namespaceSymbol = namespaceSymbol.ContainingNamespace;

                    if (namespaceSymbol is null)
                        return false;
                }

                return false;
            }

            if (MatchesEnumerable(named))
                return true;

            foreach (var iface in named.AllInterfaces)
            {
                if (MatchesEnumerable(iface))
                    return true;
            }
        }

        return false;
    }

    private bool IsNonClosureFunctionBody
    {
        get
        {
            if (_containingSymbol is not IMethodSymbol method)
                return false;

            // Lambdas have their own closure semantics; this rule is for function statements.
            if (method.DeclaringSyntaxReferences.IsDefaultOrEmpty)
                return false;

            return method.DeclaringSyntaxReferences
                .Select(r => r.GetSyntax())
                .OfType<FunctionStatementSyntax>()
                .Any();
        }
    }

    public override IEnumerable<ISymbol> LookupSymbols(string name)
    {
        var seen = new HashSet<ISymbol>();
        Binder? current = this;

        // If we're inside a function statement body (non-closure), we must NOT see locals/params
        // from enclosing blocks/top-level/methods. We still want types/imports/members.
        var restrict = IsNonClosureFunctionBody;
        var boundarySymbol = restrict ? _containingSymbol : null;
        var pastBoundary = false;

        while (current is not null)
        {
            if (restrict && !pastBoundary)
            {
                // Once we encounter a binder that belongs to a different containing symbol,
                // we've left the function body scope. After that we must not see locals/params.
                if (current is BlockBinder bb &&
                    !SymbolEqualityComparer.Default.Equals(bb.ContainingSymbol, boundarySymbol))
                {
                    pastBoundary = true;
                }
                else if (current is MethodBinder mb &&
                         !SymbolEqualityComparer.Default.Equals(mb.GetMethodSymbol(), boundarySymbol))
                {
                    pastBoundary = true;
                }
                else if (current is TopLevelBinder or LambdaBinder)
                {
                    pastBoundary = true;
                }
            }

            var allowLocalsAndParams = !pastBoundary;

            if (current is BlockBinder block)
            {
                if (allowLocalsAndParams)
                {
                    if (block._locals.TryGetValue(name, out var local) && seen.Add(local.Symbol))
                        yield return local.Symbol;

                    if (block._functions.TryGetValue(name, out var func) && seen.Add(func))
                        yield return func;

                    if (block._labelsByName.TryGetValue(name, out var label) && seen.Add(label))
                        yield return label;
                }
            }

            if (allowLocalsAndParams && current is TopLevelBinder topLevelBinder)
            {
                foreach (var param in topLevelBinder.GetParameters())
                    if (param.Name == name && seen.Add(param))
                        yield return param;
            }

            if (allowLocalsAndParams && current is MethodBinder methodBinder)
            {
                foreach (var param in methodBinder.GetMethodSymbol().Parameters)
                    if (param.Name == name && seen.Add(param))
                        yield return param;
            }

            if (allowLocalsAndParams && current is LambdaBinder lambdaBinder)
            {
                foreach (var param in lambdaBinder.GetParameters())
                    if (param.Name == name && seen.Add(param))
                        yield return param;
            }

            // Members/imports remain visible even after the boundary.
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

                converted = expressionBinder.ValidateByRefReturnExpression(symbol, converted, function.ExpressionBody.Expression);
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

            SemanticModel.CacheBoundNode(function.ExpressionBody, boundBlock, this);
        }

        return new BoundFunctionStatement(symbol); // Possibly include body here if needed
    }

    private BoundExpression BindWithExpression(WithExpressionSyntax syntax)
    {
        var receiver = BindExpression(syntax.Expression);
        var receiverType = UnwrapAlias(receiver.Type ?? Compilation.ErrorTypeSymbol);
        var assignments = BindWithAssignments(receiverType, syntax.Assignments);

        BoundExpression ReturnWithError(BoundExpressionReason reason)
        {
            return new BoundWithExpression(
                receiver,
                 [.. assignments.Select(x => x.BoundNode)],
                strategy: BoundWithStrategyKind.UpdateMethod,
                method: null,
                memberMethods: ImmutableArray<IMethodSymbol>.Empty,
                parameterMembers: ImmutableArray<ISymbol>.Empty,
                cloneMethod: null,
                copyConstructor: null,
                type: Compilation.ErrorTypeSymbol,
                reason: reason);
        }

        if (receiverType.TypeKind == TypeKind.Error)
            return ReturnWithError(BoundExpressionReason.OtherError);

        if (receiverType is not INamedTypeSymbol namedReceiver)
        {
            _diagnostics.ReportTypeDoesNotSupportWithExpression(
                receiverType.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat),
                syntax.WithKeyword.GetLocation());
            return ReturnWithError(BoundExpressionReason.UnsupportedOperation);
        }

        if (IsRecordType(receiverType) && TryGetCopyConstructor(namedReceiver, out var recordCopyConstructor))
        {
            return new BoundWithExpression(
                receiver,
                 [.. assignments.Select(x => x.BoundNode)],
                BoundWithStrategyKind.RecordClone,
                method: null,
                memberMethods: ImmutableArray<IMethodSymbol>.Empty,
                parameterMembers: ImmutableArray<ISymbol>.Empty,
                cloneMethod: null,
                copyConstructor: recordCopyConstructor,
                type: receiverType);
        }

        // Optimization: if only a single member is updated and a matching WithX(...) member method exists,
        // prefer that over the broader Update/With convention methods.
        if (assignments.Length == 1 &&
            TryBindWithMemberMethods(receiver, namedReceiver, assignments, syntax, out var singleMemberMethods, out var singleMemberMethodsReturnType))
        {
            return new BoundWithExpression(
                receiver,
                 [.. assignments.Select(x => x.BoundNode)],
                BoundWithStrategyKind.WithMemberMethods,
                method: null,
                memberMethods: singleMemberMethods,
                parameterMembers: ImmutableArray<ISymbol>.Empty,
                cloneMethod: null,
                copyConstructor: null,
                type: singleMemberMethodsReturnType);
        }

        if (TryBindWithConventionMethod(receiver, namedReceiver, assignments, syntax, "Update", BoundWithStrategyKind.UpdateMethod, out var updateBound))
            return updateBound;

        if (TryBindWithConventionMethod(receiver, namedReceiver, assignments, syntax, "With", BoundWithStrategyKind.WithMethod, out var withBound))
            return withBound;

        if (TryBindWithMemberMethods(receiver, namedReceiver, assignments, syntax, out var memberMethods, out var memberMethodsReturnType))
        {
            return new BoundWithExpression(
                receiver,
                 [.. assignments.Select(x => x.BoundNode)],
                BoundWithStrategyKind.WithMemberMethods,
                method: null,
                memberMethods: memberMethods,
                parameterMembers: ImmutableArray<ISymbol>.Empty,
                cloneMethod: null,
                copyConstructor: null,
                type: memberMethodsReturnType);
        }

        if (TryGetCloneMethod(namedReceiver, syntax.WithKeyword.GetLocation(), out var cloneMethod))
        {
            return new BoundWithExpression(
                receiver,
                 [.. assignments.Select(x => x.BoundNode)],
                BoundWithStrategyKind.Clone,
                method: null,
                memberMethods: ImmutableArray<IMethodSymbol>.Empty,
                parameterMembers: ImmutableArray<ISymbol>.Empty,
                cloneMethod: cloneMethod,
                copyConstructor: null,
                type: cloneMethod.ReturnType);
        }

        if (TryGetCopyConstructor(namedReceiver, out var copyConstructor))
        {
            return new BoundWithExpression(
                receiver,
                 [.. assignments.Select(x => x.BoundNode)],
                BoundWithStrategyKind.Clone,
                method: null,
                memberMethods: ImmutableArray<IMethodSymbol>.Empty,
                parameterMembers: ImmutableArray<ISymbol>.Empty,
                cloneMethod: null,
                copyConstructor: copyConstructor,
                type: receiverType);
        }

        _diagnostics.ReportTypeDoesNotSupportWithExpression(
            receiverType.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat),
            syntax.WithKeyword.GetLocation());
        return ReturnWithError(BoundExpressionReason.UnsupportedOperation);
    }

    private ImmutableArray<(BoundWithAssignment BoundNode, WithAssignmentSyntax SyntaxNode)> BindWithAssignments(
        ITypeSymbol receiverType,
        SyntaxList<WithAssignmentSyntax> assignments)
    {
        if (assignments.Count == 0)
            return ImmutableArray<(BoundWithAssignment BoundNode, WithAssignmentSyntax SyntaxNode)>.Empty;

        var builder = ImmutableArray.CreateBuilder<(BoundWithAssignment BoundNode, WithAssignmentSyntax SyntaxNode)>(assignments.Count);
        var seenMembers = new HashSet<string>(StringComparer.Ordinal);

        _withInitializerDepth++;
        try
        {
            foreach (var assignment in assignments)
            {
                var nameSyntax = assignment.Name;
                var nameToken = nameSyntax.Identifier;
                var memberName = nameToken.ValueText;

                if (string.IsNullOrEmpty(memberName))
                {
                    _ = BindExpression(assignment.Expression, allowReturn: false);
                    continue;
                }

                if (!seenMembers.Add(memberName))
                {
                    _diagnostics.ReportWithExpressionMemberAssignedMultipleTimes(memberName, nameSyntax.GetLocation());
                    _ = BindExpression(assignment.Expression, allowReturn: false);
                    continue;
                }

                if (receiverType is not INamedTypeSymbol namedReceiver)
                {
                    _ = BindExpression(assignment.Expression, allowReturn: false);
                    continue;
                }

                if (!TryGetAssignableMember(namedReceiver, assignment, out var member, out var memberType))
                    continue;

                var value = BindExpressionWithTargetType(assignment.Expression, memberType, allowReturn: false);
                value = PrepareRightForAssignment(value, memberType, assignment.Expression);
                if (value is BoundErrorExpression)
                    continue;

                builder.Add((new BoundWithAssignment(member, value), assignment));
            }
        }
        finally
        {
            _withInitializerDepth--;
        }

        return builder.ToImmutable();
    }

    private bool TryBindWithConventionMethod(
        BoundExpression receiver,
        INamedTypeSymbol receiverType,
        ImmutableArray<(BoundWithAssignment BoundNode, WithAssignmentSyntax Syntax)> assignments,
        WithExpressionSyntax syntax,
        string methodName,
        BoundWithStrategyKind strategy,
        out BoundWithExpression boundExpression)
    {
        boundExpression = null!;

        var candidates = new SymbolQuery(methodName, receiverType, IsStatic: false)
            .LookupMethods(this)
            .ToImmutableArray();

        if (candidates.IsDefaultOrEmpty)
            return false;

        var accessible = GetAccessibleMethods(candidates, syntax.WithKeyword.GetLocation(), reportIfInaccessible: false);
        if (accessible.IsDefaultOrEmpty)
            return false;

        if (string.Equals(methodName, "With", StringComparison.Ordinal))
        {
            accessible = accessible
                .Where(static method => !method.DeclaringSyntaxReferences.IsDefaultOrEmpty)
                .ToImmutableArray();

            if (accessible.IsDefaultOrEmpty)
                return false;
        }

        // Map assignments by *parameter-style* name (camelCase), because convention methods typically
        // use parameter names like `middleName` while members are `MiddleName`.
        // This prevents false negatives when comparing assignment keys to method parameter names.
        var assignmentMap = new Dictionary<string, BoundWithAssignment>(StringComparer.Ordinal);
        foreach (var (a, snode) in assignments)
        {
            var key = NormalizeWithConventionKey(a.Member.Name);
            assignmentMap[key] = a;
        }
        var applicable = new List<(IMethodSymbol Method, ImmutableArray<ISymbol> ParameterMembers)>();

        foreach (var method in accessible)
        {
            if (!IsReturnTypeCovariant(receiverType, method.ReturnType))
                continue;

            var parameterMembers = ImmutableArray.CreateBuilder<ISymbol>(method.Parameters.Length);
            var parameterNames = new HashSet<string>(StringComparer.Ordinal);
            var valid = true;

            foreach (var parameter in method.Parameters)
            {
                var name = NormalizeWithConventionMemberName(parameter.Name);
                if (!TryGetReadableMember(receiverType, name, syntax.WithKeyword.GetLocation(), out var member))
                {
                    valid = false;
                    break;
                }

                parameterMembers.Add(member);
                parameterNames.Add(parameter.Name);

                var argumentType = assignmentMap.TryGetValue(parameter.Name, out var assignment)
                    ? assignment.Value.Type ?? Compilation.ErrorTypeSymbol
                    : GetMemberType(member);

                if (!IsAssignable(parameter.Type, argumentType, out _))
                {
                    valid = false;
                    break;
                }
            }

            if (!valid)
                continue;

            // Reject assignments that don't correspond to any parameter name.
            if (assignmentMap.Keys.Any(name => !parameterNames.Contains(name)))
                continue;

            applicable.Add((method, parameterMembers.ToImmutable()));
        }

        if (applicable.Count == 0)
            return false;

        if (applicable.Count > 1)
        {
            _diagnostics.ReportCallIsAmbiguous(methodName, applicable.Select(x => x.Method).ToImmutableArray(), syntax.WithKeyword.GetLocation());
            boundExpression = new BoundWithExpression(
                receiver,
                [.. assignments.Select(x => x.BoundNode)],
                strategy,
                method: null,
                memberMethods: ImmutableArray<IMethodSymbol>.Empty,
                parameterMembers: ImmutableArray<ISymbol>.Empty,
                cloneMethod: null,
                copyConstructor: null,
                type: receiverType,
                reason: BoundExpressionReason.Ambiguous);
            return true;
        }

        var selected = applicable[0];
        boundExpression = new BoundWithExpression(
            receiver,
            [.. assignments.Select(x => x.BoundNode)],
            strategy,
            selected.Method,
            memberMethods: ImmutableArray<IMethodSymbol>.Empty,
            parameterMembers: selected.ParameterMembers,
            cloneMethod: null,
            copyConstructor: null,
            type: selected.Method.ReturnType);

        return true;
    }

    private static string NormalizeWithConventionKey(string memberName)
    {
        if (string.IsNullOrEmpty(memberName))
            return memberName;

        // `MiddleName` -> `middleName`
        return char.ToLowerInvariant(memberName[0]) + memberName[1..];
    }

    private static string NormalizeWithConventionMemberName(string parameterName)
    {
        if (string.IsNullOrEmpty(parameterName))
            return parameterName;

        // `middleName` -> `MiddleName`
        return char.ToUpperInvariant(parameterName[0]) + parameterName[1..];
    }

    private bool TryBindWithMemberMethods(
    BoundExpression receiver,
    INamedTypeSymbol namedReceiver,
    ImmutableArray<(BoundWithAssignment BoundNode, WithAssignmentSyntax Syntax)> assignments,
    WithExpressionSyntax syntax,
    out ImmutableArray<IMethodSymbol> memberMethods,
    out ITypeSymbol memberMethodsReturnType)
    {
        return TryBindWithMemberMethods(
            receiver,
            namedReceiver,
            assignments,
            syntax,
            out memberMethods,
            out memberMethodsReturnType,
            reportDiagnostics: true);
    }

    private bool TryBindWithMemberMethods(
        BoundExpression receiver,
        INamedTypeSymbol receiverType,
        ImmutableArray<(BoundWithAssignment BoundNode, WithAssignmentSyntax Syntax)> assignments,
        WithExpressionSyntax syntax,
        out ImmutableArray<IMethodSymbol> memberMethods,
        out ITypeSymbol returnType,
        bool reportDiagnostics)
    {
        memberMethods = ImmutableArray<IMethodSymbol>.Empty;
        returnType = receiverType;

        if (assignments.IsDefaultOrEmpty)
            return false;

        var methods = ImmutableArray.CreateBuilder<IMethodSymbol>(assignments.Length);
        ITypeSymbol lastReturnType = receiverType;

        int i = 0;
        foreach (var (assignment, syntaxNode) in assignments)
        {
            var methodName = $"With{assignment.Member.Name}";
            var candidates = new SymbolQuery(methodName, receiverType, IsStatic: false)
                .LookupMethods(this)
                .ToImmutableArray();

            if (candidates.IsDefaultOrEmpty)
            {
                if (reportDiagnostics)
                    _diagnostics.ReportTheNameDoesNotExistInTheCurrentContext(methodName, syntaxNode.Name.GetLocation());
                return false;
            }

            var accessible = GetAccessibleMethods(candidates, syntax.WithKeyword.GetLocation(), reportIfInaccessible: false);
            if (accessible.IsDefaultOrEmpty)
                return false;

            var applicable = accessible
                .Where(m => m.Parameters.Length == 1 && IsReturnTypeCovariant(receiverType, m.ReturnType))
                .ToImmutableArray();

            if (applicable.IsDefaultOrEmpty)
            {
                _diagnostics.ReportTheNameDoesNotExistInTheCurrentContext(
                    methodName,
                    syntaxNode.Name.GetLocation());
                return false;
            }

            var args = new[] { new BoundArgument(assignment.Value, RefKind.None, name: null, syntax) };
            var resolution = OverloadResolver.ResolveOverload(applicable, args, Compilation, receiver: receiver, canBindLambda: EnsureLambdaCompatible, callSyntax: syntax);

            if (resolution.IsAmbiguous)
            {
                _diagnostics.ReportCallIsAmbiguous(methodName, resolution.AmbiguousCandidates, syntax.WithKeyword.GetLocation());
                return false;
            }

            if (!resolution.Success)
            {
                if (reportDiagnostics)
                {
                    // Best effort expected type:
                    var expectedType = candidates[0].Parameters[0].Type;

                    var valueType = assignment.Value.Type ?? Compilation.ErrorTypeSymbol;
                    if (valueType.TypeKind != TypeKind.Error && expectedType.TypeKind != TypeKind.Error)
                    {
                        _diagnostics.ReportCannotConvertFromTypeToType(
                            valueType.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat),
                            expectedType.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat),
                            syntax.Assignments[i++].Expression.GetLocation());
                    }
                }
                return false;
            }

            methods.Add(resolution.Method!);
            lastReturnType = resolution.Method!.ReturnType;
        }

        memberMethods = methods.ToImmutable();
        returnType = lastReturnType;
        return true;
    }

    private bool TryGetAssignableMember(
        INamedTypeSymbol receiverType,
        WithAssignmentSyntax assignment,
        out ISymbol member,
        out ITypeSymbol memberType)
    {
        member = null!;
        memberType = Compilation.ErrorTypeSymbol;

        var memberName = assignment.Name.Identifier.ValueText;
        var members = receiverType.GetMembers(memberName);

        var property = members.OfType<IPropertySymbol>().FirstOrDefault();
        if (property is not null)
        {
            if (!EnsureMemberAccessible(property, assignment.Name.GetLocation(), "property"))
                return false;

            // In a `with` initializer, assignments are *conceptual* updates that may be applied via
            // Update/With conventions or cloning strategies. A property can therefore be assignable
            // even if it has no setter.
            var inWithInitializer = _withInitializerDepth > 0;

            // Still require the member to be readable so we can treat it as a stable assignment target
            // (and so convention methods can map parameter names back to members).
            if (property.GetMethod is null)
            {
                _diagnostics.ReportPropertyOrIndexerCannotBeAssignedIsReadOnly(property.Name, assignment.Name.GetLocation());
                _ = BindExpression(assignment.Expression, allowReturn: false);
                return false;
            }

            if (!inWithInitializer)
            {
                // Outside of `with`, require a real setter.
                if (property.SetMethod is null)
                {
                    _diagnostics.ReportPropertyOrIndexerCannotBeAssignedIsReadOnly(property.Name, assignment.Name.GetLocation());
                    _ = BindExpression(assignment.Expression, allowReturn: false);
                    return false;
                }
            }

            member = property;
            memberType = property.Type;
            return true;
        }

        var field = members.OfType<IFieldSymbol>().FirstOrDefault();
        if (field is not null)
        {
            if (!EnsureMemberAccessible(field, assignment.Name.GetLocation(), "field"))
                return false;

            var inWithInitializer = _withInitializerDepth > 0;

            if (field.IsConst || (!inWithInitializer && !field.IsMutable))
            {
                _diagnostics.ReportReadOnlyFieldCannotBeAssignedTo(assignment.Name.GetLocation());
                _ = BindExpression(assignment.Expression, allowReturn: false);
                return false;
            }

            member = field;
            memberType = field.Type;
            return true;
        }

        _diagnostics.ReportMemberDoesNotContainDefinition(
            receiverType.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat),
            memberName,
            assignment.Name.GetLocation());
        _ = BindExpression(assignment.Expression, allowReturn: false);
        return false;
    }

    private bool TryGetReadableMember(
        INamedTypeSymbol receiverType,
        string memberName,
        Location location,
        out ISymbol member)
    {
        member = null!;
        var members = receiverType.GetMembers(memberName);

        foreach (var candidate in members.OfType<IPropertySymbol>())
        {
            if (candidate.IsStatic || candidate.GetMethod is null)
                continue;

            if (!IsSymbolAccessible(candidate))
                continue;

            member = candidate;
            return true;
        }

        foreach (var candidate in members.OfType<IFieldSymbol>())
        {
            if (candidate.IsStatic)
                continue;

            if (!IsSymbolAccessible(candidate))
                continue;

            member = candidate;
            return true;
        }

        if (members.Length > 0)
            _ = EnsureMemberAccessible(members[0], location, "member");

        return false;
    }

    private bool TryGetCloneMethod(
        INamedTypeSymbol receiverType,
        Location location,
        out IMethodSymbol cloneMethod)
    {
        cloneMethod = null!;
        var candidates = new SymbolQuery("Clone", receiverType, IsStatic: false)
            .LookupMethods(this)
            .ToImmutableArray();

        if (candidates.IsDefaultOrEmpty)
            return false;

        var accessible = GetAccessibleMethods(candidates, location, reportIfInaccessible: false);
        if (accessible.IsDefaultOrEmpty)
            return false;

        var applicable = accessible
            .Where(m => m.Parameters.Length == 0 && IsReturnTypeCovariant(receiverType, m.ReturnType))
            .ToImmutableArray();

        if (applicable.IsDefaultOrEmpty)
            return false;

        if (applicable.Length > 1)
        {
            _diagnostics.ReportCallIsAmbiguous("Clone", applicable, location);
            return false;
        }

        cloneMethod = applicable[0];
        return true;
    }

    private bool TryGetCopyConstructor(INamedTypeSymbol receiverType, out IMethodSymbol copyConstructor)
    {
        copyConstructor = null!;

        foreach (var ctor in receiverType.InstanceConstructors)
        {
            if (ctor.IsStatic || ctor.Parameters.Length != 1)
                continue;

            if (!SymbolEqualityComparer.Default.Equals(ctor.Parameters[0].Type, receiverType))
                continue;

            if (!IsSymbolAccessible(ctor))
                continue;

            copyConstructor = ctor;
            return true;
        }

        return false;
    }

    private bool IsReturnTypeCovariant(ITypeSymbol receiverType, ITypeSymbol returnType)
        => IsAssignable(receiverType, returnType, out _);

    private static bool IsRecordType(ITypeSymbol typeSymbol)
        => typeSymbol is SourceNamedTypeSymbol { IsRecord: true } ||
           typeSymbol is ConstructedNamedTypeSymbol { OriginalDefinition: SourceNamedTypeSymbol { IsRecord: true } };

    private static ITypeSymbol GetMemberType(ISymbol member)
    {
        return member switch
        {
            IFieldSymbol field => field.Type,
            IPropertySymbol property => property.Type,
            _ => throw new InvalidOperationException($"Unsupported member type: {member.GetType()}")
        };
    }

    private BoundObjectInitializer BindObjectInitializer(
     ITypeSymbol instanceType,
     ObjectInitializerExpressionSyntax initializer)
    {
        _objectInitializerDepth++;
        try
        {
            // Bind initializer entries in source order and keep them as a bound node.
            // Lowering into a block happens later.

            instanceType = UnwrapAlias(instanceType);

            // SwiftUI-style convention:
            // If the instance type has a settable `Content` property, then exactly one content entry is allowed.
            // That entry is lowered as `Content = <expr>`.
            IPropertySymbol? contentProperty = null;
            if (instanceType is INamedTypeSymbol namedInstance && namedInstance.TypeKind != TypeKind.Error)
            {
                foreach (var member in namedInstance.GetMembers("Content"))
                {
                    if (member is not IPropertySymbol p)
                        continue;

                    if (p.IsStatic)
                        continue;

                    if (p.SetMethod is null)
                        continue;

                    // Use the existing accessibility helper.
                    // NOTE: We don't have a great location yet; we will re-check with the actual entry location when used.
                    contentProperty = p;
                    break;
                }
            }

            var entries = ImmutableArray.CreateBuilder<BoundObjectInitializerEntry>(initializer.Entries.Count);

            var hasContentConvention = contentProperty is not null && contentProperty.Type.TypeKind != TypeKind.Error;
            var seenContentEntry = false;

            foreach (var entry in initializer.Entries)
            {
                switch (entry)
                {
                    case ObjectInitializerAssignmentEntrySyntax assignment:
                        {
                            var boundEntry = BindObjectInitializerAssignmentEntry(instanceType, assignment);
                            if (boundEntry is not null)
                                entries.Add(boundEntry);
                            break;
                        }

                    case ObjectInitializerExpressionEntrySyntax exprEntry:
                        {
                            if (!hasContentConvention)
                            {
                                // No Content property: keep the existing behavior.
                                var expr = BindExpression(exprEntry.Expression, allowReturn: false);
                                entries.Add(new BoundObjectInitializerExpressionEntry(expr));
                                break;
                            }

                            // With Content property: only one content entry is allowed.
                            if (seenContentEntry)
                            {
                                // Still bind the expression so it gets typed and any nested diagnostics flow.
                                var extra = BindExpressionWithTargetType(exprEntry.Expression, contentProperty!.Type, allowReturn: false);

                                if (extra.Type is { } extraType)
                                {
                                    _diagnostics.ReportMultipleContentEntriesNotAllowed(
                                        instanceType.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat),
                                        exprEntry.Expression.GetLocation());
                                }

                                break;
                            }

                            // First content entry.
                            seenContentEntry = true;

                            // Ensure the Content property is accessible at the point of use.
                            if (!EnsureMemberAccessible(contentProperty!, exprEntry.GetLocation(), "property"))
                            {
                                // Still bind expression for diagnostics.
                                _ = BindExpressionWithTargetType(exprEntry.Expression, contentProperty!.Type, allowReturn: false);
                                break;
                            }

                            var value = BindExpressionWithTargetType(exprEntry.Expression, contentProperty!.Type, allowReturn: false);
                            value = PrepareRightForAssignment(value, contentProperty!.Type, exprEntry.Expression);

                            // If conversion fails, diagnostics are already reported by PrepareRightForAssignment.
                            if (value is BoundErrorExpression)
                                break;

                            // Rewrite as `Content = <value>`.
                            entries.Add(new BoundObjectInitializerAssignmentEntry(contentProperty!, value));
                            break;
                        }
                }
            }

            return new BoundObjectInitializer(entries.ToImmutable());
        }
        finally
        {
            _objectInitializerDepth--;
        }
    }

    private BoundObjectInitializerAssignmentEntry? BindObjectInitializerAssignmentEntry(
     ITypeSymbol receiverType,
     ObjectInitializerAssignmentEntrySyntax assignment)
    {
        receiverType = UnwrapAlias(receiverType);

        if (receiverType.TypeKind == TypeKind.Error)
        {
            _ = BindExpression(assignment.Expression, allowReturn: false);
            return null;
        }

        if (receiverType is not INamedTypeSymbol named)
        {
            _ = BindExpression(assignment.Expression, allowReturn: false);
            return null;
        }

        var name = assignment.Name.Identifier.ValueText;
        var members = named.GetMembers(name);

        IPropertySymbol? property = null;
        IFieldSymbol? field = null;

        foreach (var member in members)
        {
            if (member is IPropertySymbol p)
            {
                property = p;
                break;
            }
        }

        if (property is null)
        {
            foreach (var member in members)
            {
                if (member is IFieldSymbol f)
                {
                    field = f;
                    break;
                }
            }
        }

        if (property is null && field is null)
        {
            // Unknown member. RHS already bound for diagnostics.
            var memberName = assignment.Name.Identifier.ValueText;
            _diagnostics.ReportMemberDoesNotContainDefinition(
                receiverType.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat),
                memberName,
                assignment.Name.GetLocation());
            _ = BindExpression(assignment.Expression, allowReturn: false);
            return null;
        }

        if (property is not null)
        {
            if (!EnsureMemberAccessible(property, assignment.Name.GetLocation(), "property"))
                return null;

            var setMethod = property.SetMethod;

            if (setMethod is null)
            {
                // report no setter (existing behavior)
                return null;
            }
            else if (setMethod.MethodKind == MethodKind.InitOnly)
            {
                // OK here (object initializer context)
            }
            else
            {
                // OK (normal setter)
            }

            var value = BindExpressionWithTargetType(assignment.Expression, property.Type, allowReturn: false);
            value = PrepareRightForAssignment(value, property.Type, assignment.Expression);
            if (value is BoundErrorExpression)
                return null;

            return new BoundObjectInitializerAssignmentEntry(property, value);
        }

        if (field is not null)
        {
            if (!EnsureMemberAccessible(field, assignment.Name.GetLocation(), "field"))
                return null;

            if (field.IsConst || !field.IsMutable)
                return null;

            var value = BindExpressionWithTargetType(assignment.Expression, field.Type, allowReturn: false);
            value = PrepareRightForAssignment(value, field.Type, assignment.Expression);
            if (value is BoundErrorExpression)
                return null;

            return new BoundObjectInitializerAssignmentEntry(field, value);
        }

        return null;
    }

    private sealed class TypeSymbolReferenceComparer : IEqualityComparer<ITypeSymbol>
    {
        public static TypeSymbolReferenceComparer Instance { get; } = new();

        public bool Equals(ITypeSymbol? x, ITypeSymbol? y) => ReferenceEquals(x, y);

        public int GetHashCode(ITypeSymbol obj) => RuntimeHelpers.GetHashCode(obj);
    }

    private sealed class SymbolReferenceComparer<TSymbol> : IEqualityComparer<TSymbol>
        where TSymbol : class, ISymbol
    {
        public static SymbolReferenceComparer<TSymbol> Instance { get; } = new();

        public bool Equals(TSymbol? x, TSymbol? y) => ReferenceEquals(x, y);

        public int GetHashCode(TSymbol obj) => RuntimeHelpers.GetHashCode(obj);
    }
}
