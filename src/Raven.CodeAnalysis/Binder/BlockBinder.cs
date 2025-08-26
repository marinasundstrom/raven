using System.Collections.Immutable;
using System.Linq;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

partial class BlockBinder : Binder
{
    private readonly ISymbol _containingSymbol;
    protected readonly Dictionary<string, ILocalSymbol> _locals = new();

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
            LocalFunctionStatementSyntax localFunctionStatement => BindLocalFunction(localFunctionStatement).Method,
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

    public override ISymbol? LookupSymbol(string name)
    {
        if (_locals.TryGetValue(name, out var sym))
            return sym;

        if (_localFunctions.TryGetValue(name, out var func))
            return func;

        var parentSymbol = ParentBinder?.LookupSymbol(name);
        if (parentSymbol != null)
            return parentSymbol;

        return Compilation.GlobalNamespace.GetMembers(name).FirstOrDefault();
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
        //if (_locals.TryGetValue(variableDeclarator.Identifier.Text, out var existingSymbol))
        //    return existingSymbol;

        var name = variableDeclarator.Identifier.Text;

        var decl = variableDeclarator.Parent as VariableDeclarationSyntax;
        var isMutable = decl!.LetOrVarKeyword.IsKind(SyntaxKind.VarKeyword);

        ITypeSymbol type = Compilation.ErrorTypeSymbol;
        BoundExpression? boundInitializer = null;

        var initializer = variableDeclarator.Initializer;
        if (initializer is not null)
        {
            boundInitializer = BindExpression(initializer.Value);
        }

        if (variableDeclarator.TypeAnnotation is null)
        {
            if (boundInitializer is not null)
                type = boundInitializer.Type!;
        }
        else
        {
            type = ResolveType(variableDeclarator.TypeAnnotation.Type);

            if (boundInitializer is not null && type.TypeKind != TypeKind.Error && !IsAssignable(type, boundInitializer.Type!))
            {
                _diagnostics.ReportCannotConvertFromTypeToType(boundInitializer.Type!, type, initializer!.Value.GetLocation());
                boundInitializer = new BoundErrorExpression(type, null, BoundExpressionReason.TypeMismatch);
            }
        }

        var declarator = new BoundVariableDeclarator(CreateLocalSymbol(variableDeclarator, name, isMutable, type), boundInitializer!);

        return new BoundLocalDeclarationStatement([declarator]);
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

        _locals[name] = symbol;
        return symbol;
    }

    public override BoundStatement BindStatement(StatementSyntax statement)
    {
        if (TryGetCachedBoundNode(statement) is BoundStatement cached)
            return cached;

        BoundStatement boundNode = statement switch
        {
            LocalDeclarationStatementSyntax localDeclaration => BindLocalDeclaration(localDeclaration.Declaration.Declarators[0]),
            ExpressionStatementSyntax expressionStmt => new BoundExpressionStatement(BindExpression(expressionStmt.Expression)),
            LocalFunctionStatementSyntax localFunction => BindLocalFunction(localFunction),
            ReturnStatementSyntax returnStatement => BindReturnStatement(returnStatement),
            EmptyStatementSyntax emptyStatement => new BoundExpressionStatement(new BoundVoidExpression(Compilation.GetSpecialType(SpecialType.System_Void))),
            _ => throw new NotSupportedException($"Unsupported statement: {statement.Kind}")
        };

        CacheBoundNode(statement, boundNode);

        return boundNode;
    }

    private BoundStatement BindReturnStatement(ReturnStatementSyntax returnStatement)
    {
        var expr = returnStatement.Expression is not null
            ? BindExpression(returnStatement.Expression)
            : new BoundVoidExpression(Compilation.GetSpecialType(SpecialType.System_Void));

        return new BoundReturnStatement(expr);
    }

    public Dictionary<string, IMethodSymbol> _localFunctions = new();

    public virtual BoundBlockExpression BindBlock(BlockSyntax block)
    {
        if (TryGetCachedBoundNode(block) is BoundExpression cached)
            return (BoundBlockExpression)cached;

        // Step 1: Pre-declare all local functions
        foreach (var stmt in block.Statements)
        {
            if (stmt is LocalFunctionStatementSyntax localFunc)
            {
                var localFuncBinder = SemanticModel.GetBinder(localFunc, this);
                if (localFuncBinder is LocalFunctionBinder lfBinder)
                {
                    var symbol = lfBinder.GetMethodSymbol();
                    _localFunctions[symbol.Name] = symbol;
                }
            }
        }

        // Step 2: Bind and cache all statements
        var boundStatements = new List<BoundStatement>(block.Statements.Count);
        foreach (var stmt in block.Statements)
        {
            var bound = BindStatement(stmt);
            boundStatements.Add(bound);
        }

        // Step 3: Create and cache the block
        var blockExpr = new BoundBlockExpression(boundStatements.ToArray());
        CacheBoundNode(block, blockExpr);
        return blockExpr;
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
            ElementAccessExpressionSyntax elementAccess => BindElementAccessExpression(elementAccess),
            AssignmentExpressionSyntax assignment => BindAssignmentExpression(assignment),
            CollectionExpressionSyntax collection => BindCollectionExpression(collection),
            ParenthesizedExpressionSyntax parenthesizedExpression => BindParenthesizedExpression(parenthesizedExpression),
            TupleExpressionSyntax tupleExpression => BindTupleExpression(tupleExpression),
            IfExpressionSyntax ifExpression => BindIfExpression(ifExpression),
            WhileExpressionSyntax whileExpression => BindWhileExpression(whileExpression),
            ForExpressionSyntax forExpression => BindForExpression(forExpression),
            BlockSyntax block => BindBlock(block),
            IsPatternExpressionSyntax isPatternExpression => BindIsPatternExpression(isPatternExpression),
            LambdaExpressionSyntax lambdaExpression => BindLambdaExpression(lambdaExpression),
            InterpolatedStringExpressionSyntax interpolated => BindInterpolatedStringExpression(interpolated),
            UnaryExpressionSyntax unaryExpression => BindUnaryExpression(unaryExpression),
            SelfExpressionSyntax selfExpression => BindSelfExpression(selfExpression),
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

        if (GetTargetType(tupleExpression) is ITupleTypeSymbol target && target.TupleElements.Length == tupleExpression.Arguments.Count)
        {
            for (int i = 0; i < tupleExpression.Arguments.Count; i++)
            {
                var arg = tupleExpression.Arguments[i];
                var boundExpr = BindExpression(arg.Expression);
                elements.Add(boundExpr);

                var expected = target.TupleElements[i].Type;
                if (!IsAssignable(expected, boundExpr.Type!))
                {
                    _diagnostics.ReportCannotConvertFromTypeToType(boundExpr.Type!, expected, arg.GetLocation());
                }
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

        // 2. Create parameter symbols
        var parameterSymbols = new List<IParameterSymbol>();
        foreach (var p in parameterSyntaxes)
        {
            var typeSyntax = p.TypeAnnotation.Type;
            var refKind = RefKind.None;
            if (typeSyntax is ByRefTypeSyntax byRefSyntax)
            {
                refKind = p.Modifiers.Any(m => m.Kind == SyntaxKind.OutKeyword) ? RefKind.Out : RefKind.Ref;
                typeSyntax = byRefSyntax.ElementType;
            }

            var type = ResolveType(typeSyntax);
            var symbol = new SourceParameterSymbol(
                p.Identifier.Text,
                type,
                _containingSymbol,
                _containingSymbol.ContainingType as INamedTypeSymbol,
                _containingSymbol.ContainingNamespace,
                [p.GetLocation()],
                [p.GetReference()],
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

        var lambdaSymbol = new SourceLambdaSymbol(parameterSymbols, inferredReturnType, _containingSymbol);

        // 5. Bind the body using a new binder scope
        var lambdaBinder = new LambdaBinder(lambdaSymbol, this);

        foreach (var param in parameterSymbols)
            lambdaBinder.DeclareParameter(param);

        var bodyExpr = lambdaBinder.BindExpression(syntax.ExpressionBody);

        lambdaBinder.SetLambdaBody(bodyExpr);

        var capturedVariables = lambdaBinder.AnalyzeCapturedVariables();

        // 6. Infer return type if not explicitly given
        var returnType = returnTypeSyntax is not null
            ? inferredReturnType
            : bodyExpr.Type ?? Compilation.ErrorTypeSymbol;

        // 7. Construct delegate type (e.g., Func<...> or custom delegate)
        var delegateType = Compilation.CreateFunctionTypeSymbol(
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
        return new BoundLambdaExpression(parameterSymbols, returnType, bodyExpr, lambdaSymbol, lambdaSymbol.DelegateType, capturedVariables);
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

    private BoundExpression BindIfExpression(IfExpressionSyntax ifExpression)
    {
        var condition = BindExpression(ifExpression.Condition);

        var thenBinder = SemanticModel.GetBinder(ifExpression, this);
        var thenExpr = thenBinder.BindExpression(ifExpression.Expression);

        BoundExpression? elseExpr = null;
        if (ifExpression.ElseClause != null)
        {
            var elseBinder = SemanticModel.GetBinder(ifExpression.ElseClause, this);
            elseExpr = elseBinder.BindExpression(ifExpression.ElseClause.Expression);
        }

        return new BoundIfExpression(condition, thenExpr, elseExpr);
    }

    private BoundExpression BindWhileExpression(WhileExpressionSyntax whileExpression)
    {
        var condition = BindExpression(whileExpression.Condition);

        var expressionBinder = SemanticModel.GetBinder(whileExpression, this);
        var expression = expressionBinder.BindExpression(whileExpression.Expression) as BoundExpression;

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

        var body = loopBinder.BindExpression(forExpression.Body) as BoundExpression;

        return new BoundForExpression(local, collection, body!);
    }

    private BoundExpression BindMemberAccessExpression(MemberAccessExpressionSyntax memberAccess)
    {
        // 🆕 Handle target-typed access: `.Test`
        if (memberAccess.Expression is null)
        {
            var memberName = memberAccess.Name.Identifier.Text;

            // Try to resolve based on the target type context (e.g., assignment or parameter)
            var expectedType = GetTargetType(memberAccess); // <- You need to implement this based on current binder state

            if (expectedType is not null)
            {
                var member = new SymbolQuery(memberName, expectedType, IsStatic: true)
                    .Lookup(this).FirstOrDefault();

                if (member is null)
                {
                    _diagnostics.ReportUndefinedName(memberName, memberAccess.Name.GetLocation());
                    return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
                }

                return new BoundMemberAccessExpression(new BoundTypeExpression(expectedType), member);
            }

            // If we can't determine the target type, report it
            _diagnostics.ReportMemberAccessRequiresTargetType(memberName, memberAccess.Name.GetLocation());
            return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
        }

        // 🔽 Existing binding for explicit receiver
        var receiver = BindExpression(memberAccess.Expression);

        if (receiver is BoundErrorExpression)
            return receiver;

        var name = memberAccess.Name.Identifier.Text;

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
            var member = new SymbolQuery(name, typeExpr.Type, IsStatic: true)
                .Lookup(this).FirstOrDefault();

            if (member is null)
            {
                var typeName = typeExpr.Symbol!.Name;
                _diagnostics.ReportMemberDoesNotContainDefinition(typeName, memberAccess.Name.ToString(), memberAccess.Name.GetLocation());
                return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
            }

            return new BoundMemberAccessExpression(typeExpr, member);
        }

        if (receiver.Type?.SpecialType == SpecialType.System_Void)
        {
            _diagnostics.ReportMemberAccessOnVoid(name, memberAccess.Name.GetLocation());
            return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
        }

        var instanceMember = receiver.Type is null
            ? null
            : new SymbolQuery(name, receiver.Type, IsStatic: false).Lookup(this).FirstOrDefault();

        if (instanceMember == null)
        {
            _diagnostics.ReportUndefinedName(name, memberAccess.Name.GetLocation());
            return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
        }

        return new BoundMemberAccessExpression(receiver, instanceMember);
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

                case AssignmentExpressionSyntax assign when assign.RightHandSide == node:
                    var left = BindExpression(assign.LeftHandSide);
                    return left.Type;

                case ReturnStatementSyntax returnStmt:
                    return _containingSymbol is IMethodSymbol method ? method.ReturnType : null;

                case BinaryExpressionSyntax binary when binary.LeftHandSide == node:
                    return BindExpression(binary.RightHandSide).Type;

                case BinaryExpressionSyntax binary when binary.RightHandSide == node:
                    return BindExpression(binary.LeftHandSide).Type;

                case ArgumentSyntax arg:
                    // TODO: support inference from parameter types
                    return null;

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
        if (syntax is PredefinedTypeSyntax predefinedType)
        {
            var type = Compilation.ResolvePredefinedType(predefinedType);
            return new BoundTypeExpression(type);
        }

        if (syntax is TupleTypeSyntax tupleTypeSyntax)
        {
            var boundTypes = tupleTypeSyntax.Types
                .Select(t => BindTypeSyntax(t))
                .OfType<BoundTypeExpression>()
                .Select(b => b.Type)
                .ToList();

            if (boundTypes.Count != tupleTypeSyntax.Types.Count)
                return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.TypeMismatch);

            var tupleType = Compilation.CreateTupleTypeSymbol(boundTypes.Select((t, i) => ($"Item{i + 1}", t)));

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

            return BindTypeName(generic.Identifier.Text, generic.GetLocation(), typeArgs);
        }

        if (syntax is QualifiedNameSyntax qualified)
        {
            var left = BindTypeSyntax(qualified.Left);

            if (left is not BoundNamespaceExpression ns && left is not BoundTypeExpression leftType)
                return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);

            string name;
            ImmutableArray<ITypeSymbol> typeArgs = [];

            if (qualified.Right is IdentifierNameSyntax id2)
            {
                name = id2.Identifier.Text;
            }
            else if (qualified.Right is GenericNameSyntax generic2)
            {
                name = generic2.Identifier.Text;
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

                var constructed = typeArgs.IsEmpty
                    ? namedType
                    : Compilation.ConstructGenericType(namedType, typeArgs.ToArray());

                return new BoundTypeExpression(constructed);
            }

            return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
        }

        return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
    }

    private BoundExpression BindTypeName(string name, Location location, ImmutableArray<ITypeSymbol> typeArguments)
    {
        var symbol = LookupType(name);

        if (symbol is ITypeSymbol type && type is INamedTypeSymbol named)
        {
            // If the resolved symbol is already a constructed generic type (e.g., from an alias
            // like `alias StringList = System.Collections.Generic.List<string>`), then we don't
            // expect any additional type arguments when the alias is used. Return the constructed
            // type directly.
            if (named.ConstructedFrom is not null)
            {
                if (!typeArguments.IsEmpty)
                {
                    //_diagnostics.ReportTypeArityMismatch(name, named.Arity, typeArguments.Length, location);
                    return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.TypeMismatch);
                }

                return new BoundTypeExpression(named);
            }

            if (named.Arity != typeArguments.Length)
            {
                //_diagnostics.ReportTypeArityMismatch(name, named.Arity, typeArguments.Length, location);
                return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.TypeMismatch);
            }

            var constructed = typeArguments.IsEmpty
                ? named
                : Compilation.ConstructGenericType(named, typeArguments.ToArray());

            return new BoundTypeExpression(constructed);
        }

        if (symbol is INamespaceSymbol ns)
            return new BoundNamespaceExpression(ns);

        _diagnostics.ReportUndefinedName(name, location);
        return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
    }

    private BoundExpression BindLiteralExpression(LiteralExpressionSyntax syntax)
    {
        if (syntax.Kind == SyntaxKind.NullLiteralExpression)
        {
            return new BoundLiteralExpression(BoundLiteralExpressionKind.NullLiteral, null!, Compilation.NullTypeSymbol);
        }

        var value = syntax.Token.Value ?? syntax.Token.Text!;
        ITypeSymbol type = value switch
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
        var symbol = LookupSymbol(syntax.Identifier.Text);

        if (symbol is null)
        {
            _diagnostics.ReportUndefinedName(syntax.Identifier.Text, syntax.Identifier.GetLocation());
            return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
        }

        return symbol switch
        {
            INamespaceSymbol ns => new BoundNamespaceExpression(ns),
            ITypeSymbol type => new BoundTypeExpression(type),
            ILocalSymbol local => new BoundLocalAccess(local),
            IParameterSymbol param => new BoundParameterAccess(param),
            IFieldSymbol field => new BoundFieldAccess(field),
            IPropertySymbol prop => new BoundPropertyAccess(prop),
            _ => new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound)
        };
    }

    private BoundExpression BindBinaryExpression(BinaryExpressionSyntax syntax)
    {
        var left = BindExpression(syntax.LeftHandSide);
        var right = BindExpression(syntax.RightHandSide);

        var opKind = syntax.OperatorToken.Kind;

        // 1. Specialfall: string + any → string.Concat(...)
        if (opKind == SyntaxKind.PlusToken &&
            (left.Type.SpecialType == SpecialType.System_String || right.Type.SpecialType == SpecialType.System_String))
        {
            var concatMethod = ResolveStringConcatMethod(left, right);
            return new BoundInvocationExpression(concatMethod, [left, right]);
        }

        // 2. Överlagrade operatorer
        var userDefinedOperator = ResolveUserDefinedOperator(opKind, left.Type, right.Type);
        if (userDefinedOperator is not null)
        {
            return new BoundInvocationExpression(userDefinedOperator, [left, right]);
        }

        // 3. Inbyggda operatorer
        var op = BoundBinaryOperator.Lookup(Compilation, opKind, left.Type, right.Type);
        if (op is not null)
        {
            return new BoundBinaryExpression(left, op, right);
        }

        // 4. Fel
        _diagnostics.ReportUndefinedBinaryOperator(opKind.ToString(), left.Type, right.Type, syntax.OperatorToken.GetLocation());

        return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
    }

    private IMethodSymbol ResolveStringConcatMethod(BoundExpression left, BoundExpression right)
    {
        var stringType = Compilation.GetSpecialType(SpecialType.System_String);
        var candidates = stringType.GetMembers("Concat").OfType<IMethodSymbol>();

        var candidate = OverloadResolver.ResolveOverload(candidates, [left, right], Compilation);

        if (candidate is null)
            throw new InvalidOperationException("No matching Concat method found.");

        return candidate;
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
            receiver = BindExpression(memberAccess.Expression);

            // ❗ Early exit if receiver is invalid
            if (receiver is BoundErrorExpression)
                return receiver;

            if (receiver.Type?.SpecialType is SpecialType.System_Void)
            {
                _diagnostics.ReportMemberAccessOnVoid(memberAccess.Name.Identifier.Text, memberAccess.Name.GetLocation());
                return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
            }

            methodName = memberAccess.Name.Identifier.Text;
        }
        else if (syntax.Expression is IdentifierNameSyntax id)
        {
            var symbol = LookupSymbol(id.Identifier.Text);

            if (symbol is ILocalSymbol or IParameterSymbol or IFieldSymbol or IPropertySymbol)
            {
                receiver = BindIdentifierName(id);
                if (receiver is BoundErrorExpression)
                    return receiver;

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
                .ToArray();

            if (candidateMethods.Length > 0)
            {
                var method = OverloadResolver.ResolveOverload(candidateMethods, boundArguments, Compilation);
                if (method is not null)
                    return new BoundInvocationExpression(method, boundArguments.ToArray(), receiver);

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
                .ToArray();

            if (candidates.Length == 0)
            {
                if (methodName == "Invoke")
                    _diagnostics.ReportInvalidInvocation(syntax.Expression.GetLocation());
                else
                    _diagnostics.ReportUndefinedName(methodName, syntax.Expression.GetLocation());

                return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
            }

            var method = OverloadResolver.ResolveOverload(candidates, boundArguments, Compilation);
            if (method is null)
            {
                _diagnostics.ReportNoOverloadForMethod(methodName, boundArguments.Length, syntax.GetLocation());
                return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.OverloadResolutionFailed);
            }

            return new BoundInvocationExpression(method, boundArguments.ToArray(), receiver);
        }

        // No receiver -> try methods first, then constructors
        var methodCandidates = new SymbolQuery(methodName).LookupMethods(this).ToArray();

        if (methodCandidates.Length > 0)
        {
            var method = OverloadResolver.ResolveOverload(methodCandidates, boundArguments, Compilation);
            if (method is not null)
                return new BoundInvocationExpression(method, boundArguments.ToArray(), null);

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

        _diagnostics.ReportUndefinedName(methodName, syntax.Expression.GetLocation());
        return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
    }

    private BoundExpression BindConstructorInvocation(
        INamedTypeSymbol typeSymbol,
        BoundExpression[] boundArguments,
        InvocationExpressionSyntax syntax,
        BoundExpression? receiver = null)
    {
        var constructor = OverloadResolver.ResolveOverload(typeSymbol.Constructors, boundArguments, Compilation);
        if (constructor is null)
        {
            _diagnostics.ReportNoOverloadForMethod(typeSymbol.Name, boundArguments.Length, syntax.GetLocation());
            return new BoundErrorExpression(typeSymbol, null, BoundExpressionReason.OverloadResolutionFailed);
        }

        return new BoundObjectCreationExpression(constructor, boundArguments, receiver);
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
            _diagnostics.ReportUndefinedName(syntax.Type.ToString(), syntax.Type.GetLocation());
            return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
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
        var constructor = OverloadResolver.ResolveOverload(typeSymbol.Constructors, boundArguments, Compilation);
        if (constructor == null)
        {
            _diagnostics.ReportNoOverloadForMethod(typeSymbol.Name, boundArguments.Length, syntax.GetLocation());
            return new BoundErrorExpression(typeSymbol, null, BoundExpressionReason.OverloadResolutionFailed);
        }

        return new BoundObjectCreationExpression(constructor, boundArguments.ToArray());
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
            _diagnostics.ReportUndefinedIndexer(receiverType, syntax.GetLocation());
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

    private BoundExpression BindAssignmentExpression(AssignmentExpressionSyntax syntax)
    {
        if (syntax.LeftHandSide is ElementAccessExpressionSyntax elementAccess)
        {
            var right = BindExpression(syntax.RightHandSide);

            var receiver = BindExpression(elementAccess.Expression);
            var args = elementAccess.ArgumentList.Arguments.Select(x => BindExpression(x.Expression)).ToArray();

            if (receiver.Type?.TypeKind is TypeKind.Array)
            {
                return new BoundArrayAssignmentExpression(
                    new BoundArrayAccessExpression(receiver, args, receiver.Type),
                    right);
            }

            var indexer = ResolveIndexer(receiver.Type!, args.Length);

            if (indexer is null || indexer.SetMethod is null)
            {
                _diagnostics.ReportInvalidIndexerAssignment(syntax.GetLocation());
                return new BoundErrorExpression(receiver.Type!, null, BoundExpressionReason.NotFound);
            }

            var access = new BoundIndexerAccessExpression(receiver, args, indexer);
            return new BoundIndexerAssignmentExpression(access, right);
        }

        // Fall back to normal variable/property assignment
        var left = BindExpression(syntax.LeftHandSide);

        if (left.Symbol is ILocalSymbol localSymbol)
        {
            if (!localSymbol.IsMutable)
            {
                _diagnostics.ReportThisValueIsNotMutable(localSymbol.Name, syntax.LeftHandSide.GetLocation());
                return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
            }

            var right2 = BindExpression(syntax.RightHandSide);

            if (right2 is BoundEmptyCollectionExpression)
            {
                return new BoundLocalAssignmentExpression(localSymbol, new BoundEmptyCollectionExpression(localSymbol.Type));
            }

            if (!IsAssignable(localSymbol.Type, right2.Type!))
            {
                _diagnostics.ReportCannotConvertFromTypeToType(right2.Type!, localSymbol.Type, syntax.RightHandSide.GetLocation());
                return new BoundErrorExpression(localSymbol.Type, null, BoundExpressionReason.TypeMismatch);
            }

            return new BoundLocalAssignmentExpression(localSymbol, right2);
        }
        else if (left.Symbol is IFieldSymbol fieldSymbol)
        {
            /* if (propertySymbol. is null)
            {
                //_diagnostics.ReportThisValueIsNotMutable(localSymbol.Name, syntax.LeftHandSide.GetLocation());
                return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
            } */

            var right2 = BindExpression(syntax.RightHandSide);

            if (right2 is BoundEmptyCollectionExpression)
            {
                return new BoundFieldAssignmentExpression(right2, fieldSymbol, new BoundEmptyCollectionExpression(fieldSymbol.Type));
            }

            if (!IsAssignable(fieldSymbol.Type, right2.Type!))
            {
                _diagnostics.ReportCannotConvertFromTypeToType(right2.Type!, fieldSymbol.Type, syntax.RightHandSide.GetLocation());
                return new BoundErrorExpression(fieldSymbol.Type, null, BoundExpressionReason.TypeMismatch);
            }

            return new BoundFieldAssignmentExpression(GetReceiver(left), fieldSymbol, right2);
        }
        else if (left.Symbol is IPropertySymbol propertySymbol)
        {
            if (propertySymbol.SetMethod is null)
            {
                //_diagnostics.ReportThisValueIsNotMutable(localSymbol.Name, syntax.LeftHandSide.GetLocation());
                return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
            }

            var right2 = BindExpression(syntax.RightHandSide);

            if (right2 is BoundEmptyCollectionExpression)
            {
                return new BoundPropertyAssignmentExpression(right2, propertySymbol, new BoundEmptyCollectionExpression(propertySymbol.Type));
            }

            if (!IsAssignable(propertySymbol.Type, right2.Type!))
            {
                _diagnostics.ReportCannotConvertFromTypeToType(right2.Type!, propertySymbol.Type, syntax.RightHandSide.GetLocation());
                return new BoundErrorExpression(propertySymbol.Type, null, BoundExpressionReason.TypeMismatch);
            }

            return new BoundPropertyAssignmentExpression(GetReceiver(left), propertySymbol, right2);
        }

        return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
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

    private bool IsAssignable(ITypeSymbol targetType, ITypeSymbol sourceType)
    {
        // Trivial exact match
        if (SymbolEqualityComparer.Default.Equals(targetType, sourceType))
            return true;

        // Allow implicit conversions, e.g., int -> double
        var conversion = Compilation.ClassifyConversion(sourceType, targetType);
        return conversion.IsImplicit;
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

        var elements = ImmutableArray.CreateBuilder<BoundExpression>();

        foreach (var expr in syntax.Elements)
        {
            var boundElement = BindExpression(expr.Expression);
            elements.Add(boundElement);
        }

        if (targetType is IArrayTypeSymbol arrayType)
        {
            var elementType = arrayType.ElementType;

            foreach (var element in elements)
            {
                if (!IsAssignable(elementType, element.Type!))
                {
                    _diagnostics.ReportCannotConvertFromTypeToType(element.Type!, elementType, syntax.GetLocation());
                }
            }

            return new BoundCollectionExpression(arrayType, elements.ToImmutable());
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

            foreach (var element in elements)
            {
                if (!IsAssignable(elementType, element.Type!))
                {
                    _diagnostics.ReportCannotConvertFromTypeToType(element.Type!, elementType, syntax.GetLocation());
                }
            }

            return new BoundCollectionExpression(namedType, elements.ToImmutable(), addMethod);
        }

        // Fallback to array if target type couldn't be determined
        ITypeSymbol? inferredElementType = elements.Count > 0
            ? elements[0].Type
            : Compilation.GetSpecialType(SpecialType.System_Object);

        var fallbackArray = Compilation.CreateArrayTypeSymbol(inferredElementType!);

        return new BoundCollectionExpression(fallbackArray, elements.ToImmutable());
    }

    public override IEnumerable<ISymbol> LookupSymbols(string name)
    {
        var seen = new HashSet<ISymbol>();
        Binder? current = this;

        while (current is not null)
        {
            if (current is BlockBinder block)
            {
                if (block._locals.TryGetValue(name, out var local) && seen.Add(local))
                    yield return local;

                if (block._localFunctions.TryGetValue(name, out var func) && seen.Add(func))
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
                    if (seen.Add(local.Name))
                        yield return local;
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

    public override BoundLocalFunctionStatement BindLocalFunction(LocalFunctionStatementSyntax localFunction)
    {
        // Get the binder from the factory
        var binder = SemanticModel.GetBinder(localFunction, this);

        if (binder is not LocalFunctionBinder localFunctionBinder)
            throw new InvalidOperationException("Expected LocalFunctionBinder");

        // Register the symbol in the current scope
        var symbol = localFunctionBinder.GetMethodSymbol();

        // Bind the body with method binder
        var methodBinder = localFunctionBinder.GetMethodBodyBinder();
        var blockBinder = SemanticModel.GetBinder(localFunction.Body, methodBinder);
        var body = blockBinder.BindExpression(localFunction.Body);

        return new BoundLocalFunctionStatement(symbol); // Possibly include body here if needed
    }
}
