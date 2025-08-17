using System.Collections.Immutable;
using System.Reflection.Metadata;
using System.Security.Cryptography.X509Certificates;

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

        var parentSymbol1 = ParentBinder?.LookupSymbol(name);
        if (parentSymbol1 != null)
            return parentSymbol1;

        var parentSymbol = base.LookupSymbol(name);
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

        if (variableDeclarator.TypeAnnotation is null)
        {
            var initializer = variableDeclarator.Initializer;
            if (initializer is not null)
            {
                var initializerExpr = initializer.Value;
                boundInitializer = BindExpression(initializerExpr);
                type = boundInitializer.Type!;
            }
            else
            {
                //Diagnostics.ReportMemberAccessOnVoid();    
            }
        }
        else
        {
            type = ResolveType(variableDeclarator.TypeAnnotation.Type);
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
        var expr = BindExpression(returnStatement.Expression);
        return new BoundReturnStatement(expr);
    }

    public Dictionary<string, IMethodSymbol> _localFunctions = new();

    private BoundExpression BindBlock(BlockSyntax block)
    {
        if (TryGetCachedBoundNode(block) is BoundExpression cached)
            return cached;

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
            BlockSyntax block => BindBlock(block),
            IsPatternExpressionSyntax isPatternExpression => BindIsPatternExpression(isPatternExpression),
            LambdaExpressionSyntax lambdaExpression => BindLambdaExpression(lambdaExpression),
            UnaryExpressionSyntax unaryExpression => BindUnaryExpression(unaryExpression),
            SelfExpressionSyntax selfExpression => BindSelfExpression(selfExpression),
            ExpressionSyntax.Missing missing => BindMissingExpression(missing),
            _ => throw new NotSupportedException($"Unsupported expression: {syntax.Kind}")
        };

        CacheBoundNode(syntax, boundNode);

        return boundNode;
    }

    private BoundExpression BindSelfExpression(SelfExpressionSyntax selfExpression)
    {
        if (_containingSymbol is IMethodSymbol method && !method.IsStatic)
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
        var elementTypes = new List<ITypeSymbol>();
        var elementNames = new List<string?>();
        var dict = new Dictionary<string, ITypeSymbol>();

        foreach (var node in tupleExpression.Arguments)
        {
            if (node is ArgumentSyntax arg)
            {
                var boundExpr = BindExpression(arg.Expression);
                elements.Add(boundExpr);
                elementTypes.Add(boundExpr.Type ?? Compilation.ErrorTypeSymbol);

                string? name = arg.NameColon?.Name.ToString(); // might be null
                elementNames.Add(name);

                dict.Add(name, boundExpr.Type ?? Compilation.ErrorTypeSymbol);
            }
        }

        var tupleType = CreateTupleTypeSymbol(
           dict.Select(x =>
           {
               return (x.Key, x.Value);
           }).ToArray()
        );

        return new BoundTupleExpression(
            elements.ToImmutableArray(),
            tupleType
        );
    }

    internal ITypeSymbol CreateTupleTypeSymbol(IEnumerable<(string, ITypeSymbol)> typeArgs)
    {
        var systemNamespace = Compilation.GlobalNamespace.LookupNamespace("System");

        var delegateType = systemNamespace?.GetMembers("ValueTuple")
            .OfType<INamedTypeSymbol>()
            .FirstOrDefault(t => t.Arity == typeArgs.Count());

        var tupleType = (INamedTypeSymbol)delegateType.Construct(typeArgs.Select(x => x.Item2).ToArray());

        var tuple = new TupleTypeSymbol(tupleType, null, null, null, []);

        List<IFieldSymbol> elements = new List<IFieldSymbol>();

        int i = 0;
        foreach (var tupleField in tupleType.GetMembers().OfType<SubstitutedFieldSymbol>())
        {
            elements.Add(new TupleFieldSymbol(typeArgs.ElementAt(i).Item1, tupleField, tupleType, []));
            i++;
        }

        tuple.SetTupleElements(elements);

        return tuple;
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
            var type = ResolveType(p.TypeAnnotation.Type);
            var symbol = new SourceParameterSymbol(
                p.Identifier.Text,
                type,
                _containingSymbol,
                _containingSymbol.ContainingType as INamedTypeSymbol,
                _containingSymbol.ContainingNamespace,
                [p.GetLocation()],
                [p.GetReference()]
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
        var statement = expressionBinder.BindStatement(whileExpression.Statement) as BoundExpressionStatement;

        return new BoundWhileExpression(condition, statement.Expression!);
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
                var member = expectedType.GetMembers(memberName).FirstOrDefault();

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

            _diagnostics.ReportUndefinedName(name, memberAccess.Name.GetLocation());
            return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
        }

        if (receiver is BoundTypeExpression typeExpr)
        {
            var member = typeExpr.Type.GetMembers(name).FirstOrDefault();

            if (member is null)
            {
                _diagnostics.ReportUndefinedName(name, memberAccess.Name.GetLocation());
                return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
            }

            return new BoundMemberAccessExpression(typeExpr, member);
        }

        if (receiver.Type?.SpecialType == SpecialType.System_Void)
        {
            _diagnostics.ReportMemberAccessOnVoid(name, memberAccess.Name.GetLocation());
            return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
        }

        var instanceMember = receiver.Type?.ResolveMembers(name).FirstOrDefault();

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
        var symbol = LookupSymbol(name);

        if (symbol is ITypeSymbol type && type is INamedTypeSymbol named)
        {
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
            SyntaxKind.NullKeyword => BoundLiteralExpressionKind.NullLiteral,

            _ => throw new Exception("Unsupported literal type")
        };

        return new BoundLiteralExpression(kind, value, type);
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
            receiver = null;
            methodName = id.Identifier.Text;
        }
        else
        {
            _diagnostics.ReportInvalidInvocation(syntax.Expression.GetLocation());
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
            return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);

        // Lookup candidate methods
        IEnumerable<IMethodSymbol> candidates;
        if (receiver != null)
        {
            candidates = receiver.Type.ResolveMembers(methodName).OfType<IMethodSymbol>();
        }
        else
        {
            var symbol = LookupSymbol(methodName);
            if (symbol == null)
            {
                _diagnostics.ReportUndefinedName(methodName, syntax.Expression.GetLocation());
                return new BoundErrorExpression(
                    Compilation.ErrorTypeSymbol,
                    null,
                    BoundExpressionReason.NotFound
                );
            }

            candidates = symbol is IMethodSymbol single
                ? [single]
                : (symbol as INamedTypeSymbol)?.GetMembers(methodName).OfType<IMethodSymbol>() ?? Enumerable.Empty<IMethodSymbol>();
        }

        if (!candidates.Any())
        {
            _diagnostics.ReportUndefinedName(methodName, syntax.Expression.GetLocation());
            return new BoundErrorExpression(
                Compilation.ErrorTypeSymbol,
                null,
                BoundExpressionReason.NotFound
            );
        }

        // Try overload resolution
        var method = OverloadResolver.ResolveOverload(candidates, boundArguments, Compilation);
        if (method == null)
        {
            _diagnostics.ReportNoOverloadForMethod(methodName, boundArguments.Length, syntax.GetLocation());
            return new BoundErrorExpression(
                Compilation.ErrorTypeSymbol,
                null,
                BoundExpressionReason.OverloadResolutionFailed
            );
        }

        return new BoundInvocationExpression(method, boundArguments.ToArray(), receiver);
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

            return new BoundFieldAssignmentExpression(right2, fieldSymbol, right2);
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

            return new BoundPropertyAssignmentExpression(right2, propertySymbol, right2);
        }

        throw new Exception();
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
        var elements = ImmutableArray.CreateBuilder<BoundExpression>();
        ITypeSymbol? elementType = null;

        if (syntax.Elements.Count == 0)
        {
            // Empty collection: default to object[]
            elementType = Compilation.GetSpecialType(SpecialType.System_Object);

            return new BoundEmptyCollectionExpression();
        }

        foreach (var expr in syntax.Elements)
        {
            var boundElement = BindExpression(expr.Expression);
            elements.Add(boundElement);

            if (elementType == null)
            {
                elementType = boundElement.Type!;
            }
            else if (!SymbolEqualityComparer.Default.Equals(elementType, boundElement.Type))
            {
                // TODO: Add implicit conversion or report error for heterogeneous types
            }
        }

        if (elementType == null)
        {
            // Empty collection: default to object[]
            elementType = Compilation.GetSpecialType(SpecialType.System_Object);
        }

        var arrayType = Compilation.CreateArrayTypeSymbol(elementType);

        return new BoundCollectionExpression(arrayType, elements.ToArray(), elementType);
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

                foreach (var symbol in block.SymbolTable.Values)
                {
                    if (seen.Add(symbol.Name))
                        yield return symbol;
                }
            }

            // Import namespaces
            if (current is ImportBinder importBinder)
            {
                foreach (var ns in importBinder.GetImportedNamespaces())
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
