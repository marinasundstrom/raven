using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

class BlockBinder : Binder
{
    private readonly Dictionary<string, ILocalSymbol> _locals = new();

    public BlockBinder(Binder parent) : base(parent) { }

    public override ISymbol? LookupSymbol(string name)
    {
        // Local scope
        if (_locals.TryGetValue(name, out var sym))
            return sym;

        // Parent scopes
        var parentSymbol = base.LookupSymbol(name);
        if (parentSymbol != null)
            return parentSymbol;

        // Global namespace lookup
        return Compilation.GlobalNamespace
            .GetMembers(name)
            .FirstOrDefault(); // could be namespace or type
    }

    public override SymbolInfo BindSymbol(SyntaxNode node)
    {
        ISymbol? symbol = null;

        switch (node)
        {
            case VariableDeclaratorSyntax varDecl:
                symbol = BindVariableDeclaration(varDecl);
                break;

            case ExpressionSyntax expr:
                return BindExpression(expr).GetSymbolInfo();

            case ExpressionStatementSyntax exprStmt:
                return BindStatement(exprStmt).GetSymbolInfo();

            default:
                return base.BindSymbol(node);
        }

        return new SymbolInfo(symbol);
    }

    public override BoundExpression BindStatement(StatementSyntax statement)
    {
        return statement switch
        {
            LocalDeclarationStatementSyntax localDeclaration => new BoundLocalExpression(BindVariableDeclaration(localDeclaration.Declaration.Declarators[0])),
            ExpressionStatementSyntax expressionStmt => BindExpression(expressionStmt.Expression),
            /*BlockSyntax block => BindBlock(block),
            IfStatementSyntax ifStmt => BindIfStatement(ifStmt),
            WhileStatementSyntax whileStmt => BindWhileStatement(whileStmt),
            ForStatementSyntax forStmt => BindForStatement(forStmt),
            ReturnStatementSyntax returnStmt => BindReturnStatement(returnStmt), */
            _ => throw new NotSupportedException($"Unsupported statement: {statement.Kind}")
        };
    }

    private BoundExpression BindBlock(BlockSyntax block)
    {
        var blockBinder = new BlockBinder(this);
        var statements = block.Statements.Select(blockBinder.BindStatement).ToArray();

        return new BoundBlockExpression(statements);
    }

    private ILocalSymbol BindVariableDeclaration(VariableDeclaratorSyntax variableDeclarator)
    {
        var name = variableDeclarator.Name.Identifier.Text;

        var decl = variableDeclarator.Parent as VariableDeclarationSyntax;
        var isReadOnly = decl!.LetOrVarKeyword.IsKind(SyntaxKind.LetKeyword);

        ITypeSymbol type;

        if (variableDeclarator.TypeAnnotation is null)
        {
            var initializerExpr = variableDeclarator.Initializer!.Value;
            var boundInitializer = BindExpression(initializerExpr);
            type = boundInitializer.Type!;
        }
        else
        {
            type = ResolveType(variableDeclarator.TypeAnnotation.Type);
        }

        var symbol = new SourceLocalSymbol(name, type, isReadOnly, null, null, null, [variableDeclarator.GetLocation()], []);
        _locals[name] = symbol;

        return symbol;
    }

    public override BoundExpression BindExpression(ExpressionSyntax syntax)
    {
        return syntax switch
        {
            LiteralExpressionSyntax literal => BindLiteralExpression(literal),
            IdentifierNameSyntax identifier => BindIdentifierName(identifier),
            BinaryExpressionSyntax binary => BindBinaryExpression(binary),
            InvocationExpressionSyntax invocation => BindInvocationExpression(invocation),
            MemberAccessExpressionSyntax memberAccess => BindMemberAccessExpression(memberAccess),
            BlockSyntax block => BindBlock(block),
            _ => throw new NotSupportedException($"Unsupported expression: {syntax.Kind}")
        };
    }

    private BoundExpression BindMemberAccessExpression(MemberAccessExpressionSyntax memberAccess)
    {
        var receiver = BindExpression(memberAccess.Expression);

        // If receiver is already an error, short-circuit
        if (receiver is BoundErrorExpression)
            return receiver;

        var memberName = memberAccess.Name.Identifier.Text;

        // Namespace access
        if (receiver is BoundNamespaceExpression nsExpr)
        {
            var member = nsExpr.Namespace.GetMembers(memberName).FirstOrDefault();

            if (member is INamespaceSymbol ns2)
                return new BoundNamespaceExpression(ns2);

            if (member is ITypeSymbol type)
                return new BoundTypeExpression(type);

            _diagnostics.ReportUndefinedName(memberName, memberAccess.Name.GetLocation());
            return new BoundErrorExpression(ErrorTypeSymbol.Default, null, CandidateReason.NotFound);
        }

        // Static type access
        if (receiver is BoundTypeExpression typeExpr)
        {
            var member = typeExpr.Type.GetMembers(memberName).FirstOrDefault();

            if (member is null)
            {
                _diagnostics.ReportUndefinedName(memberName, memberAccess.Name.GetLocation());
                return new BoundErrorExpression(ErrorTypeSymbol.Default, null, CandidateReason.NotFound);
            }

            return new BoundMemberAccessExpression(typeExpr, member);
        }

        // Instance member access (for objects)
        if (receiver.Type?.SpecialType == SpecialType.System_Void)
        {
            _diagnostics.ReportMemberAccessOnVoid(memberName, memberAccess.Name.GetLocation());
            return new BoundErrorExpression(ErrorTypeSymbol.Default, null, CandidateReason.NotFound);
        }

        var instanceMember = receiver.Type?.GetMembers(memberName).FirstOrDefault();

        if (instanceMember == null)
        {
            _diagnostics.ReportUndefinedName(memberName, memberAccess.Name.GetLocation());
            return new BoundErrorExpression(ErrorTypeSymbol.Default, null, CandidateReason.NotFound);
        }

        return new BoundMemberAccessExpression(receiver, instanceMember);
    }

    private BoundExpression BindLiteralExpression(LiteralExpressionSyntax syntax)
    {
        var value = syntax.Token.Value!;
        ITypeSymbol type = value switch
        {
            int => Compilation.GetSpecialType(SpecialType.System_Int32),
            bool => Compilation.GetSpecialType(SpecialType.System_Boolean),
            string => Compilation.GetSpecialType(SpecialType.System_String),
            _ => throw new Exception("Unsupported literal type")
        };

        return new BoundLiteralExpression(value, type);
    }

    private BoundExpression BindIdentifierName(IdentifierNameSyntax syntax)
    {
        var symbol = LookupSymbol(syntax.Identifier.Text);

        return symbol switch
        {
            INamespaceSymbol ns => new BoundNamespaceExpression(ns),
            ITypeSymbol type => new BoundTypeExpression(type),
            ILocalSymbol local => new BoundLocalExpression(local),
            _ => new BoundErrorExpression(ErrorTypeSymbol.Default, null, CandidateReason.NotFound)
        };
    }

    private BoundExpression BindBinaryExpression(BinaryExpressionSyntax syntax)
    {
        var left = BindExpression(syntax.LeftHandSide);
        var right = BindExpression(syntax.RightHandSide);

        var opKind = syntax.OperatorToken.Kind;
        var op = BoundBinaryOperator.Lookup(Compilation, opKind, left.Type, right.Type);

        if (op is null)
        {
            _diagnostics.ReportUndefinedBinaryOperator(opKind.ToString(), left.Type, right.Type, syntax.OperatorToken.GetLocation());

            return new BoundErrorExpression(
                ErrorTypeSymbol.Default,
                null,
                CandidateReason.NotFound
            );
        }

        return new BoundBinaryExpression(left, op, right);
    }

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

            if (receiver.Type?.SpecialType == SpecialType.System_Void)
            {
                _diagnostics.ReportMemberAccessOnVoid(memberAccess.Name.Identifier.Text, memberAccess.Name.GetLocation());
                return new BoundErrorExpression(ErrorTypeSymbol.Default, null, CandidateReason.NotFound);
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
            return new BoundErrorExpression(ErrorTypeSymbol.Default, null, CandidateReason.NotFound);
        }

        // Bind arguments
        var boundArguments = new List<BoundExpression>();
        bool hasErrors = false;
        foreach (var arg in syntax.ArgumentList.Arguments)
        {
            var boundArg = BindExpression(arg.Expression);
            if (boundArg is BoundErrorExpression)
                hasErrors = true;
            boundArguments.Add(boundArg);
        }

        if (hasErrors)
            return new BoundErrorExpression(ErrorTypeSymbol.Default, null, CandidateReason.NotFound);

        // Lookup candidate methods
        IEnumerable<IMethodSymbol> candidates;
        if (receiver != null)
        {
            candidates = receiver.Type.GetMembers(methodName).OfType<IMethodSymbol>();
        }
        else
        {
            var symbol = LookupSymbol(methodName);
            if (symbol == null)
            {
                _diagnostics.ReportSymbolNotFound(methodName, syntax.Expression.GetLocation());
                return new BoundErrorExpression(
                    ErrorTypeSymbol.Default,
                    null,
                    CandidateReason.NotFound
                );
            }

            candidates = symbol is IMethodSymbol single
                ? new[] { single }
                : (symbol as INamedTypeSymbol)?.GetMembers(methodName).OfType<IMethodSymbol>() ?? Enumerable.Empty<IMethodSymbol>();
        }

        if (!candidates.Any())
        {
            _diagnostics.ReportSymbolNotFound(methodName, syntax.Expression.GetLocation());
            return new BoundErrorExpression(
                ErrorTypeSymbol.Default,
                null,
                CandidateReason.NotFound
            );
        }

        // Try overload resolution
        var method = ResolveOverload(candidates, boundArguments.ToArray());
        if (method == null)
        {
            _diagnostics.ReportNoMatchingOverload(methodName, syntax.GetLocation());
            return new BoundErrorExpression(
                ErrorTypeSymbol.Default,
                null,
                CandidateReason.OverloadResolutionFailure
            );
        }

        return new BoundCallExpression(method, boundArguments.ToArray(), receiver);
    }

    private IMethodSymbol? ResolveOverload(IEnumerable<IMethodSymbol> methods, BoundExpression[] arguments)
    {
        foreach (var method in methods)
        {
            var parameters = method.Parameters;

            if (parameters.Length != arguments.Length)
                continue;

            bool allMatch = true;
            for (int i = 0; i < arguments.Length; i++)
            {
                if (!Compilation.ClassifyConversion(arguments[i].Type, parameters[i].Type).IsImplicit)
                {
                    allMatch = false;
                    break;
                }
            }

            if (allMatch)
                return method;
        }

        return null; // No matching overload
    }
}