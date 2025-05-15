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

    private BoundExpression BindMemberAccessExpression(MemberAccessExpressionSyntax syntax)
    {
        var receiver = BindExpression(syntax.Expression);

        var memberName = syntax.Name.Identifier.Text;

        if (receiver is BoundNamespaceExpression nsExpr)
        {
            var ns = nsExpr.Namespace;
            var member = ns.GetMembers(memberName).FirstOrDefault();

            if (member is INamespaceSymbol ns2)
                return new BoundNamespaceExpression(ns2);

            if (member is ITypeSymbol type)
                return new BoundTypeExpression(type);

            _diagnostics.ReportUndefinedName(memberName, syntax.Name.GetLocation());
            return new BoundErrorExpression(ErrorTypeSymbol.Default, null, CandidateReason.NotFound);
        }

        var receiverType = receiver.Type;

        var symbol = receiverType
            .GetMembers(memberName)
            .FirstOrDefault();

        if (symbol is null)
        {
            _diagnostics.ReportUndefinedName(memberName, syntax.Name.GetLocation());
            return new BoundErrorExpression(ErrorTypeSymbol.Default, null, CandidateReason.NotFound);
        }

        return new BoundMemberAccessExpression(receiver, symbol);
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

        if (symbol is INamespaceSymbol ns)
            return new BoundNamespaceExpression(ns);

        if (symbol is ILocalSymbol local)
            return new BoundLocalExpression(local);

        _diagnostics.ReportUndefinedName(syntax.Identifier.Text, syntax.GetLocation());

        return new BoundErrorExpression(
            ErrorTypeSymbol.Default,
            null,
            CandidateReason.NotFound
        );
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

        // Determine the method name and receiver
        if (syntax.Expression is MemberAccessExpressionSyntax memberAccess)
        {
            receiver = BindExpression(memberAccess.Expression);
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
            return new BoundErrorExpression(
                ErrorTypeSymbol.Default,
                null,
                CandidateReason.NotFound
            );
        }

        // Bind argument expressions first
        var arguments = syntax.ArgumentList.Arguments.Select(arg => BindExpression(arg.Expression)).ToArray();

        IEnumerable<IMethodSymbol> candidates;
        if (receiver != null)
        {
            var receiverType = receiver.Type;
            candidates = receiverType.GetMembers(methodName).OfType<IMethodSymbol>();
        }
        else
        {
            var symbol = LookupSymbol(methodName);
            candidates = symbol is IMethodSymbol single
                ? new[] { single }
                : (symbol as INamedTypeSymbol)?.GetMembers(methodName).OfType<IMethodSymbol>() ?? Enumerable.Empty<IMethodSymbol>();
        }

        // Overload resolution: find best match
        var method = ResolveOverload(candidates, arguments);
        if (method == null)
        {
            _diagnostics.NotAMethod(methodName, syntax.Expression.GetLocation());
            return new BoundErrorExpression(
                Compilation.GetSpecialType(SpecialType.System_Object),
                null,
                CandidateReason.Ambiguous // or NotInvocable
            );
        }

        return new BoundCallExpression(method, arguments, receiver);
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