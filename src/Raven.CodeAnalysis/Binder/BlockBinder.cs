using System.Collections.Immutable;

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
                symbol = BindLocalDeclaration(varDecl);
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
            LocalDeclarationStatementSyntax localDeclaration => new BoundLocalExpression(BindLocalDeclaration(localDeclaration.Declaration.Declarators[0])),
            ExpressionStatementSyntax expressionStmt => BindExpression(expressionStmt.Expression),
            EmptyStatementSyntax emptyStatement => new BoundVoidExpression(Compilation),
            _ => throw new NotSupportedException($"Unsupported statement: {statement.Kind}")
        };
    }

    private BoundExpression BindBlock(BlockSyntax block)
    {
        var blockBinder = Compilation.BinderFactory.GetBinder(block, this);
        var statements = block.Statements.Select(blockBinder.BindStatement).ToArray();

        return new BoundBlockExpression(statements);
    }

    private ILocalSymbol BindLocalDeclaration(VariableDeclaratorSyntax variableDeclarator)
    {
        if (_locals.TryGetValue(variableDeclarator.Name.Identifier.Text, out var existingSymbol))
            return existingSymbol;

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

        ISymbol containingSymbol = null!;

        var symbol = new SourceLocalSymbol(name, type, isReadOnly, containingSymbol, null, null, [variableDeclarator.GetLocation()], [variableDeclarator.GetReference()]);
        _locals[name] = symbol;

        return symbol;
    }

    public override BoundExpression BindExpression(ExpressionSyntax syntax) => syntax switch
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
        IfExpressionSyntax ifExpression => BindIfExpression(ifExpression),
        WhileExpressionSyntax whileExpression => BindWhileExpression(whileExpression),
        BlockSyntax block => BindBlock(block),
        ExpressionSyntax.Missing missing => BindMissingExpression(missing),
        _ => throw new NotSupportedException($"Unsupported expression: {syntax.Kind}")
    };

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

        var thenBinder = Compilation.BinderFactory.GetBinder(ifExpression, this);
        var thenExpr = thenBinder.BindExpression(ifExpression.Expression);

        BoundExpression? elseExpr = null;
        if (ifExpression.ElseClause != null)
        {
            var elseBinder = Compilation.BinderFactory.GetBinder(ifExpression.ElseClause, this);
            elseExpr = elseBinder.BindExpression(ifExpression.ElseClause.Expression);
        }

        return new BoundIfExpression(condition, thenExpr, elseExpr);
    }

    private BoundExpression BindWhileExpression(WhileExpressionSyntax whileExpression)
    {
        var condition = BindExpression(whileExpression.Condition);

        var expressionBinder = Compilation.BinderFactory.GetBinder(whileExpression, this);
        var expression = expressionBinder.BindStatement(whileExpression.Statement);

        return new BoundWhileExpression(condition, expression);
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
            return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
        }

        // Static type access
        if (receiver is BoundTypeExpression typeExpr)
        {
            var member = typeExpr.Type.GetMembers(memberName).FirstOrDefault();

            if (member is null)
            {
                _diagnostics.ReportUndefinedName(memberName, memberAccess.Name.GetLocation());
                return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
            }

            return new BoundMemberAccessExpression(typeExpr, member);
        }

        // Instance member access (for objects)
        if (receiver.Type?.SpecialType == SpecialType.System_Void)
        {
            _diagnostics.ReportMemberAccessOnVoid(memberName, memberAccess.Name.GetLocation());
            return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
        }

        var instanceMember = receiver.Type?.GetMembers(memberName).FirstOrDefault();

        if (instanceMember == null)
        {
            _diagnostics.ReportUndefinedName(memberName, memberAccess.Name.GetLocation());
            return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
        }

        return new BoundMemberAccessExpression(receiver, instanceMember);
    }

    private BoundExpression BindTypeSyntax(TypeSyntax syntax)
    {
        if (syntax is IdentifierNameSyntax id)
        {
            var symbol = LookupSymbol(id.Identifier.Text);

            return symbol switch
            {
                INamespaceSymbol ns => new BoundNamespaceExpression(ns),
                ITypeSymbol type => new BoundTypeExpression(type),
                _ => new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound)
                /*
                _ =>
                {
                    _diagnostics.ReportUndefinedName(id.Identifier.Text, id.Identifier.GetLocation());
                    return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
                }
                */
            };
        }
        else if (syntax is QualifiedNameSyntax qualified)
        {
            var left = BindTypeSyntax(qualified.Left);

            if (left is BoundNamespaceExpression nsExpr)
            {
                var member = nsExpr.Namespace.GetMembers(qualified.Right.Identifier.Text)
                                             .FirstOrDefault(m => m is INamespaceSymbol || m is ITypeSymbol);

                return member switch
                {
                    INamespaceSymbol ns => new BoundNamespaceExpression(ns),
                    ITypeSymbol type => new BoundTypeExpression(type),
                    _ => new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound)
                    /*_ =>
                        {
                    _diagnostics.ReportUndefinedName(qualified.Right.Identifier.Text, qualified.Right.GetLocation());
                        return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
                }*/
                }
            ;
            }
            else if (left is BoundTypeExpression typeExpr)
            {
                var member = typeExpr.Type.GetMembers(qualified.Right.Identifier.Text)
                                          .OfType<INamedTypeSymbol>()
                                          .FirstOrDefault();

                if (member != null)
                    return new BoundTypeExpression(member);

                _diagnostics.ReportUndefinedName(qualified.Right.Identifier.Text, qualified.Right.GetLocation());
                return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
            }
            else
            {
                //_diagnostics.ReportInvalidQualifiedName(qualified.GetLocation());
                return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
            }
        }

        //_diagnostics.ReportInvalidTypeSyntax(syntax.GetLocation());
        return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
    }

    private BoundExpression BindLiteralExpression(LiteralExpressionSyntax syntax)
    {
        var value = syntax.Token.Value ?? syntax.Token.Text!;
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

        if (symbol is null)
        {
            _diagnostics.ReportUndefinedName(syntax.Identifier.Text, syntax.Identifier.GetLocation());
            return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);
        }

        return symbol switch
        {
            INamespaceSymbol ns => new BoundNamespaceExpression(ns),
            ITypeSymbol type => new BoundTypeExpression(type),
            ILocalSymbol local => new BoundLocalExpression(local),
            IPropertySymbol prop => new BoundPropertyExpression(prop),
            _ => new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound)
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
                Compilation.ErrorTypeSymbol,
                null,
                BoundExpressionReason.NotFound
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
            return new BoundErrorExpression(Compilation.ErrorTypeSymbol, null, BoundExpressionReason.NotFound);

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
        var method = ResolveOverload(candidates, boundArguments.ToArray());
        if (method == null)
        {
            _diagnostics.ReportNoOverloadForMethod(methodName, boundArguments.Count, syntax.GetLocation());
            return new BoundErrorExpression(
                Compilation.ErrorTypeSymbol,
                null,
                BoundExpressionReason.OverloadResolutionFailed
            );
        }

        return new BoundCallExpression(method, boundArguments.ToArray(), receiver);
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
        var boundArguments = new List<BoundExpression>(syntax.ArgumentList.Arguments.Count);
        bool hasErrors = false;
        foreach (var arg in syntax.ArgumentList.Arguments)
        {
            var boundArg = BindExpression(arg.Expression);
            if (boundArg is BoundErrorExpression)
                hasErrors = true;
            boundArguments.Add(boundArg);
        }

        if (hasErrors)
            return new BoundErrorExpression(typeSymbol, null, BoundExpressionReason.ArgumentBindingFailed);

        // Overload resolution
        var constructor = ResolveOverload(typeSymbol.Constructors, boundArguments.ToArray());
        if (constructor == null)
        {
            _diagnostics.ReportNoOverloadForMethod(typeSymbol.Name, boundArguments.Count, syntax.GetLocation());
            return new BoundErrorExpression(typeSymbol, null, BoundExpressionReason.OverloadResolutionFailed);
        }

        return new BoundObjectCreationExpression(constructor, boundArguments.ToArray());
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

        if (receiverType.IsArray)
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

            if (receiver.Type?.IsArray == true)
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
        var localSymbol = (ILocalSymbol)left.Symbol!;

        if (localSymbol.IsReadOnly)
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
}