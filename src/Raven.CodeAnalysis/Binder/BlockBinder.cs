using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

class BlockBinder : Binder
{
    private readonly Dictionary<string, ILocalSymbol> _locals = new();

    public BlockBinder(Binder parent) : base(parent) { }

    public override ISymbol? LookupSymbol(string name)
    {
        if (_locals.TryGetValue(name, out var sym))
            return sym;

        return base.LookupSymbol(name);
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
                return BindExpression(exprStmt.Expression).GetSymbolInfo();

            default:
                return base.BindSymbol(node);
        }

        return new SymbolInfo(symbol);
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
            _ => throw new NotSupportedException($"Unsupported expression: {syntax.Kind}")
        };
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

        if (symbol is ILocalSymbol local)
            return new BoundLocalExpression(local);

        _diagnostics.ReportUndefinedName(syntax.Identifier.Text, syntax.GetLocation());

        return new BoundErrorExpression(
            Compilation.GetSpecialType(SpecialType.System_Object),
            null,
            CandidateReason.NotFound
        );
    }

    private BoundExpression BindBinaryExpression(BinaryExpressionSyntax syntax)
    {
        var left = BindExpression(syntax.LeftHandSide);
        var right = BindExpression(syntax.RightHandSide);

        var opKind = syntax.OperatorToken.Kind;
        var op = BoundBinaryOperator.Lookup(opKind, left.Type, right.Type);

        if (op is null)
        {
            _diagnostics.ReportUndefinedBinaryOperator(opKind.ToString(), left.Type, right.Type, syntax.OperatorToken.GetLocation());

            return new BoundErrorExpression(
                Compilation.GetSpecialType(SpecialType.System_Object),
                null,
                CandidateReason.NotFound
            );
        }

        return new BoundBinaryExpression(left, op, right);
    }

    private BoundExpression BindInvocationExpression(InvocationExpressionSyntax syntax)
    {
        if (syntax.Expression is not IdentifierNameSyntax id)
        {
            _diagnostics.ReportInvalidInvocation(syntax.Expression.GetLocation());

            return new BoundErrorExpression(
                Compilation.GetSpecialType(SpecialType.System_Object),
                null,
                CandidateReason.NotFound
            );
        }

        var symbol = LookupSymbol(id.Identifier.Text);

        if (symbol is not IMethodSymbol method)
        {
            _diagnostics.ReportNotInvocable(id.Identifier.Text, syntax.Expression.GetLocation());

            return new BoundErrorExpression(
                Compilation.GetSpecialType(SpecialType.System_Object),
                symbol,
                CandidateReason.NotInvocable
            );
        }

        var arguments = syntax.ArgumentList.Arguments.Select(arg => BindExpression(arg.Expression)).ToArray();

        if (method.Parameters.Length != arguments.Length)
        {
            _diagnostics.ReportArgumentCountMismatch(
                method.Name,
                method.Parameters.Length,
                arguments.Length,
                syntax.ArgumentList.GetLocation()
            );

            return new BoundErrorExpression(
                Compilation.GetSpecialType(SpecialType.System_Object),
                method,
                CandidateReason.WrongArity
            );
        }

        return new BoundCallExpression(method, arguments);
    }
}