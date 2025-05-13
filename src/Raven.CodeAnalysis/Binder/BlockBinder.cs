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
        switch (node)
        {
            case VariableDeclaratorSyntax varDecl:
                return new SymbolInfo(BindVariableDeclaration(varDecl));

            case ExpressionStatementSyntax exprStmt:
                BindExpression(exprStmt.Expression);
                break;

            default:
                return base.BindSymbol(node);
        }

        return default!;

        //return base.BindSymbol(node);
    }

    private ILocalSymbol BindVariableDeclaration(VariableDeclaratorSyntax variableDeclarator)
    {
        var name = variableDeclarator.Name.Identifier.Text;

        var decl = variableDeclarator.Parent as VariableDeclarationSyntax;
        var isReadOnly = decl!.LetOrVarKeyword.IsKind(SyntaxKind.LetKeyword);

        ITypeSymbol type = null!;
        if (variableDeclarator.TypeAnnotation is null)
        {
            // Infer type from initializer
            var initializerExpr = variableDeclarator.Initializer!.Value;
            var boundInitializer = BindExpression(initializerExpr);
            type = boundInitializer.Type!;
        }
        else
        {
            // Use explicitly declared type
            type = ResolveType(variableDeclarator.TypeAnnotation.Type);
        }

        var symbol = new SourceLocalSymbol(name, type, isReadOnly, null, null, null, [variableDeclarator.GetLocation()], []);
        _locals[name] = symbol;

        return symbol;
    }

    public override BoundExpression BindExpression(ExpressionSyntax syntax)
    {
        switch (syntax)
        {
            case LiteralExpressionSyntax literal:
                return BindLiteralExpression(literal);

            /*
            case IdentifierNameSyntax identifier:
                return BindIdentifierName(identifier);

            case BinaryExpressionSyntax binary:
                return BindBinaryExpression(binary);

            case InvocationExpressionSyntax invocation:
                return BindInvocationExpression(invocation);
            */

            default:
                throw new NotSupportedException($"Unsupported expression: {syntax.Kind}");
        }
    }

    private BoundExpression BindLiteralExpression(LiteralExpressionSyntax syntax)
    {
        var value = syntax.Token.Value; // already parsed
        ITypeSymbol type;

        switch (value)
        {
            case int _: type = Compilation.GetSpecialType(SpecialType.System_Int32); break;
            case bool _: type = Compilation.GetSpecialType(SpecialType.System_Boolean); break;
            case string _: type = Compilation.GetSpecialType(SpecialType.System_String); break;
            default: throw new Exception("Unsupported literal type");
        }

        return new BoundLiteralExpression(value, type);
    }
}
