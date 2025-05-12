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

    private BoundNode BindExpression(ExpressionSyntax expression)
    {
        throw new NotImplementedException();
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
        ITypeSymbol type = null!; // ResolveType(varDecl.TypeAnnotation); // t.ex. "int"
        var name = variableDeclarator.Name.Identifier.Text;

        var isReadOnly = (variableDeclarator.Parent as VariableDeclarationSyntax).LetOrVarKeyword.IsKind(SyntaxKind.LetKeyword);

        var symbol = new SourceLocalSymbol(name, type, isReadOnly, null, null, null, [variableDeclarator.GetLocation()], []);
        _locals[name] = symbol;

        return symbol;
    }

    /*
    private ITypeSymbol ResolveType(TypeSyntax typeSyntax)
    {
        return _compilation.GetTypeByName(typeSyntax.ToString());
    }
    */
}
