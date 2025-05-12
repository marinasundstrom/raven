using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

class BlockBinder : Binder
{
    private readonly Dictionary<string, ILocalSymbol> _locals = new();

    public BlockBinder(Binder parent) : base(parent) { }

    public BoundNode BindStatement(SyntaxNode statement)
    {
        return statement switch
        {
            LocalDeclarationStatementSyntax varDecl => BindVariableDeclaration(varDecl),
            ExpressionStatementSyntax exprStmt => BindExpression(exprStmt.Expression),
            _ => null!,
        };
    }

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

    private BoundNode BindVariableDeclaration(LocalDeclarationStatementSyntax variableDeclarationSyntax)
    {
        foreach (var decl in variableDeclarationSyntax.Declaration.Declarators)
        {
            /*
            var type = ResolveType(varDecl.TypeAnnotation); // t.ex. "int"
            var name = node.Identifier.Text;
            var symbol = new SourceLocalSymbol(name, type);
            _locals[name] = symbol;
            */
        }

        return null!;
    }
}
