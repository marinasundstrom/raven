using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

class MethodBinder : BlockBinder
{
    private readonly Dictionary<string, IParameterSymbol> _parameters = new();
    private readonly IMethodSymbol _methodSymbol;

    public MethodBinder(IMethodSymbol methodSymbol, Binder parent) : base(methodSymbol, parent)
    {
        _methodSymbol = methodSymbol;
    }

    public MethodBinder(IMethodSymbol methodSymbol, Binder parent, IEnumerable<IParameterSymbol> parameters) : base(methodSymbol, parent)
    {
        _methodSymbol = methodSymbol;

        foreach (var param in parameters)
            _parameters[param.Name] = param;
    }

    public override ISymbol? LookupSymbol(string name)
    {
        var paramSymbol = _methodSymbol.Parameters.FirstOrDefault(p => p.Name == name);
        if (paramSymbol is not null)
            return paramSymbol;

        var parentSymbol = base.LookupSymbol(name);
        if (parentSymbol != null)
            return parentSymbol;

        return Compilation.GlobalNamespace.GetMembers(name).FirstOrDefault();
    }

    public override BoundBlockExpression BindBlock(BlockSyntax block)
    {
        var bound = base.BindBlock(block);

        if (_methodSymbol.IsNamedConstructor)
        {
            var selfLocal = new SourceLocalSymbol(
                "__self",
                _methodSymbol.ContainingType!,
                isMutable: true,
                _methodSymbol,
                _methodSymbol.ContainingType,
                _methodSymbol.ContainingNamespace,
                [block.GetLocation()],
                [block.GetReference()]);

            var rewriter = new NamedConstructorRewriter(_methodSymbol, selfLocal);
            bound = rewriter.Rewrite(bound);
        }

        CacheBoundNode(block, bound);
        return bound;
    }

    private sealed class NamedConstructorRewriter : BoundTreeRewriter
    {
        private readonly IMethodSymbol _methodSymbol;
        private readonly SourceLocalSymbol _self;

        public NamedConstructorRewriter(IMethodSymbol methodSymbol, SourceLocalSymbol self)
        {
            _methodSymbol = methodSymbol;
            _self = self;
        }

        public BoundBlockExpression Rewrite(BoundBlockExpression body)
        {
            var statements = VisitList(body.Statements).Cast<BoundStatement>().ToList();

            var ctor = _methodSymbol.ContainingType!.Constructors.First(c => c.Parameters.Length == 0);
            var creation = new BoundObjectCreationExpression(ctor, Array.Empty<BoundExpression>());
            var declarator = new BoundVariableDeclarator(_self, creation);
            var declaration = new BoundLocalDeclarationStatement(new[] { declarator });
            statements.Insert(0, declaration);
            statements.Add(new BoundReturnStatement(new BoundLocalAccess(_self)));

            return new BoundBlockExpression(statements);
        }

        public override BoundNode? VisitSelfExpression(BoundSelfExpression node)
        {
            return new BoundLocalAccess(_self);
        }
    }

    public IMethodSymbol GetMethodSymbol() => _methodSymbol;
}