using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

class LocalFunctionBinder : Binder
{
    private readonly LocalFunctionStatementSyntax _syntax;
    private MethodBinder? _methodBodyBinder;
    private SourceMethodSymbol? _methodSymbol;

    public LocalFunctionBinder(Binder parent, LocalFunctionStatementSyntax syntax)
        : base(parent)
    {
        _syntax = syntax;
    }

    public override ISymbol? BindDeclaredSymbol(SyntaxNode node)
    {
        if (node == _syntax)
        {
            return GetMethodSymbol();
        }

        return base.BindDeclaredSymbol(node);
    }

    public IMethodSymbol GetMethodSymbol()
    {
        if (_methodSymbol is not null)
            return _methodSymbol;

        //ISymbol container = null; //this.ContainingSymbol;
        var container = Compilation.SourceGlobalNamespace.LookupType("Program");

        var returnType = ResolveType(_syntax.ReturnType.Type);

        _methodSymbol = new SourceMethodSymbol(
            _syntax.Identifier.Text,
            returnType,
            [],
            container,
            container.ContainingType,
            container.ContainingNamespace,
            [_syntax.GetLocation()],
            [_syntax.GetReference()],
            isStatic: true);

        var parameters = _syntax.ParameterList.Parameters
            .Select(p => new SourceParameterSymbol(
                p.Identifier.Text,
                ResolveType(p.TypeAnnotation.Type),
                _methodSymbol,
                container.ContainingType,
                container.ContainingNamespace,
                [p.GetLocation()],
                [p.GetReference()]
            ))
            .ToArray();

        _methodSymbol.SetParameters(parameters);
        return _methodSymbol;
    }

    public MethodBinder GetMethodBodyBinder()
    {
        return _methodBodyBinder ??= new MethodBinder(this, GetMethodSymbol().Parameters);
    }
}