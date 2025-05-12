using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

sealed class GlobalBinder : Binder
{
    public GlobalBinder(Compilation compilation) : base(null!)
    {
        Compilation = compilation;
    }

    public Compilation Compilation { get; }

    public override SymbolInfo BindSymbol(SyntaxNode node)
    {
        throw new NotImplementedException();
    }
}
