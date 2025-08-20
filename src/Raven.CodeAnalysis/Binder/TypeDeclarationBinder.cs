using System.Linq;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal abstract class TypeDeclarationBinder : Binder
{
    protected TypeDeclarationBinder(Binder parent, INamedTypeSymbol containingType, SyntaxNode syntax)
        : base(parent)
    {
        ContainingSymbol = containingType;
        Syntax = syntax;
    }

    protected SyntaxNode Syntax { get; }

    public new INamedTypeSymbol ContainingSymbol { get; }

    public override ISymbol? LookupSymbol(string name)
    {
        var symbol = ContainingSymbol.GetMembers(name).FirstOrDefault();
        if (symbol is not null)
            return symbol;

        return base.LookupSymbol(name);
    }

    public override ISymbol? BindDeclaredSymbol(SyntaxNode node)
    {
        return node == Syntax ? ContainingSymbol : base.BindDeclaredSymbol(node);
    }
}
