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

    public override ITypeSymbol? LookupType(string name)
    {
        var typeParameter = ContainingSymbol.TypeParameters.FirstOrDefault(tp => tp.Name == name);
        if (typeParameter is not null)
            return typeParameter;

        return base.LookupType(name);
    }

    public override ISymbol? BindDeclaredSymbol(SyntaxNode node)
    {
        return node == Syntax ? ContainingSymbol : base.BindDeclaredSymbol(node);
    }

    protected override IReadOnlyDictionary<string, ITypeSymbol> GetInScopeTypeParameters()
    {
        var map = new Dictionary<string, ITypeSymbol>(StringComparer.Ordinal);

        if (this.ContainingSymbol is INamedTypeSymbol nt && !nt.TypeParameters.IsDefaultOrEmpty)
        {
            foreach (var tp in nt.TypeParameters)
                map.TryAdd(tp.Name, tp);
        }

        return map;
    }
}
