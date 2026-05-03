

using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

class CompilationUnitBinder : Binder
{
    public CompilationUnitBinder(Binder parent, SemanticModel semanticModel) : base(parent)
    {
        SemanticModel = semanticModel;
    }

    public override SemanticModel SemanticModel { get; }

    public override ITypeSymbol? LookupType(string name)
    {
        var parentType = base.LookupType(name);
        if (parentType != null)
            return parentType;

        return Compilation.GlobalNamespace.GetMembers(name).OfType<ITypeSymbol>().FirstOrDefault();
    }

    public override ISymbol? LookupSymbol(string name)
        => LookupSymbols(name).FirstOrDefault();

    public override IEnumerable<ISymbol> LookupSymbols(string name)
    {
        foreach (var symbol in base.LookupSymbols(name))
            yield return symbol;

        foreach (var symbol in Compilation.GlobalNamespace.GetMembers(name))
            yield return symbol;
    }
}
