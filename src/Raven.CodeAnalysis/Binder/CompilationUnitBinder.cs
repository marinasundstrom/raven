

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
    {
        var parentSymbol = base.LookupSymbol(name);
        if (parentSymbol != null)
            return parentSymbol;

        return Compilation.GlobalNamespace.GetMembers(name).FirstOrDefault();
    }
}
