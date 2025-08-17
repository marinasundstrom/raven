

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

class CompilationUnitBinder : Binder
{
    public CompilationUnitBinder(Binder parent, SemanticModel semanticModel) : base(parent)
    {
        SemanticModel = semanticModel;
    }

    public override SemanticModel SemanticModel { get; }

    public override ISymbol? LookupSymbol(string name)
    {
        var parentSymbol = base.LookupSymbol(name);
        if (parentSymbol != null)
            return parentSymbol;

        return Compilation.GlobalNamespace.GetMembers(name).FirstOrDefault();
    }
}