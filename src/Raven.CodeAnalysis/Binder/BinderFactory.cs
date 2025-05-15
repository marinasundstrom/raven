using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

class BinderFactory
{
    private readonly Dictionary<SyntaxNode, Binder> _cache = new();
    private readonly Compilation _compilation;
    private Binder rootBinder;

    public BinderFactory(Compilation compilation)
    {
        _compilation = compilation;
    }

    public Binder GetBinder(SyntaxNode node, Binder parentBinder = null)
    {
        if (_cache.TryGetValue(node, out var existingBinder))
            return existingBinder;

        // special case for CompilationUnitSyntax
        if (node is CompilationUnitSyntax cu)
        {
            var topLevelBinder = CreateTopLevelBinder(cu, _compilation.GlobalBinder);
            _cache[cu] = topLevelBinder;
            return topLevelBinder;
        }

        // Ensure parent binder is constructed and cached first
        Binder actualParentBinder = parentBinder;

        if (actualParentBinder == null)
        {
            if (!_cache.TryGetValue(node.Parent, out actualParentBinder))
            {
                // Recursively create and cache the parent binder first
                actualParentBinder = GetBinder(node.Parent);
            }
        }

        // Now safely construct this node's binder with a guaranteed parent
        Binder newBinder = node switch
        {
            NamespaceDeclarationSyntax ns => CreateNamespaceBinder(ns, actualParentBinder),
            MethodDeclarationSyntax => new MethodBinder(actualParentBinder),
            BlockSyntax => new LocalScopeBinder(actualParentBinder),
            _ => actualParentBinder
        };

        _cache[node] = newBinder;
        return newBinder;
    }

    /*
    private Binder CreateCompilationUnitBinder(CompilationUnitSyntax cu, Binder parent)
    {
        // For now, treat it like a global namespace binder
        var globalNs = _compilation.GlobalNamespace;
        var binder = new NamespaceBinder(parent, globalNs, _compilation);

        // Register all `using` directives from the compilation unit
        foreach (var import in cu.Imports)
        {
            var importSymbol = _compilation.GetNamespaceSymbol(import.NamespaceOrType.ToString());
            if (importSymbol is NamespaceSymbol ns)
                binder.AddUsingDirective(ns);
        }

        return binder;
    }
    */

    private Binder CreateTopLevelBinder(CompilationUnitSyntax cu, Binder parentBinder)
    {
        // Determine if there's a file-scoped namespace
        var declaredNamespace = cu.Members.OfType<FileScopedNamespaceDeclarationSyntax>().FirstOrDefault();

        INamespaceSymbol targetNamespace;

        if (declaredNamespace is not null)
        {
            targetNamespace = _compilation.GetNamespaceSymbol(declaredNamespace.Name.ToString())
                              ?? throw new Exception("Namespace not found");

            parentBinder = new NamespaceBinder(parentBinder, targetNamespace, _compilation);
        }
        else
        {
            targetNamespace = _compilation.GlobalNamespace;
            parentBinder = new NamespaceBinder(parentBinder, targetNamespace, _compilation);
        }

        // Process import/using directives
        var imports = cu.Imports
            .Select(i => _compilation.GetNamespaceSymbol(i.NamespaceOrType.ToString()))
            .OfType<INamespaceSymbol>()
            .ToList();

        var importBinder = new ImportBinder(parentBinder, imports);

        // Synthesize the Program/Main method inside the namespace
        var mainMethodSymbol = new SynthesizedMainMethodSymbol(_compilation, targetNamespace);

        return new TopLevelBinder(importBinder, mainMethodSymbol);
    }

    private Binder CreateNamespaceBinder(NamespaceDeclarationSyntax nsSyntax, Binder parentBinder)
    {
        var nsSymbol = _compilation.GlobalNamespace.LookupNamespace(nsSyntax.Name.ToString());
        var nsBinder = new NamespaceBinder(parentBinder, (NamespaceSymbol)nsSymbol, _compilation);

        // Register `import` directives
        foreach (var importDirective in nsSyntax.Imports)
        {
            var importedNamespace = _compilation.GlobalNamespace.LookupNamespace(importDirective.NamespaceOrType.ToString());
            if (importedNamespace != null)
                nsBinder.AddUsingDirective((NamespaceSymbol)importedNamespace);
        }

        return nsBinder;
    }

    public IEnumerable<Binder> GetAllBinders()
    {
        return _cache.Values.Distinct();
    }
}
