using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

abstract class Binder
{
    protected readonly Binder ParentBinder;
    protected readonly NamespaceSymbol CurrentNamespace;
    protected readonly Dictionary<string, ISymbol> SymbolTable = new();

    public Binder(Binder parent)
    {
        ParentBinder = parent;
        //CurrentNamespace = currentNamespace;
    }

    public virtual SymbolInfo BindSymbol(SyntaxNode node)
    {
        return ParentBinder?.BindSymbol(node) ?? default;
    }

    public virtual TypeInfo BindType(SyntaxNode node)
    {
        return ParentBinder?.BindType(node) ?? default;
    }

    /// <summary>
    /// Looks up a type name, checking the current namespace and falling back to global.
    /// </summary>
    public virtual ITypeSymbol LookupType(string name)
    {
        // Check if the type exists in the current namespace
        var type = CurrentNamespace?.LookupType(name);
        if (type != null)
            return type;

        // Delegate to parent binder (if any)
        return ParentBinder?.LookupType(name);
    }

    public virtual INamespaceSymbol LookupNamespace(string name)
    {
        // First, check the current namespace
        var ns = CurrentNamespace?.LookupNamespace(name);
        if (ns != null)
            return ns;

        // Check parent binders
        return ParentBinder?.LookupNamespace(name);
    }

    public void DeclareSymbol(string name, ISymbol symbol)
    {
        if (SymbolTable.ContainsKey(name))
            throw new Exception($"Symbol '{name}' is already declared in this scope.");
        SymbolTable[name] = symbol;
    }

    public virtual ISymbol? LookupSymbol(string name)
    {
        return SymbolTable.TryGetValue(name, out var symbol) ? symbol : ParentBinder?.LookupSymbol(name);
    }
}
