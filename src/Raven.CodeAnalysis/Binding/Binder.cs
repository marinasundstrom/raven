using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

abstract class Binder
{
    protected readonly Binder ParentBinder;
    protected readonly Dictionary<string, ISymbol> SymbolTable = new();

    public Binder(Binder parent)
    {
        ParentBinder = parent;
    }

    public virtual Compilation? Compilation
    {
        get
        {
            if (ParentBinder is null)
                return null;

            if (this is GlobalBinder globalBinder)
            {
                return globalBinder.Compilation;
            }

            return ParentBinder.Compilation;
        }
    }

    public virtual INamespaceSymbol? CurrentNamespace => ParentBinder?.CurrentNamespace;

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

    public virtual BoundExpression BindExpression(ExpressionSyntax expression)
    {
        return ParentBinder?.BindExpression(expression)
               ?? throw new NotImplementedException("BindExpression not implemented in root binder.");
    }

    public virtual ITypeSymbol ResolveType(TypeSyntax typeSyntax)
    {
        if (typeSyntax is PredefinedTypeSyntax predefinedTypeSyntax)
        {
            return Compilation.ResolvePredefinedType(predefinedTypeSyntax);
        }

        if (typeSyntax is IdentifierNameSyntax ident)
        {
            var type = LookupType(ident.Identifier.Text);
            if (type is not null)
                return type;
        }

        // Add more cases here as needed (e.g. qualified names, generics, etc.)

        throw new Exception($"Type '{typeSyntax}' could not be resolved.");
    }
}
