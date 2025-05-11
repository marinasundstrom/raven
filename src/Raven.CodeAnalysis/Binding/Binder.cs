using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

abstract class Binder
{
    protected readonly Binder ParentBinder;
    protected readonly NamespaceSymbol CurrentNamespace;

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
}

sealed class RootBinder : Binder
{
    public RootBinder(Binder parent) : base(parent) { }

    public override SymbolInfo BindSymbol(SyntaxNode node)
    {
        throw new NotImplementedException();
    }
}

class NamespaceBinder : Binder
{
    private readonly NamespaceSymbol _namespaceSymbol;
    private readonly Compilation _compilation;
    private readonly List<NamespaceSymbol> _imports = new(); // Stores `using` directives

    public NamespaceBinder(Binder parent, NamespaceSymbol ns, Compilation compilation)
        : base(parent)
    {
        _namespaceSymbol = ns;
        _compilation = compilation;
    }

    /// <summary>
    /// Adds a namespace to the list of imports (`using System;`).
    /// </summary>
    public void AddUsingDirective(NamespaceSymbol importedNamespace)
    {
        if (!_imports.Contains(importedNamespace))
            _imports.Add(importedNamespace);
    }

    /// <summary>
    /// Looks up a type, checking imported namespaces before the current namespace.
    /// </summary>
    public override ITypeSymbol LookupType(string name)
    {
        // 1. Check the current namespace
        var type = _namespaceSymbol.LookupType(name);
        if (type != null)
            return type;

        // 2. Check imported namespaces (from `using` statements)
        foreach (var ns in _imports)
        {
            type = ns.LookupType(name);
            if (type != null)
                return type;
        }

        // 3. Finally, check global namespace for metadata types
        return _compilation.GlobalNamespace.LookupType(name) ?? base.LookupType(name);
    }
}

class MethodBinder : Binder
{
    public MethodBinder(Binder parent) : base(parent) { }

    private readonly Dictionary<string, IParameterSymbol> _parameters = new();

    public MethodBinder(Binder parent, IEnumerable<IParameterSymbol> parameters) : base(parent)
    {
        foreach (var param in parameters)
            _parameters[param.Name] = param;
    }

    public override SymbolInfo BindSymbol(SyntaxNode node)
    {
        if (node is IdentifierNameSyntax identifier)
        {
            if (_parameters.TryGetValue(identifier.Identifier.Text, out var param))
                return new SymbolInfo(param);
        }
        return base.BindSymbol(node);
    }
}

class LocalScopeBinder : Binder
{
    private readonly Dictionary<string, SourceLocalSymbol> _locals = new();

    public LocalScopeBinder(Binder parent) : base(parent) { }

    /// <summary>
    /// Defines a new local variable in the current scope.
    /// </summary>
    public void DeclareLocal(string name, ITypeSymbol type)
    {
        if (_locals.ContainsKey(name))
        {
            throw new Exception($"Variable '{name}' is already defined in this scope.");
        }

        _locals[name] = null; //new SourceLocalSymbol(name, type);
    }

    /// <summary>
    /// Attempts to resolve a local variable in this scope or parent binders.
    /// </summary>
    public override SymbolInfo BindSymbol(SyntaxNode node)
    {
        if (node is IdentifierNameSyntax identifier)
        {
            // First, check local variables
            if (_locals.TryGetValue(identifier.Identifier.Text, out var symbol))
                return new SymbolInfo(symbol);

            // If not found, check method parameters
            return ParentBinder?.BindSymbol(node) ?? default;
        }

        return base.BindSymbol(node);
    }
}

class BinderFactory
{
    private readonly Dictionary<SyntaxNode, Binder> _cache = new();
    private readonly Compilation _compilation;

    public BinderFactory(Compilation compilation)
    {
        _compilation = compilation;
    }

    public Binder GetBinder(SyntaxNode node, Binder parentBinder = null)
    {
        if (_cache.TryGetValue(node, out var existingBinder))
        {
            return existingBinder;
        }

        // Ensure parent binder exists before creating the current binder
        var actualParentBinder = parentBinder ?? (node.Parent != null ? GetBinder(node.Parent) : null)!;

        Binder newBinder = node switch
        {
            NamespaceDeclarationSyntax nsSyntax => CreateNamespaceBinder(nsSyntax, actualParentBinder),
            // ClassDeclarationSyntax => new TypeBinder(actualParentBinder),
            MethodDeclarationSyntax => new MethodBinder(actualParentBinder),
            BlockSyntax => new LocalScopeBinder(actualParentBinder),
            _ => actualParentBinder // Use the resolved parent binder
        };

        _cache[node] = newBinder;
        return newBinder;
    }

    public BoundExpression BindExpression(SyntaxNode node)
    {
        return node switch
        {
            LiteralExpressionSyntax literal => new BoundLiteralExpression(literal.Token.Value, GetTypeFromLiteral(literal.Token.Value)),
            IdentifierNameSyntax identifier => BindVariable(identifier),
            BinaryExpressionSyntax binary => BindBinaryExpression(binary),
            _ => throw new Exception($"Unexpected syntax: {node.Kind}")
        };
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

    private ITypeSymbol GetTypeFromLiteral(object value)
    {
        throw new NotImplementedException();
    }

    private BoundLiteralExpression BindVariable(IdentifierNameSyntax identifier)
    {
        throw new NotImplementedException();
    }

    private BoundBinaryExpression BindBinaryExpression(BinaryExpressionSyntax syntax)
    {
        var left = BindExpression(syntax.LeftHandSide);
        var right = BindExpression(syntax.RightHandSide);

        var op = BoundBinaryOperator.Lookup(syntax.OperatorToken.Kind, left.Type, right.Type);
        if (op == null)
        {
            throw new Exception($"Operator '{syntax.OperatorToken.Text}' is not defined for types {left.Type} and {right.Type}.");
        }

        return new BoundBinaryExpression(left, op, right);
    }
}