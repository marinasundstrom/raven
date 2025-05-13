using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

class BinderFactory
{
    private readonly Dictionary<SyntaxNode, Binder> _cache = new();
    private readonly Compilation _compilation;
    private GlobalBinder rootBinder;

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

        if (node.Parent is null)
        {
            actualParentBinder = rootBinder ??= _compilation.GlobalBinder;
        }

        Binder newBinder = node switch
        {
            CompilationUnitSyntax cu => CreateTopLevelBinder(cu, actualParentBinder),
            NamespaceDeclarationSyntax nsSyntax => CreateNamespaceBinder(nsSyntax, actualParentBinder),
            // ClassDeclarationSyntax => new TypeBinder(actualParentBinder),
            MethodDeclarationSyntax => new MethodBinder(actualParentBinder),
            BlockSyntax => new LocalScopeBinder(actualParentBinder),
            _ => actualParentBinder // Use the resolved parent binder
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
