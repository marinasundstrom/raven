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
            BlockSyntax => actualParentBinder is MethodBinder ? new BlockBinder((actualParentBinder?.ParentBinder as LocalFunctionBinder)?.GetMethodSymbol(), actualParentBinder) : new LocalScopeBinder(actualParentBinder),
            IfExpressionSyntax expr => new LocalScopeBinder(actualParentBinder),
            ElseClauseSyntax elseClause => new LocalScopeBinder(actualParentBinder),
            WhileExpressionSyntax expr => new LocalScopeBinder(actualParentBinder),
            LocalFunctionStatementSyntax localFunc => new LocalFunctionBinder(actualParentBinder, localFunc),
            _ => actualParentBinder
        };

        _cache[node] = newBinder;
        return newBinder;
    }

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

        var programClassSymbol = new SynthesizedProgramClassSymbol(_compilation, targetNamespace.AsSourceNamespace(), [cu.GetLocation()], [cu.GetReference()]);

        var mainMethodSymbol = new SynthesizedMainMethodSymbol(programClassSymbol, [cu.GetLocation()], [cu.GetReference()]);

        var topLevelBinder = new TopLevelBinder(importBinder, mainMethodSymbol);

        foreach (var member in cu.Members.OfType<BaseTypeDeclarationSyntax>())
        {
            if (member is EnumDeclarationSyntax enumDeclaration)
            {
                var enumType = new SourceNamedTypeSymbol(enumDeclaration.Identifier.Text, _compilation.GetTypeByMetadataName("System.Enum"), TypeKind.Enum, targetNamespace.AsSourceNamespace(), null, targetNamespace.AsSourceNamespace(),
                    [enumDeclaration.GetLocation()], [enumDeclaration.GetReference()]);

                int value = 0;
                foreach (var enumMember in enumDeclaration.Members)
                {
                    new SourceFieldSymbol(enumMember.Identifier.Text, enumType, true, true, value, enumType, enumType, targetNamespace.AsSourceNamespace(),
                         [enumMember.GetLocation()], [enumMember.GetReference()]);
                    value++;
                }
            }
        }

        // 🟢 Step 1: Predeclare all local functions
        foreach (var stmt in cu.Members.OfType<GlobalStatementSyntax>())
        {
            if (stmt.Statement is LocalFunctionStatementSyntax localFunc)
            {
                var binder = GetBinder(localFunc, topLevelBinder);
                if (binder is LocalFunctionBinder lfBinder)
                {
                    var symbol = lfBinder.GetMethodSymbol();
                    topLevelBinder.DeclareLocalFunction(symbol);
                }
            }
        }

        // 🟢 Step 2: Bind all statements
        foreach (var stmt in cu.Members.OfType<GlobalStatementSyntax>())
        {
            topLevelBinder.BindGlobalStatement(stmt);
        }

        return topLevelBinder;
    }

    private Binder CreateNamespaceBinder(NamespaceDeclarationSyntax nsSyntax, Binder parentBinder)
    {
        var nsSymbol = _compilation.GlobalNamespace.LookupNamespace(nsSyntax.Name.ToString());
        var nsBinder = new NamespaceBinder(parentBinder, (INamespaceSymbol)nsSymbol, _compilation);

        // Register `import` directives
        foreach (var importDirective in nsSyntax.Imports)
        {
            var importedNamespace = _compilation.GlobalNamespace.LookupNamespace(importDirective.NamespaceOrType.ToString());
            if (importedNamespace != null)
                nsBinder.AddUsingDirective((INamespaceSymbol)importedNamespace);
        }

        return nsBinder;
    }

    public IEnumerable<Binder> GetAllBinders()
    {
        return _cache.Values.Distinct();
    }

    public IEnumerable<Binder> GetAllBinders(SyntaxTree syntaxTree)
    {
        return _cache
            .Where(x => x.Key.SyntaxTree == syntaxTree)
            .Select(x => x.Value)
            .Distinct();
    }
}
