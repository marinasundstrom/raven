using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

sealed class GlobalBinder : Binder
{
    private readonly INamespaceSymbol _currentNamespace;

    public GlobalBinder(Compilation compilation) : base(null!)
    {
        Compilation = compilation;
        _currentNamespace = compilation.GlobalNamespace;
    }

    public override Compilation Compilation { get; }

    public override INamespaceSymbol? CurrentNamespace => _currentNamespace;

    public override ITypeSymbol? LookupType(string name)
    {
        var type = base.LookupType(name); // Look in CurrentNamespace

        if (type != null)
            return type;

        // Fallback to global metadata-based lookup. Semantic-query binder paths
        // suppress source namespace completion and should not initialize all
        // source declarations just to probe a metadata type.
        return Compilation.IsSourceNamespaceLookupDeclarationCompletionSuppressed
            ? Compilation.TryGetMetadataReferenceTypeByMetadataName(name)
            : Compilation.GetTypeByMetadataName(name);
    }
}
