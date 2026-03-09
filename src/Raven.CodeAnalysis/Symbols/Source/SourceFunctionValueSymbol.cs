namespace Raven.CodeAnalysis.Symbols;

internal sealed class SourceFunctionValueSymbol : SourceLocalSymbol
{
    public SourceFunctionValueSymbol(
        string name,
        ITypeSymbol type,
        bool isMutable,
        ISymbol containingSymbol,
        INamedTypeSymbol? containingType,
        INamespaceSymbol? containingNamespace,
        Location[] locations,
        SyntaxReference[] declaringSyntaxReferences,
        IMethodSymbol targetMethod)
        : base(
            name,
            type,
            isMutable,
            containingSymbol,
            containingType,
            containingNamespace,
            locations,
            declaringSyntaxReferences)
    {
        TargetMethod = targetMethod;
    }

    public IMethodSymbol TargetMethod { get; }
}
