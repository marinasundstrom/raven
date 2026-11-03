namespace Raven.CodeAnalysis.Symbols;

internal sealed class SourceFunctionValueSymbol : SourceLocalSymbol
{
    public SourceFunctionValueSymbol(
        string name,
        TypeSymbol type,
        bool isMutable,
        Symbol containingSymbol,
        NamedTypeSymbol? containingType,
        NamespaceSymbol? containingNamespace,
        Location[] locations,
        SyntaxReference[] declaringSyntaxReferences,
        MethodSymbol targetMethod)
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

    public MethodSymbol TargetMethod { get; }
}
