using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal sealed partial class SourceLambdaSymbol : SourceSymbol, ILambdaSymbol
{
    public SourceLambdaSymbol(
        IReadOnlyList<IParameterSymbol> parameters,
        ITypeSymbol returnType,
        ISymbol containingSymbol,
        INamedTypeSymbol? containingType = null,
        INamespaceSymbol? containingNamespace = null,
        Location[]? locations = null,
        SyntaxReference[]? declaringSyntaxReferences = null)
        : base(
            SymbolKind.Method,
            "<lambda>",
            containingSymbol,
            containingType,
            containingNamespace,
            locations ?? Array.Empty<Location>(),
            declaringSyntaxReferences ?? Array.Empty<SyntaxReference>())
    {
        Parameters = parameters.ToImmutableArray();
        ReturnType = returnType;
    }

    public ImmutableArray<IParameterSymbol> Parameters { get; }

    public ITypeSymbol ReturnType { get; private set; }

    public bool IsConstructor => false;

    public override bool IsStatic => true;

    public MethodKind MethodKind => MethodKind.LambdaMethod;

    public IMethodSymbol? OriginalDefinition => null;

    public bool IsAbstract => false;
    public bool IsAsync => false;
    public bool IsCheckedBuiltin => false;
    public bool IsDefinition => true;
    public bool IsExtensionMethod => false;
    public bool IsExtern => false;
    public bool IsGenericMethod => false;
    public bool IsOverride => false;
    public bool IsReadOnly => false;
    public bool IsSealed => false;
    public bool IsVirtual => false;

    public void SetReturnType(ITypeSymbol returnType)
    {
        ReturnType = returnType;
    }

    public ITypeSymbol? DelegateType { get; private set; }

    public void SetDelegateType(ITypeSymbol delegateType)
    {
        DelegateType = delegateType;
    }
}