using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class ProjectedMethodSymbol : Symbol, IMethodSymbol
{
    internal ProjectedMethodSymbol(INamedTypeSymbol receiverType, IMethodSymbol adapterMethod)
        : base(
            receiverType,
            receiverType,
            receiverType.ContainingNamespace,
            [],
            [],
            adapterMethod.DeclaredAccessibility,
            addAsMember: false)
    {
        AdapterMethod = adapterMethod;
    }

    internal IMethodSymbol AdapterMethod { get; }

    public override SymbolKind Kind => SymbolKind.Method;
    public override string Name => AdapterMethod.Name;
    public override string MetadataName => AdapterMethod.MetadataName;
    public override IAssemblySymbol ContainingAssembly => ContainingType!.ContainingAssembly;
    public override IModuleSymbol ContainingModule => ContainingType!.ContainingModule;
    public override bool IsImplicitlyDeclared => true;
    public override bool IsStatic => AdapterMethod.ExtensionMemberKind == ExtensionMemberKind.Static;
    public override ISymbol UnderlyingSymbol => AdapterMethod;
    public MethodKind MethodKind => AdapterMethod.MethodKind;
    public ITypeSymbol ReturnType => AdapterMethod.ReturnType;
    public ImmutableArray<IParameterSymbol> Parameters => AdapterMethod.ExtensionMemberKind == ExtensionMemberKind.Instance
        ? AdapterMethod.Parameters.RemoveAt(0)
        : AdapterMethod.Parameters;
    public IMethodSymbol? OriginalDefinition => this;
    public bool IsAbstract => false;
    public bool IsAsync => false;
    public bool IsCheckedBuiltin => false;
    public bool IsDefinition => true;
    public bool IsExtensionMethod => false;
    public bool IsExtern => false;
    public bool IsUnsafe => AdapterMethod.IsUnsafe;
    public bool IsGenericMethod => false;
    public bool IsOverride => false;
    public bool IsReadOnly => false;
    public bool IsFinal => true;
    public bool IsVirtual => false;
    public bool IsIterator => false;
    public IteratorMethodKind IteratorKind => IteratorMethodKind.None;
    public ITypeSymbol? IteratorElementType => null;
    public ImmutableArray<IMethodSymbol> ExplicitInterfaceImplementations => [];
    public ImmutableArray<ITypeParameterSymbol> TypeParameters => [];
    public ImmutableArray<ITypeSymbol> TypeArguments => [];
    public IMethodSymbol? ConstructedFrom => this;
    public bool SetsRequiredMembers => false;

    public ImmutableArray<AttributeData> GetReturnTypeAttributes() => AdapterMethod.GetReturnTypeAttributes();

    public IMethodSymbol Construct(params ITypeSymbol[] typeArguments)
    {
        if (typeArguments.Length != 0)
            throw new InvalidOperationException("Framework projection methods in the standard catalog are not generic.");

        return this;
    }

    public override void Accept(SymbolVisitor visitor) => visitor.VisitMethod(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitMethod(this);
}
