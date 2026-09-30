using System.Linq;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Documentation;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class NativeSelfMethodSymbol : Symbol, IMethodSymbol
{
    internal NativeSelfMethodSymbol(Compilation compilation, ITypeSymbol implementingType, IMethodSymbol adapterMethod)
        : base(
            adapterMethod.ContainingType!,
            adapterMethod.ContainingType,
            adapterMethod.ContainingNamespace,
            [],
            [],
            adapterMethod.DeclaredAccessibility,
            addAsMember: false)
    {
        AdapterMethod = adapterMethod;
        _compilation = compilation;
        ImplementingType = implementingType;
    }

    private readonly Compilation _compilation;
    internal ITypeSymbol ImplementingType { get; }

    internal IMethodSymbol AdapterMethod { get; }

    public override SymbolKind Kind => SymbolKind.Method;
    public override string Name => AdapterMethod.Name;
    public override string MetadataName => AdapterMethod.MetadataName;
    public override IAssemblySymbol? ContainingAssembly => ContainingType?.ContainingAssembly;
    public override IModuleSymbol? ContainingModule => ContainingType?.ContainingModule;
    public override bool IsImplicitlyDeclared => true;
    public override bool IsStatic => AdapterMethod.IsStatic;
    public override ISymbol UnderlyingSymbol => AdapterMethod;
    public override DocumentationComment? GetDocumentationComment() => AdapterMethod.GetDocumentationComment();
    public MethodKind MethodKind => AdapterMethod.MethodKind;
    public ITypeSymbol ReturnType => RuntimeSelfTypes.Substitute(_compilation, AdapterMethod.ReturnType, ImplementingType);
    public ImmutableArray<IParameterSymbol> Parameters => AdapterMethod.Parameters.Select(p => (IParameterSymbol)new SourceParameterSymbol(p.Name,
        RuntimeSelfTypes.Substitute(_compilation, p.Type, ImplementingType), this, ContainingType, ContainingNamespace, [], [], p.RefKind,
        p.HasExplicitDefaultValue, p.ExplicitDefaultValue, isMutable: p.IsMutable, isVarParams: p.IsVarParams)).ToImmutableArray();
    public IMethodSymbol? OriginalDefinition => this;
    public bool IsAbstract => AdapterMethod.IsAbstract;
    public bool IsAsync => false;
    public bool IsCheckedBuiltin => false;
    public bool IsDefinition => true;
    public bool IsExtensionMethod => false;
    public bool IsExtern => false;
    public bool IsUnsafe => AdapterMethod.IsUnsafe;
    public bool IsGenericMethod => false;
    public bool IsOverride => false;
    public bool IsReadOnly => AdapterMethod.IsReadOnly;
    public bool IsFinal => AdapterMethod.IsFinal;
    public bool IsVirtual => AdapterMethod.IsVirtual;
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
            throw new InvalidOperationException("Native Self contract methods cannot have independent method type parameters.");

        return this;
    }

    public override void Accept(SymbolVisitor visitor) => visitor.VisitMethod(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitMethod(this);
}
