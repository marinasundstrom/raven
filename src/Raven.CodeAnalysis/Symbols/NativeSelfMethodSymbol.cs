using System.Linq;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Documentation;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class NativeSelfMethodSymbol : SymbolBase, MethodSymbol
{
    internal NativeSelfMethodSymbol(Compilation compilation, TypeSymbol implementingType, MethodSymbol adapterMethod)
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
    internal TypeSymbol ImplementingType { get; }

    internal MethodSymbol AdapterMethod { get; }

    public override SymbolKind Kind => SymbolKind.Method;
    public override string Name => AdapterMethod.Name;
    public override string MetadataName => AdapterMethod.MetadataName;
    public override AssemblySymbol? ContainingAssembly => ContainingType?.ContainingAssembly;
    public override ModuleSymbol? ContainingModule => ContainingType?.ContainingModule;
    public override bool IsImplicitlyDeclared => true;
    public override bool IsStatic => AdapterMethod.IsStatic;
    public override Symbol UnderlyingSymbol => AdapterMethod;
    public override DocumentationComment? GetDocumentationComment() => AdapterMethod.GetDocumentationComment();
    public MethodKind MethodKind => AdapterMethod.MethodKind;
    public TypeSymbol ReturnType => RuntimeSelfTypes.Substitute(_compilation, AdapterMethod.ReturnType, ImplementingType);
    public ImmutableArray<ParameterSymbol> Parameters => AdapterMethod.Parameters.Select(p => (ParameterSymbol)new SourceParameterSymbol(p.Name,
        RuntimeSelfTypes.Substitute(_compilation, p.Type, ImplementingType), this, ContainingType, ContainingNamespace, [], [], p.RefKind,
        p.HasExplicitDefaultValue, p.ExplicitDefaultValue, isMutable: p.IsMutable, isVarParams: p.IsVarParams)).ToImmutableArray();
    public MethodSymbol? OriginalDefinition => this;
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
    public TypeSymbol? IteratorElementType => null;
    public ImmutableArray<MethodSymbol> ExplicitInterfaceImplementations => [];
    public ImmutableArray<TypeParameterSymbol> TypeParameters => [];
    public ImmutableArray<TypeSymbol> TypeArguments => [];
    public MethodSymbol? ConstructedFrom => this;
    public bool SetsRequiredMembers => false;

    public ImmutableArray<AttributeData> GetReturnTypeAttributes() => AdapterMethod.GetReturnTypeAttributes();

    public MethodSymbol Construct(params TypeSymbol[] typeArguments)
    {
        if (typeArguments.Length != 0)
            throw new InvalidOperationException("Native Self contract methods cannot have independent method type parameters.");

        return this;
    }

    public override void Accept(SymbolVisitor visitor) => visitor.VisitMethod(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitMethod(this);
}
