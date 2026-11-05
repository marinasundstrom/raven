using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

/// <summary>The zero-initializing constructor of a value type without a declared parameterless constructor.</summary>
internal sealed class SynthesizedDefaultValueTypeConstructorSymbol : SymbolBase, MethodSymbol
{
    private readonly Compilation _compilation;

    internal SynthesizedDefaultValueTypeConstructorSymbol(NamedTypeSymbol type, Compilation compilation)
        : base(SymbolKind.Method, ".ctor", type, type, type.ContainingNamespace,
            type.Locations.ToArray(), [], Accessibility.Public)
    {
        _compilation = compilation;
    }

    public override AssemblySymbol? ContainingAssembly => ContainingType?.ContainingAssembly;
    public override ModuleSymbol? ContainingModule => ContainingType?.ContainingModule;
    public override bool IsImplicitlyDeclared => true;
    public MethodKind MethodKind => MethodKind.Constructor;
    public TypeSymbol ReturnType => _compilation.GetSpecialType(SpecialType.System_Unit);
    public ImmutableArray<ParameterSymbol> Parameters => [];
    public MethodSymbol? OriginalDefinition => this;
    public bool IsAbstract => false;
    public bool IsAsync => false;
    public bool IsCheckedBuiltin => false;
    public bool IsDefinition => true;
    public bool IsExtensionMethod => false;
    public bool IsExtern => false;
    public bool IsUnsafe => false;
    public bool IsGenericMethod => false;
    public bool IsOverride => false;
    public bool IsReadOnly => false;
    public bool IsFinal => false;
    public bool IsVirtual => false;
    public bool IsIterator => false;
    public IteratorMethodKind IteratorKind => IteratorMethodKind.None;
    public TypeSymbol? IteratorElementType => null;
    public ImmutableArray<MethodSymbol> ExplicitInterfaceImplementations => [];
    public ImmutableArray<TypeParameterSymbol> TypeParameters => [];
    public ImmutableArray<TypeSymbol> TypeArguments => [];
    public MethodSymbol? ConstructedFrom => this;
    public bool SetsRequiredMembers => false;
    public ImmutableArray<AttributeData> GetReturnTypeAttributes() => [];

    public MethodSymbol Construct(params TypeSymbol[] typeArguments)
    {
        if (typeArguments.Length != 0)
            throw new InvalidOperationException("Default value-type constructors are not generic methods.");
        return this;
    }

    public override void Accept(SymbolVisitor visitor) => visitor.VisitMethod(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitMethod(this);
}
