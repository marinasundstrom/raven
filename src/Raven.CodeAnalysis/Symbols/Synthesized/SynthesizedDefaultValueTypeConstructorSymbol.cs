using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

/// <summary>The zero-initializing constructor of a metadata value type without a declared parameterless constructor.</summary>
internal sealed class SynthesizedDefaultValueTypeConstructorSymbol : Symbol, IMethodSymbol
{
    private readonly Compilation _compilation;

    internal SynthesizedDefaultValueTypeConstructorSymbol(INamedTypeSymbol type, Compilation compilation)
        : base(SymbolKind.Method, ".ctor", type, type, type.ContainingNamespace,
            type.Locations.ToArray(), [], Accessibility.Public)
    {
        _compilation = compilation;
    }

    public override IAssemblySymbol? ContainingAssembly => ContainingType?.ContainingAssembly;
    public override IModuleSymbol? ContainingModule => ContainingType?.ContainingModule;
    public override bool IsImplicitlyDeclared => true;
    public MethodKind MethodKind => MethodKind.Constructor;
    public ITypeSymbol ReturnType => _compilation.GetSpecialType(SpecialType.System_Unit);
    public ImmutableArray<IParameterSymbol> Parameters => [];
    public IMethodSymbol? OriginalDefinition => this;
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
    public ITypeSymbol? IteratorElementType => null;
    public ImmutableArray<IMethodSymbol> ExplicitInterfaceImplementations => [];
    public ImmutableArray<ITypeParameterSymbol> TypeParameters => [];
    public ImmutableArray<ITypeSymbol> TypeArguments => [];
    public IMethodSymbol? ConstructedFrom => this;
    public bool SetsRequiredMembers => false;
    public ImmutableArray<AttributeData> GetReturnTypeAttributes() => [];

    public IMethodSymbol Construct(params ITypeSymbol[] typeArguments)
    {
        if (typeArguments.Length != 0)
            throw new InvalidOperationException("Default value-type constructors are not generic methods.");
        return this;
    }

    public override void Accept(SymbolVisitor visitor) => visitor.VisitMethod(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitMethod(this);
}
