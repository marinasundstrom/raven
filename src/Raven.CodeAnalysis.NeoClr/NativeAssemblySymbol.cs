using System.Collections.Immutable;
using NeoCLR.Metadata.Experimental.Model;
using Raven.CodeAnalysis.Metadata;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.NeoClr;

internal sealed class NativeAssemblySymbol : Symbol, IImportedAssemblySymbol
{
    internal NativeAssemblySymbol(Compilation compilation, NeoClrMetadataReference reference)
        : base(SymbolKind.Assembly, reference.Definition.Name, null, null, null, [], [])
    {
        Reference = reference;
        Module = new NativeModuleSymbol(compilation, this);
    }
    internal NeoClrMetadataReference Reference { get; }
    internal NativeModuleSymbol Module { get; }
    public object DefinitionIdentity => Reference.Definition.Identity;
    public INamespaceSymbol GlobalNamespace => Module.GlobalNamespace;
    public IEnumerable<IModuleSymbol> Modules => [Module];
    public INamedTypeSymbol? GetTypeByMetadataName(string name) => null;
    public INamedTypeSymbol? GetTypeBySimpleName(string name, int arity) => null;
    public ImmutableArray<INamedTypeSymbol> GetExtensionConversionContainers() => [];
    public override void Accept(SymbolVisitor visitor) => visitor.VisitAssembly(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitAssembly(this);
}

internal sealed class NativeModuleSymbol : Symbol, IModuleSymbol
{
    private readonly Compilation compilation;
    private readonly NativeAssemblySymbol assembly;
    internal NativeModuleSymbol(Compilation compilation, NativeAssemblySymbol assembly)
        : base(SymbolKind.Module, assembly.Reference.Definition.MainModule.Name, assembly, null, null, [], [])
    {
        this.compilation = compilation; this.assembly = assembly;
        var root = new NativeNamespaceSymbol("", this, null);
        GlobalNamespace = root;
        foreach (var method in assembly.Reference.Definition.MainModule.Functions)
        {
            var ns = root;
            foreach (var part in (method.Namespace ?? "").Split('.', StringSplitOptions.RemoveEmptyEntries))
                ns = ns.GetOrAddNamespace(part);
            ns.Add(new NativeMethodSymbol(compilation, method, ns));
        }
    }
    public override IAssemblySymbol ContainingAssembly => assembly;
    public override IModuleSymbol ContainingModule => this;
    public INamespaceSymbol GlobalNamespace { get; }
    // Resolve only after this assembly's identities have been published in Compilation.
    public ImmutableArray<IAssemblySymbol> ReferencedAssemblySymbols => [.. assembly.Reference.Definition.MainModule.AssemblyReferences.Select(dependency =>
        (IAssemblySymbol)compilation.GetAssemblyOrModuleSymbol(compilation.References.OfType<NeoClrMetadataReference>().Single(r => r.Definition.Identity.Equals(dependency.Identity)))!)];
    public INamespaceSymbol? GetModuleNamespace(INamespaceSymbol ns) => ModuleNamespaceResolver.Resolve(this, ns);
    public override void Accept(SymbolVisitor visitor) => visitor.VisitModule(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitModule(this);
}

internal sealed class NativeNamespaceSymbol : Symbol, INamespaceSymbol
{
    private readonly List<ISymbol> members = [];
    internal NativeNamespaceSymbol(string name, ISymbol owner, NativeNamespaceSymbol? parent)
        : base(SymbolKind.Namespace, name, owner, null, parent, [], []) { }
    internal void Add(ISymbol member) => members.Add(member);
    internal NativeNamespaceSymbol GetOrAddNamespace(string name)
    {
        if (LookupNamespace(name) is NativeNamespaceSymbol found) return found;
        var child = new NativeNamespaceSymbol(name, this, this); Add(child); return child;
    }
    public override IModuleSymbol ContainingModule => ContainingSymbol!.ContainingModule!;
    public override IAssemblySymbol ContainingAssembly => ContainingSymbol!.ContainingAssembly!;
    public bool IsNamespace => true;
    public bool IsType => false;
    public bool IsGlobalNamespace => ContainingNamespace is null;
    public ImmutableArray<ISymbol> GetMembers() => [.. members];
    public ImmutableArray<ISymbol> GetMembers(string name) => [.. members.Where(m => m.Name == name)];
    public INamespaceSymbol? LookupNamespace(string name) => members.OfType<INamespaceSymbol>().SingleOrDefault(n => n.Name == name);
    public ITypeSymbol? LookupType(string name) => null;
    public bool IsMemberDefined(string name, out ISymbol? symbol) { symbol = members.FirstOrDefault(m => m.Name == name); return symbol is not null; }
    public string ToMetadataName() => IsGlobalNamespace ? "" : ContainingNamespace!.IsGlobalNamespace ? Name : ContainingNamespace.ToMetadataName() + "." + Name;
    public override void Accept(SymbolVisitor visitor) => visitor.VisitNamespace(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitNamespace(this);
}

internal sealed class NativeMethodSymbol : Symbol, IMethodSymbol
{
    private readonly Compilation compilation;
    private readonly MethodSignature signature;
    private readonly Lazy<ImmutableArray<IParameterSymbol>> parameters;
    internal NativeMethodSymbol(Compilation compilation, MethodDefinition definition, NativeNamespaceSymbol owner)
        : base(SymbolKind.Method, definition.Name, owner, null, owner, [], [],
            (definition.Attributes & 7) == 6 ? Accessibility.Public : Accessibility.Internal)
    {
        this.compilation = compilation; Definition = definition;
        if (!definition.TryGetSignature(out var decoded)) throw new InvalidDataException("native signature unavailable");
        signature = decoded!;
        parameters = new(() => [.. signature.ParameterTypes.Select((p, i) => (IParameterSymbol)new NativeParameterSymbol(i, Map(p.Primitive!.Value), this))]);
    }
    internal MethodDefinition Definition { get; }
    private ITypeSymbol Map(PrimitiveType type) => compilation.GetSpecialType(type switch {
        PrimitiveType.Int32 => SpecialType.System_Int32, PrimitiveType.Int64 => SpecialType.System_Int64,
        PrimitiveType.Boolean => SpecialType.System_Boolean, PrimitiveType.String => SpecialType.System_String,
        PrimitiveType.Void => SpecialType.System_Unit, _ => throw new InvalidDataException("unsupported native primitive") });
    public override IModuleSymbol ContainingModule => ContainingNamespace!.ContainingModule!;
    public override IAssemblySymbol ContainingAssembly => ContainingNamespace!.ContainingAssembly!;
    public override bool IsStatic => true;
    public MethodKind MethodKind => MethodKind.Ordinary;
    public ITypeSymbol ReturnType => Map(signature.ReturnType.Primitive!.Value);
    public ImmutableArray<IParameterSymbol> Parameters => parameters.Value;
    public ImmutableArray<AttributeData> GetReturnTypeAttributes() => [];
    public IMethodSymbol OriginalDefinition => this;
    public IMethodSymbol ConstructedFrom => this;
    public ImmutableArray<ITypeParameterSymbol> TypeParameters => [];
    public ImmutableArray<ITypeSymbol> TypeArguments => [];
    public ImmutableArray<IMethodSymbol> ExplicitInterfaceImplementations => [];
    public IMethodSymbol Construct(params ITypeSymbol[] types) => types.Length == 0 ? this : throw new ArgumentException("nongeneric native function");
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
    public bool SetsRequiredMembers => false;
    public override void Accept(SymbolVisitor visitor) => visitor.VisitMethod(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitMethod(this);
}

internal sealed class NativeParameterSymbol : Symbol, IParameterSymbol
{
    internal NativeParameterSymbol(int ordinal, ITypeSymbol type, NativeMethodSymbol method)
        : base(SymbolKind.Parameter, "$arg" + ordinal, method, null, method.ContainingNamespace, [], []) => Type = type;
    public ITypeSymbol Type { get; }
    public bool HasImplicitName => true;
    public bool IsVarParams => false;
    public RefKind RefKind => RefKind.None;
    public bool IsMutable => false;
    public bool HasExplicitDefaultValue => false;
    public object? ExplicitDefaultValue => null;
    public override IModuleSymbol ContainingModule => ContainingSymbol!.ContainingModule!;
    public override IAssemblySymbol ContainingAssembly => ContainingSymbol!.ContainingAssembly!;
    public override void Accept(SymbolVisitor visitor) => visitor.VisitParameter(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitParameter(this);
}
