using System.Collections.Immutable;
using System.Collections.Concurrent;

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
    public INamedTypeSymbol? GetTypeByMetadataName(string name) => Module.Types.SingleOrDefault(t => ((ITypeSymbol)t).ToFullyQualifiedMetadataName() == name);
    public INamedTypeSymbol? GetTypeBySimpleName(string name, int arity) => Module.Types.Where(t => t.Name == name && t.Arity == arity).Take(2).ToArray() is [var only] ? only : null;
    public ImmutableArray<INamedTypeSymbol> GetExtensionConversionContainers() => [];
    public override void Accept(SymbolVisitor visitor) => visitor.VisitAssembly(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitAssembly(this);
}

internal sealed class NativeModuleSymbol : Symbol, IModuleSymbol
{
    private readonly Compilation compilation;
    private readonly NativeAssemblySymbol assembly;
    private readonly Dictionary<TypeDefinition, NativeNamedTypeSymbol> typeSymbols;
    private readonly NativeAssemblyResolver resolver;
    private readonly ConcurrentDictionary<SignatureType, ITypeSymbol> signatureTypes = new();
    internal NativeModuleSymbol(Compilation compilation, NativeAssemblySymbol assembly)
        : base(SymbolKind.Module, assembly.Reference.Definition.MainModule.Name, assembly, null, null, [], [])
    {
        this.compilation = compilation; this.assembly = assembly;
        resolver = new(compilation.References.OfType<NeoClrMetadataReference>().Select(r => r.Definition));
        var root = new NativeNamespaceSymbol("", this, null);
        GlobalNamespace = root;
        Types = [.. assembly.Reference.Definition.MainModule.Types.Select(type => {
            var ns = Namespace(type.Namespace);
            var symbol = new NativeNamedTypeSymbol(compilation, type, ns);
            ns.Add(symbol);
            return symbol;
        })];
        typeSymbols = Types.ToDictionary(type => type.Definition);
        NativeNamespaceSymbol Namespace(string name)
        {
            var ns = root;
            foreach (var part in name.Split('.', StringSplitOptions.RemoveEmptyEntries)) ns = ns.GetOrAddNamespace(part);
            return ns;
        }
        foreach (var method in assembly.Reference.Definition.MainModule.Functions)
        {
            var ns = root;
            foreach (var part in (method.Namespace ?? "").Split('.', StringSplitOptions.RemoveEmptyEntries))
                ns = ns.GetOrAddNamespace(part);
            ns.Add(new NativeMethodSymbol(compilation, method, ns));
        }
    }
    internal ImmutableArray<NativeNamedTypeSymbol> Types { get; }
    internal NativeNamedTypeSymbol Resolve(TypeReference reference)
    {
        var definition = reference.Resolve(resolver);
        if (typeSymbols.TryGetValue(definition, out var local)) return local;
        var input = compilation.References.OfType<NeoClrMetadataReference>().Single(r => ReferenceEquals(r.Definition, definition.Module.Assembly));
        var external = (NativeAssemblySymbol)compilation.GetAssemblyOrModuleSymbol(input)!;
        return external.Module.typeSymbols[definition];
    }
    internal ITypeSymbol Map(SignatureType signature) => signatureTypes.GetOrAdd(signature, type =>
        type.ArrayElement is { } element ? compilation.CreateArrayTypeSymbol(Map(element))
        : type.ReferencedType is { } reference ? Resolve(reference)
        : compilation.GetSpecialType(type.Primitive switch
        {
            PrimitiveType.Int32 => SpecialType.System_Int32,
            PrimitiveType.Int64 => SpecialType.System_Int64,
            PrimitiveType.Boolean => SpecialType.System_Boolean,
            PrimitiveType.String => SpecialType.System_String,
            PrimitiveType.Void => SpecialType.System_Unit,
            _ => throw new InvalidDataException("unsupported native signature")
        }));
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
    public ITypeSymbol? LookupType(string name) => members.OfType<ITypeSymbol>().SingleOrDefault(t => t.Name == name);
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
    private readonly Lazy<ITypeSymbol> returnType;
    internal NativeMethodSymbol(Compilation compilation, MethodDefinition definition, ISymbol owner)
        : base(SymbolKind.Method, definition.Name, owner, owner as INamedTypeSymbol, owner as INamespaceSymbol ?? owner.ContainingNamespace, [], [],
            (definition.Attributes & 7) == 6 ? Accessibility.Public : (definition.Attributes & 7) == 3 ? Accessibility.Internal : Accessibility.Private)
    {
        this.compilation = compilation; Definition = definition;
        if (!definition.TryGetSignature(out var decoded)) throw new InvalidDataException("native signature unavailable");
        signature = decoded!;
        returnType = new(() => MethodKind == MethodKind.Constructor ? compilation.GetSpecialType(SpecialType.System_Void) : Map(signature.ReturnType));
        parameters = new(() => [.. signature.ParameterTypes.Select((p, i) => (IParameterSymbol)new NativeParameterSymbol(i, Map(p), this))]);
    }
    private NativePropertySymbol? property;
    internal void Associate(NativePropertySymbol value)
    {
        if (property is not null) throw new InvalidDataException("native accessor is associated with more than one property");
        property = value;
    }
    public ISymbol? AssociatedSymbol => property;
    internal MethodDefinition Definition { get; }
    private ITypeSymbol Map(SignatureType type) => ((NativeModuleSymbol)ContainingModule).Map(type);
    public override IModuleSymbol ContainingModule => ContainingNamespace!.ContainingModule!;
    public override IAssemblySymbol ContainingAssembly => ContainingNamespace!.ContainingAssembly!;
    public override bool IsStatic => Definition.IsStatic;
    public MethodKind MethodKind => Definition.Name == ".ctor" ? MethodKind.Constructor : property is null ? MethodKind.Ordinary
        : ReferenceEquals(property.GetMethod, this) ? MethodKind.PropertyGet : MethodKind.PropertySet;
    public ITypeSymbol ReturnType => returnType.Value;
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
