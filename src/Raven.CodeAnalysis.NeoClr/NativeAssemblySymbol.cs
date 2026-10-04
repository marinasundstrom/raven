using System.Collections.Immutable;
using System.Collections.Concurrent;

using NeoCLR.Metadata.Experimental.Model;
using NeoCLR.Metadata.Experimental.Introspection;

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
    public ResolvedAssemblyArtifact ResolvedArtifact => Reference.Artifact;
    public INamespaceSymbol GlobalNamespace => Module.GlobalNamespace;
    public IEnumerable<IModuleSymbol> Modules => [Module];
    public INamedTypeSymbol? GetTypeByMetadataName(string name) => Module.Types.SingleOrDefault(t => ((ITypeSymbol)t).ToFullyQualifiedMetadataName() == name);
    public INamedTypeSymbol? GetTypeBySimpleName(string name, int arity) => Module.Types.Where(t => t.ContainingType is null && t.Name == name && t.Arity == arity).Take(2).ToArray() is [var only] ? only : null;
    public ImmutableArray<INamedTypeSymbol> GetExtensionConversionContainers() => [];
    public override void Accept(SymbolVisitor visitor) => visitor.VisitAssembly(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitAssembly(this);
}

internal sealed class NativeModuleSymbol : Symbol, IModuleSymbol
{
    private readonly Compilation compilation;
    private readonly NativeAssemblySymbol assembly;
    private readonly Dictionary<uint, NativeNamedTypeSymbol> typeSymbols;
    private readonly ConcurrentDictionary<NeoCLR.Metadata.Experimental.Introspection.TypeInfo, ITypeSymbol> viewSymbols = new();
    internal NativeModuleSymbol(Compilation compilation, NativeAssemblySymbol assembly)
        : base(SymbolKind.Module, assembly.Reference.Definition.MainModule.Name, assembly, null, null, [], [])
    {
        this.compilation = compilation; this.assembly = assembly;
        var metadata = NativeMetadataContext.For(compilation);
        var moduleView = metadata.Resolve(assembly.Reference.Definition.Identity).GetModules().Single();
        var root = new NativeNamespaceSymbol("", this, null);
        GlobalNamespace = root;
        typeSymbols = [];
        var definitions = moduleView.GetTypes().ToDictionary(type => type.MetadataToken);
        var unionContracts = new NativeUnionContracts(definitions.Values);
        var resolving = new HashSet<uint>();
        foreach (var type in definitions.Values) AddType(type);
        NativeNamedTypeSymbol AddType(NominalTypeInfo type)
        {
            if (typeSymbols.TryGetValue(type.MetadataToken, out var existing)) return existing;
            if (!resolving.Add(type.MetadataToken)) throw new InvalidDataException("cyclic native type ownership");
            var parent = type.DeclaringType is { } declaring ? AddType(definitions[declaring.MetadataToken]) : null;
            var ns = parent is null ? Namespace(type.Namespace) : (NativeNamespaceSymbol)parent.ContainingNamespace!;
            NativeNamedTypeSymbol symbol = unionContracts.Unions.ContainsKey(type.MetadataToken)
                ? new NativeUnionSymbol(compilation, type, ns, parent)
                : unionContracts.Cases.TryGetValue(type.MetadataToken, out var caseContract)
                    ? new NativeUnionCaseSymbol(compilation, type, ns, (NativeUnionSymbol)AddType(definitions[caseContract.UnionToken]), parent!, caseContract)
                    : unionContracts.Companions.TryGetValue(type.MetadataToken, out var unionToken)
                        ? new NativeUnionCompanionSymbol(compilation, type, ns, parent, () => (IUnionSymbol)typeSymbols[unionToken])
                        : new NativeNamedTypeSymbol(compilation, type, ns, parent);
            typeSymbols.Add(type.MetadataToken, symbol);
            if (parent is null) ns.Add(symbol); else parent.AddNestedType(symbol);
            resolving.Remove(type.MetadataToken);
            return symbol;
        }
        foreach (var pair in unionContracts.Unions)
        {
            var union = (NativeUnionSymbol)typeSymbols[pair.Key];
            union.SetCases(pair.Value.Select(item => (IUnionCaseTypeSymbol)typeSymbols[item.Token]));
            foreach (var item in pair.Value)
                if (!ReferenceEquals(definitions[item.Token].DeclaringType, definitions[pair.Key]))
                    union.AddNestedType(typeSymbols[item.Token]);
        }
        Types = [.. typeSymbols.Values];
        NativeNamespaceSymbol Namespace(string name)
        {
            var ns = root;
            foreach (var part in name.Split('.', StringSplitOptions.RemoveEmptyEntries)) ns = ns.GetOrAddNamespace(part);
            return ns;
        }
        foreach (var method in moduleView.GetFunctions())
        {
            var ns = root;
            foreach (var part in method.Namespace.Split('.', StringSplitOptions.RemoveEmptyEntries))
                ns = ns.GetOrAddNamespace(part);
            ns.Add(new NativeMethodSymbol(compilation, method, ns));
        }
    }
    internal ImmutableArray<NativeNamedTypeSymbol> Types { get; }
    private INamedTypeSymbol Resolve(NominalTypeInfo view)
    {
        if (view.Module.Assembly.Identity.Equals(assembly.Reference.Definition.Identity))
            return typeSymbols[view.MetadataToken];
        if (assembly.Reference.Bootstrap is { } bootstrap && bootstrap.Definition.Identity.Equals(view.Module.Assembly.Identity))
            return ((IAssemblySymbol)compilation.GetAssemblyOrModuleSymbol(bootstrap.Reference)!).GetTypeByMetadataName(view.FullName)
                ?? throw new InvalidDataException("primitive bootstrap semantic type missing: " + view.FullName);
        var input = compilation.References.OfType<NeoClrMetadataReference>().Single(r => r.Definition.Identity.Equals(view.Module.Assembly.Identity));
        var external = (NativeAssemblySymbol)compilation.GetAssemblyOrModuleSymbol(input)!;
        return external.Module.typeSymbols[view.MetadataToken];
    }
    private readonly Dictionary<uint, NativeMethodSymbol> methodSymbols = [];
    internal void RegisterMethod(uint token, NativeMethodSymbol symbol) => methodSymbols.Add(token, symbol);
    internal NativeMethodSymbol GetMethodSymbol(uint token) => methodSymbols[token];
    private ITypeSymbol MapMethodParameter(MethodGenericParameterTypeInfo parameter)
    {
        var view = parameter.DeclaringMethod;
        var module = this;
        if (!view.Module.Assembly.Identity.Equals(assembly.Reference.Definition.Identity))
        {
            var input = compilation.References.OfType<NeoClrMetadataReference>().Single(r => r.Definition.Identity.Equals(view.Module.Assembly.Identity));
            module = ((NativeAssemblySymbol)compilation.GetAssemblyOrModuleSymbol(input)!).Module;
        }
        return module.methodSymbols[view.MetadataToken].TypeParameters[parameter.Position];
    }
    internal ITypeSymbol MapView(NeoCLR.Metadata.Experimental.Introspection.TypeInfo view) => viewSymbols.GetOrAdd(view, MapViewCore);
    private ITypeSymbol MapViewCore(NeoCLR.Metadata.Experimental.Introspection.TypeInfo view) => view switch
    {
        SelfTypeInfo => compilation.ResolveRuntimeSelfType()
            ?? throw new InvalidDataException("native Self signatures require an explicit runtime Self contract"),
        NominalTypeInfo nominal => Resolve(nominal),
        ConstructedTypeInfo constructed => Resolve(constructed.Definition).Construct(constructed.TypeArguments.Select(MapView).ToArray()),
        FunctionTypeInfo { NoResult: true } function => compilation.CreateNoResultFunctionTypeSymbol(function.ParameterTypes.Select(MapView).ToArray()),
        FunctionTypeInfo function => compilation.CreateFunctionTypeSymbol(function.ParameterTypes.Select(MapView).ToArray(), MapView(function.ReturnType)),
        ArrayTypeInfo array => compilation.CreateArrayTypeSymbol(MapView(array.ElementType)),
        GenericParameterTypeInfo parameter => Resolve(parameter.DeclaringType).TypeParameters[parameter.Position],
        MethodGenericParameterTypeInfo parameter => MapMethodParameter(parameter),
        PrimitiveTypeInfo primitive => compilation.GetSpecialType(primitive.Kind switch
        {
            PrimitiveType.Int32 => SpecialType.System_Int32,
            PrimitiveType.Byte => SpecialType.System_Byte,
            PrimitiveType.Int64 => SpecialType.System_Int64,
            PrimitiveType.Boolean => SpecialType.System_Boolean,
            PrimitiveType.String => SpecialType.System_String,
            PrimitiveType.Void => SpecialType.System_Unit,
            _ => throw new InvalidDataException("unsupported native primitive")
        }),
        _ => throw new InvalidDataException("unsupported metadata type view")
    };
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

internal sealed class NativeNamespaceSymbol : Symbol, INamespaceSymbol, INamespaceExtensionLookup
{
    private readonly List<ISymbol> members = [];
    internal NativeNamespaceSymbol(string name, ISymbol owner, NativeNamespaceSymbol? parent)
        : base(SymbolKind.Namespace, name, owner, null, parent, [], []) { }
    internal void Add(ISymbol member) => members.Add(member);
    public ImmutableArray<INamedTypeSymbol> GetExtensionMethodContainers(string methodName) =>
        [.. members.OfType<NativeNamedTypeSymbol>().Where(type => type.IsExtensionContainer &&
            type.GetMembers(methodName).OfType<IMethodSymbol>().Any(method => method.IsExtensionMethod))];

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
    public ITypeSymbol? LookupType(string name)
    {
        var matches = members.OfType<INamedTypeSymbol>().Where(t => t.Name == name).ToArray();
        return matches.Length == 1 ? matches[0] : matches.SingleOrDefault(t => t.Arity == 0);
    }
    public bool IsMemberDefined(string name, out ISymbol? symbol) { symbol = members.FirstOrDefault(m => m.Name == name); return symbol is not null; }
    public string ToMetadataName() => IsGlobalNamespace ? "" : ContainingNamespace!.IsGlobalNamespace ? Name : ContainingNamespace.ToMetadataName() + "." + Name;
    public override void Accept(SymbolVisitor visitor) => visitor.VisitNamespace(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitNamespace(this);
}

internal sealed class NativeMethodSymbol : Symbol, IMethodSymbol
{
    private readonly Compilation compilation;
    private readonly NeoCLR.Metadata.Experimental.Introspection.MethodInfo view;
    private readonly Lazy<ImmutableArray<IParameterSymbol>> parameters;
    private readonly Lazy<ITypeSymbol> returnType;

    internal NativeMethodSymbol(Compilation compilation, NeoCLR.Metadata.Experimental.Introspection.MethodInfo methodView, ISymbol owner)
        : base(SymbolKind.Method, methodView.Name, owner, owner as INamedTypeSymbol, owner as INamespaceSymbol ?? owner.ContainingNamespace, [], [],
            NativeMetadataAccess.Map(methodView.Accessibility))
    {
        this.compilation = compilation;
        view = methodView;
        IsStatic = view.IsStatic;
        IsAbstract = view.IsAbstract;
        IsVirtual = view.IsVirtual;
        var module = (NativeModuleSymbol)ContainingModule;
        TypeParameters = [.. view.GenericParameterNames.Select((name, i) => (ITypeParameterSymbol)new NativeTypeParameterSymbol(name, i, this))];
        TypeArguments = [.. TypeParameters];
        module.RegisterMethod(view.MetadataToken, this);
        returnType = new(() => MethodKind == MethodKind.Constructor ? compilation.GetSpecialType(SpecialType.System_Void) : module.MapView(view.ReturnType));
        parameters = new(() => [.. view.GetParameters().Select(p => (IParameterSymbol)new NativeParameterSymbol(p.Position, p.Name, module.MapView(p.ParameterType), this,
            p.PassingMode switch
            {
                NeoCLR.Metadata.Experimental.Introspection.ParameterPassingMode.Value => RefKind.None,
                NeoCLR.Metadata.Experimental.Introspection.ParameterPassingMode.Ref => RefKind.Ref,
                NeoCLR.Metadata.Experimental.Introspection.ParameterPassingMode.Out => RefKind.Out,
                _ => throw new InvalidDataException("unsupported native parameter passing mode")
            }))]);
    }
    private NativePropertySymbol? property;
    internal void Associate(NativePropertySymbol value)
    {
        if (property is not null) throw new InvalidDataException("native accessor is associated with more than one property");
        property = value;
    }
    public ISymbol? AssociatedSymbol => property;
    public override IModuleSymbol ContainingModule => ContainingNamespace!.ContainingModule!;
    public override IAssemblySymbol ContainingAssembly => ContainingNamespace!.ContainingAssembly!;
    public override bool IsStatic { get; }
    public MethodKind MethodKind => view.IsConstructor ? MethodKind.Constructor : view.IsStaticConstructor ? MethodKind.StaticConstructor : property is null ? MethodKind.Ordinary
        : ReferenceEquals(property.GetMethod, this) ? MethodKind.PropertyGet : MethodKind.PropertySet;
    public ITypeSymbol ReturnType => returnType.Value;
    public ImmutableArray<IParameterSymbol> Parameters => parameters.Value;
    public ImmutableArray<AttributeData> GetReturnTypeAttributes() => [];
    public IMethodSymbol OriginalDefinition => this;
    public IMethodSymbol ConstructedFrom => this;
    public ImmutableArray<ITypeParameterSymbol> TypeParameters { get; }
    public ImmutableArray<ITypeSymbol> TypeArguments { get; }
    public ImmutableArray<IMethodSymbol> ExplicitInterfaceImplementations => [];
    public IMethodSymbol Construct(params ITypeSymbol[] types) => types.Length == 0 && TypeParameters.IsEmpty ? this : new ConstructedMethodSymbol(this, [.. types]);
    public bool IsAbstract { get; }
    public bool IsAsync => false;
    public bool IsCheckedBuiltin => false;
    public bool IsDefinition => true;
    public bool IsExtensionMethod => IsStatic && MethodKind == MethodKind.Ordinary &&
        ContainingType is NativeNamedTypeSymbol { IsExtensionContainer: true } && !Parameters.IsEmpty;
    public bool IsExtern => false;
    public bool IsUnsafe => false;
    public bool IsGenericMethod => !TypeParameters.IsEmpty;
    public bool IsOverride => view.IsVirtual && !view.IsNewSlot;
    public bool IsReadOnly => false;
    public bool IsFinal => false;
    public bool IsVirtual { get; }
    public bool IsIterator => false;
    public IteratorMethodKind IteratorKind => IteratorMethodKind.None;
    public ITypeSymbol? IteratorElementType => null;
    public bool SetsRequiredMembers => false;
    public override void Accept(SymbolVisitor visitor) => visitor.VisitMethod(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitMethod(this);
}

internal sealed class NativeParameterSymbol : Symbol, IParameterSymbol
{
    internal NativeParameterSymbol(int ordinal, string? name, ITypeSymbol type, NativeMethodSymbol method, RefKind refKind)
        : base(SymbolKind.Parameter, name ?? "$arg" + ordinal, method, null, method.ContainingNamespace, [], []) { Type = type; RefKind = refKind; HasImplicitName = name is null; }
    public ITypeSymbol Type { get; }
    public bool HasImplicitName { get; }
    public bool IsVarParams => false;
    public RefKind RefKind { get; }
    public bool IsMutable => false;
    public bool HasExplicitDefaultValue => false;
    public object? ExplicitDefaultValue => null;
    public override IModuleSymbol ContainingModule => ContainingSymbol!.ContainingModule!;
    public override IAssemblySymbol ContainingAssembly => ContainingSymbol!.ContainingAssembly!;
    public override void Accept(SymbolVisitor visitor) => visitor.VisitParameter(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitParameter(this);
}
