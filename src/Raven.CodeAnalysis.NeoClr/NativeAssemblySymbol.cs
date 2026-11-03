using System.Collections.Immutable;
using System.Collections.Concurrent;

using NeoCLR.Metadata.Experimental.Model;
using NeoCLR.Metadata.Experimental.Introspection;

using Raven.CodeAnalysis.Metadata;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.NeoClr;

internal sealed class NativeAssemblySymbol : SymbolBase, ImportedAssemblySymbol
{
    private readonly Lazy<ImmutableArray<AttributeData>> attributes;
    public override ImmutableArray<AttributeData> GetAttributes() => attributes.Value;
    internal NativeAssemblySymbol(Compilation compilation, NeoClrMetadataReference reference)
        : base(SymbolKind.Assembly, reference.Definition.Name, null, null, null, [], [])
    {
        Reference = reference;
        Module = new NativeModuleSymbol(compilation, this);
        attributes = new(() => Module.MapAttributes(NativeMetadataContext.For(compilation).Resolve(reference.Definition.Identity).GetCustomAttributes()));
    }
    internal NeoClrMetadataReference Reference { get; }
    internal NativeModuleSymbol Module { get; }
    public object DefinitionIdentity => Reference.Definition.Identity;
    public ResolvedAssemblyArtifact ResolvedArtifact => Reference.Artifact;
    public NamespaceSymbol GlobalNamespace => Module.GlobalNamespace;
    public IEnumerable<ModuleSymbol> Modules => [Module];
    public NamedTypeSymbol? GetTypeByMetadataName(string name) => Module.Types.SingleOrDefault(t => ((TypeSymbol)t).ToFullyQualifiedMetadataName() == name);
    public NamedTypeSymbol? GetTypeBySimpleName(string name, int arity) => Module.Types.Where(t => t.ContainingType is null && t.Name == name && t.Arity == arity).Take(2).ToArray() is [var only] ? only : null;
    public ImmutableArray<NamedTypeSymbol> GetExtensionConversionContainers() => [];
    public override void Accept(SymbolVisitor visitor) => visitor.VisitAssembly(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitAssembly(this);
}

internal sealed class NativeModuleSymbol : SymbolBase, ModuleSymbol
{
    private readonly Compilation compilation;
    private readonly NativeAssemblySymbol assembly;
    private readonly Dictionary<uint, NativeNamedTypeSymbol> typeSymbols;
    private readonly ConcurrentDictionary<NeoCLR.Metadata.Experimental.Introspection.TypeInfo, TypeSymbol> viewSymbols = new();
    internal NativeModuleSymbol(Compilation compilation, NativeAssemblySymbol assembly)
        : base(SymbolKind.Module, assembly.Reference.Definition.MainModule.Name, assembly, null, null, [], [])
    {
        this.compilation = compilation; this.assembly = assembly;
        var metadata = NativeMetadataContext.For(compilation);
        var assemblyView = metadata.Resolve(assembly.Reference.Definition.Identity);
        var root = new NativeNamespaceSymbol("", this, null);
        GlobalNamespace = root;
        typeSymbols = [];
        var definitions = assemblyView.GetTypes().ToDictionary(type => type.MetadataToken);
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
                        ? new NativeUnionCompanionSymbol(compilation, type, ns, parent, () => (UnionSymbol)typeSymbols[unionToken])
                        : new NativeNamedTypeSymbol(compilation, type, ns, parent);
            typeSymbols.Add(type.MetadataToken, symbol);
            if (parent is null) ns.Add(symbol); else parent.AddNestedType(symbol);
            resolving.Remove(type.MetadataToken);
            return symbol;
        }
        foreach (var pair in unionContracts.Unions)
        {
            var union = (NativeUnionSymbol)typeSymbols[pair.Key];
            union.SetCases(pair.Value.Select(item => (UnionCaseTypeSymbol)typeSymbols[item.Token]));
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
        foreach (var module in assembly.Reference.Definition.GetModules())
            _ = Namespace(module.Name);
        foreach (var constant in assembly.Reference.Definition.MainModule.Constants)
        {
            var ns = Namespace(constant.Namespace);
            ns.Add(new NativeAssemblyConstantSymbol(constant, ns, () => compilation.GetSpecialType(SpecialType.System_Double)));
        }
        foreach (var method in assemblyView.GetModules().SelectMany(module => module.GetFunctions()))
        {
            var ns = root;
            foreach (var part in method.Namespace.Split('.', StringSplitOptions.RemoveEmptyEntries))
                ns = ns.GetOrAddNamespace(part);
            ns.Add(new NativeMethodSymbol(compilation, method, ns));
        }
    }
    internal ImmutableArray<NativeNamedTypeSymbol> Types { get; }
    private NamedTypeSymbol Resolve(NominalTypeInfo view)
    {
        if (view.Module.Assembly.Identity.Equals(assembly.Reference.Definition.Identity))
            return typeSymbols[view.MetadataToken];
        if (assembly.Reference.Bootstrap is { } bootstrap && bootstrap.Definition.Identity.Equals(view.Module.Assembly.Identity))
            return ((AssemblySymbol)compilation.GetAssemblyOrModuleSymbol(bootstrap.Reference)!).GetTypeByMetadataName(view.FullName)
                ?? throw new InvalidDataException("primitive bootstrap semantic type missing: " + view.FullName);
        var input = compilation.References.OfType<NeoClrMetadataReference>().Single(r => r.Definition.Identity.Equals(view.Module.Assembly.Identity));
        var external = (NativeAssemblySymbol)compilation.GetAssemblyOrModuleSymbol(input)!;
        return external.Module.typeSymbols[view.MetadataToken];
    }
    private readonly Dictionary<uint, NativeMethodSymbol> methodSymbols = [];
    internal void RegisterMethod(uint token, NativeMethodSymbol symbol) => methodSymbols.Add(token, symbol);
    internal NativeMethodSymbol GetMethodSymbol(uint token) => methodSymbols[token];
    private TypeSymbol MapMethodParameter(MethodGenericParameterTypeInfo parameter)
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
    internal ImmutableArray<AttributeData> MapAttributes(IReadOnlyList<CustomAttributeInfo> attributes)
        => NativeAttributeData.Read(compilation, this, attributes);
    internal TypeSymbol MapView(NeoCLR.Metadata.Experimental.Introspection.TypeInfo view) => viewSymbols.GetOrAdd(view, MapViewCore);
    internal TypeSymbol MapAnnotatedView(NeoCLR.Metadata.Experimental.Introspection.TypeInfo view, NullableAnnotation? annotation)
    {
        var type = MapView(view);
        if (annotation is null) return type;
        int Positions(TypeSymbol symbol)
        {
            if (symbol.IsValueType && symbol is not NamedTypeSymbol { TypeArguments.Length: > 0 }) return 0;
            return 1 + (symbol switch
            {
                StructuralFunctionTypeSymbol function => function.ParameterTypes.Sum(Positions) + Positions(function.ReturnType),
                ArrayTypeSymbol array => Positions(array.ElementType),
                NamedTypeSymbol named => named.TypeArguments.Sum(Positions),
                _ => 0
            });
        }
        // Older native producers treated function signatures as opaque nullable nodes.
        // Keep their outer annotation usable; missing nested facts cannot be recovered.
        bool expandFunctions = annotation.IsUniform || Positions(type) == annotation.Flags.Count;
        int position = 0;
        byte Next()
        {
            if (annotation.IsUniform) return annotation.Flags[0];
            if (position >= annotation.Flags.Count) throw new InvalidDataException("truncated native nullable transform");
            return annotation.Flags[position++];
        }
        TypeSymbol Apply(TypeSymbol symbol)
        {
            bool value = symbol.IsValueType;
            if (value && symbol is not NamedTypeSymbol { TypeArguments.Length: > 0 }) return symbol;
            byte flag = Next();
            TypeSymbol result = symbol;
            if (symbol is StructuralFunctionTypeSymbol function)
                result = expandFunctions ? function.Substitute(Apply) : function;
            else if (symbol is ArrayTypeSymbol array)
                result = compilation.CreateArrayTypeSymbol(Apply(array.ElementType), array.Rank);
            else if (symbol is NamedTypeSymbol { TypeArguments.Length: > 0 } named)
                result = ((NamedTypeSymbol)named.OriginalDefinition).Construct(named.TypeArguments.Select(Apply).ToArray());
            return !value && flag == 2 ? result.GetNullableType() : result;
        }
        var annotated = Apply(type);
        if (!annotation.IsUniform && position != annotation.Flags.Count) throw new InvalidDataException("excess native nullable transform flags");
        return annotated;
    }
    private TypeSymbol MapViewCore(NeoCLR.Metadata.Experimental.Introspection.TypeInfo view) => view switch
    {
        TupleTypeInfo tuple => compilation.CreateTupleTypeSymbol(tuple.ElementTypes.Select((t, i) => (tuple.ElementNames[i], MapView(t)))),
        SelfTypeInfo => compilation.ResolveRuntimeSelfType()
            ?? throw new InvalidDataException("native Self signatures require an explicit runtime Self contract"),
        ReferenceCompositionTypeInfo { Kind: ReferenceCompositionKind.Intersection } composition =>
            compilation.CreateIntersectionTypeSymbol(composition.Constituents.Select(MapView).ToArray()),
        ReferenceCompositionTypeInfo { Kind: ReferenceCompositionKind.Union } composition =>
            compilation.CreateStructuralUnionTypeSymbol(composition.Constituents.Select(MapView).ToArray()),
        NominalTypeInfo nominal => Resolve(nominal),
        ConstructedTypeInfo constructed => Resolve(constructed.Definition).Construct(constructed.TypeArguments.Select(MapView).ToArray()),
        FunctionReturnTypeInfo projection => new FunctionReturnTypeSymbolImplementation(MapView(projection.FunctionType), compilation.GetSpecialType(SpecialType.System_Unit)),
        FunctionTypeInfo { NoResult: true } function => compilation.CreateNoResultFunctionTypeSymbol(function.ParameterTypes.Select(MapView).ToArray()),
        FunctionTypeInfo function => compilation.CreateFunctionTypeSymbol(function.ParameterTypes.Select(MapView).ToArray(), MapView(function.ReturnType)),
        PointerTypeInfo pointer => compilation.CreatePointerTypeSymbol(pointer.ElementType is PrimitiveTypeInfo { Kind: PrimitiveType.Void }
            ? compilation.GetSpecialType(SpecialType.System_Void) : MapView(pointer.ElementType)),
        ArrayTypeInfo array => compilation.CreateArrayTypeSymbol(MapView(array.ElementType)),
        GenericParameterTypeInfo parameter => Resolve(parameter.DeclaringType).TypeParameters[parameter.Position],
        MethodGenericParameterTypeInfo parameter => MapMethodParameter(parameter),
        PrimitiveTypeInfo primitive => compilation.GetSpecialType(primitive.Kind switch
        {
            PrimitiveType.Int32 => SpecialType.System_Int32,
            PrimitiveType.Byte => SpecialType.System_Byte,
            PrimitiveType.SByte => SpecialType.System_SByte,
            PrimitiveType.Int16 => SpecialType.System_Int16,
            PrimitiveType.UInt16 => SpecialType.System_UInt16,
            PrimitiveType.UInt32 => SpecialType.System_UInt32,
            PrimitiveType.UInt64 => SpecialType.System_UInt64,
            PrimitiveType.IntPtr => SpecialType.System_IntPtr,
            PrimitiveType.UIntPtr => SpecialType.System_UIntPtr,
            PrimitiveType.RuntimeTypeHandle => SpecialType.System_RuntimeTypeHandle,
            PrimitiveType.Int64 => SpecialType.System_Int64,
            PrimitiveType.Single => SpecialType.System_Single,
            PrimitiveType.Double => SpecialType.System_Double,
            PrimitiveType.Boolean => SpecialType.System_Boolean,
            PrimitiveType.String => SpecialType.System_String,
            PrimitiveType.Void => SpecialType.System_Unit,
            _ => throw new InvalidDataException("unsupported native primitive")
        }),
        _ => throw new InvalidDataException("unsupported metadata type view")
    };
    public override AssemblySymbol ContainingAssembly => assembly;
    public override ModuleSymbol ContainingModule => this;
    public NamespaceSymbol GlobalNamespace { get; }
    // Resolve only after this assembly's identities have been published in Compilation.
    public ImmutableArray<AssemblySymbol> ReferencedAssemblySymbols => [.. assembly.Reference.Definition.MainModule.AssemblyReferences.Select(dependency =>
        (AssemblySymbol)compilation.GetAssemblyOrModuleSymbol(compilation.References.OfType<NeoClrMetadataReference>().Single(r => r.Definition.Identity.Equals(dependency.Identity)))!)];
    public NamespaceSymbol? GetModuleNamespace(NamespaceSymbol ns) => ModuleNamespaceResolver.Resolve(this, ns);
    public override void Accept(SymbolVisitor visitor) => visitor.VisitModule(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitModule(this);
}

internal sealed class NativeNamespaceSymbol : SymbolBase, NamespaceSymbol, INamespaceExtensionLookup
{
    public override Raven.CodeAnalysis.Documentation.DocumentationComment? GetDocumentationComment() => NativeDocumentation.Get(this);

    private readonly List<Symbol> members = [];
    internal NativeNamespaceSymbol(string name, Symbol owner, NativeNamespaceSymbol? parent)
        : base(SymbolKind.Namespace, name, owner, null, parent, [], []) { }
    internal void Add(Symbol member) => members.Add(member);
    public ImmutableArray<NamedTypeSymbol> GetExtensionMethodContainers(string methodName) =>
        [.. members.OfType<NativeNamedTypeSymbol>().Where(type => type.IsExtensionContainer &&
            type.GetMembers(methodName).OfType<MethodSymbol>().Any(method => method.IsExtensionMethod))];

    internal NativeNamespaceSymbol GetOrAddNamespace(string name)
    {
        if (LookupNamespace(name) is NativeNamespaceSymbol found) return found;
        var child = new NativeNamespaceSymbol(name, this, this); Add(child); return child;
    }
    public override ModuleSymbol ContainingModule => ContainingSymbol!.ContainingModule!;
    public override AssemblySymbol ContainingAssembly => ContainingSymbol!.ContainingAssembly!;
    public bool IsNamespace => true;
    public bool IsType => false;
    public bool IsGlobalNamespace => ContainingNamespace is null;
    public bool IsModule => true;
    public ImmutableArray<Symbol> GetMembers() => [.. members];
    public ImmutableArray<Symbol> GetMembers(string name) => [.. members.Where(m => m.Name == name)];
    public NamespaceSymbol? LookupNamespace(string name) => members.OfType<NamespaceSymbol>().SingleOrDefault(n => n.Name == name);
    public TypeSymbol? LookupType(string name)
    {
        var matches = members.OfType<NamedTypeSymbol>().Where(t => t.Name == name).ToArray();
        return matches.Length == 1 ? matches[0] : matches.SingleOrDefault(t => t.Arity == 0);
    }
    public bool IsMemberDefined(string name, out Symbol? symbol) { symbol = members.FirstOrDefault(m => m.Name == name); return symbol is not null; }
    public string ToMetadataName() => IsGlobalNamespace ? "" : ContainingNamespace!.IsGlobalNamespace ? Name : ContainingNamespace.ToMetadataName() + "." + Name;
    public override void Accept(SymbolVisitor visitor) => visitor.VisitNamespace(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitNamespace(this);
}

internal sealed class NativeMethodSymbol : SymbolBase, MethodSymbol
{
    internal override bool IsTerminalRuntimeCall => RuntimeFailureContract.IsTerminal(this, compilation.Options);
    public override Raven.CodeAnalysis.Documentation.DocumentationComment? GetDocumentationComment() => NativeDocumentation.Get(this);

    private readonly Compilation compilation;
    private readonly NeoCLR.Metadata.Experimental.Introspection.MethodInfo view;
    internal uint NativeMetadataToken => view.MetadataToken;
    internal bool HasExplicitInterfaceMappings
    {
        get
        {
            var module = ((NativeAssemblySymbol)ContainingAssembly).Reference.Definition.MainModule;
            return module.Name != view.MetadataScopeName ||
                module.GetMethodDefinition(view.MetadataToken)?.ExplicitInterfaceImplementations.Count != 0;
        }
    }
    private readonly Lazy<ImmutableArray<ParameterSymbol>> parameters;
    private readonly Lazy<TypeSymbol> returnType;
    private readonly Lazy<TypeSymbol?> declaredFunctionType;
    private readonly Lazy<ImmutableArray<AttributeData>> attributes;
    private readonly Lazy<ImmutableArray<AttributeData>> returnAttributes;
    public override ImmutableArray<AttributeData> GetAttributes() => attributes.Value;

    internal NativeMethodSymbol(Compilation compilation, NeoCLR.Metadata.Experimental.Introspection.MethodInfo methodView, Symbol owner)
        : base(SymbolKind.Method, methodView.Name, owner, owner as NamedTypeSymbol, owner as NamespaceSymbol ?? owner.ContainingNamespace, [], [],
            NativeMetadataAccess.Map(methodView.Accessibility))
    {
        this.compilation = compilation;
        view = methodView;
        IsStatic = view.IsStatic;
        IsAbstract = view.IsAbstract;
        IsVirtual = view.IsVirtual;
        var module = (NativeModuleSymbol)ContainingModule;
        TypeParameters = [.. view.GenericParameterNames.Select((name, i) => (TypeParameterSymbol)new NativeTypeParameterSymbol(compilation, name, i, this,
            () => [.. ((NeoCLR.Metadata.Experimental.Introspection.MethodGenericParameterTypeInfo)view.GetGenericArguments()[i])
                .GetTypeConstraints().Select(module.MapView)],
            specialConstraints: NativeConstraintMapping.ToSymbol(((MethodGenericParameterTypeInfo)view.GetGenericArguments()[i]).SpecialConstraints),
            getAttributes: () => module.MapAttributes(((MethodGenericParameterTypeInfo)view.GetGenericArguments()[i]).GetCustomAttributes())))];
        TypeArguments = [.. TypeParameters];
        module.RegisterMethod(view.MetadataToken, this);
        attributes = new(() => module.MapAttributes(view.GetCustomAttributes()));
        returnAttributes = new(() => module.MapAttributes(view.GetReturnCustomAttributes()));
        declaredFunctionType = new(() => view.FunctionType is FunctionTypeInfo ? null : module.MapView(view.FunctionType));
        returnType = new(() => DeclaredFunctionType is { } operand ? new FunctionReturnTypeSymbolImplementation(operand, compilation.GetSpecialType(SpecialType.System_Unit)) : MethodKind == MethodKind.Constructor ? compilation.GetSpecialType(SpecialType.System_Void) : module.MapAnnotatedView(view.ReturnType, view.ReturnNullableAnnotation));
        parameters = new(() => DeclaredFunctionType is not null ? [] : [.. view.GetParameters().Select(p => (ParameterSymbol)new NativeParameterSymbol(p.Position, p.Name, module.MapAnnotatedView(p.ParameterType, p.NullableAnnotation), this,
            p.PassingMode switch
            {
                NeoCLR.Metadata.Experimental.Introspection.ParameterPassingMode.Value => RefKind.None,
                NeoCLR.Metadata.Experimental.Introspection.ParameterPassingMode.Ref => RefKind.Ref,
                NeoCLR.Metadata.Experimental.Introspection.ParameterPassingMode.Out => RefKind.Out,
                _ => throw new InvalidDataException("unsupported native parameter passing mode")
            }, p.IsParameterArray, () => module.MapAttributes(p.GetCustomAttributes()), p.HasDefaultValue, p.DefaultValue)) ]);
    }
    private PropertySymbol? property;
    internal void Associate(PropertySymbol value)
    {
        if (property is not null) throw new InvalidDataException("native accessor is associated with more than one property");
        property = value;
    }
    public Symbol? AssociatedSymbol => property;
    public override ModuleSymbol ContainingModule => ContainingNamespace!.ContainingModule!;
    public override AssemblySymbol ContainingAssembly => ContainingNamespace!.ContainingAssembly!;
    public override bool IsStatic { get; }
    public MethodKind MethodKind => view.IsConstructor ? MethodKind.Constructor : view.IsStaticConstructor ? MethodKind.StaticConstructor : property is null ? Name is "op_Implicit" or "op_Explicit" or "op_CheckedExplicit" ? MethodKind.Conversion
        : Name.StartsWith("op_", StringComparison.Ordinal) ? MethodKind.UserDefinedOperator : MethodKind.Ordinary
        : ReferenceEquals(property.GetMethod, this) ? MethodKind.PropertyGet : property is NativePropertySymbol { IsInitOnly: true } ? MethodKind.InitOnly : MethodKind.PropertySet;
    public TypeSymbol? DeclaredFunctionType => declaredFunctionType.Value;
    public TypeSymbol ReturnType => returnType.Value;
    public ImmutableArray<ParameterSymbol> Parameters => parameters.Value;
    public ImmutableArray<AttributeData> GetReturnTypeAttributes() => returnAttributes.Value;
    public MethodSymbol OriginalDefinition => this;
    public MethodSymbol ConstructedFrom => this;
    public ImmutableArray<TypeParameterSymbol> TypeParameters { get; }
    public ImmutableArray<TypeSymbol> TypeArguments { get; }
    public ImmutableArray<MethodSymbol> ExplicitInterfaceImplementations => [];
    public MethodSymbol Construct(params TypeSymbol[] types) => types.Length == 0 && TypeParameters.IsEmpty ? this : new ConstructedMethodSymbol(this, [.. types]);
    public bool IsAbstract { get; }
    public bool IsAsync => false;
    public bool IsCheckedBuiltin => false;
    public bool IsDefinition => true;
    public bool IsExtensionMethod => IsStatic && (MethodKind == MethodKind.Ordinary || property is { IsExtensionProperty: true }) &&
        ContainingType is NativeNamedTypeSymbol { IsExtensionContainer: true } && !Parameters.IsEmpty;
    internal bool IsRuntimeInternalCall => (view.GetMethodImplementationFlags() & System.Reflection.MethodImplAttributes.InternalCall) != 0;
    public bool IsExtern => view.NativeImport is not null || IsRuntimeInternalCall;
    public bool IsUnsafe => false;
    public bool IsGenericMethod => !TypeParameters.IsEmpty;
    public bool IsOverride => view.IsVirtual && !view.IsNewSlot;
    public bool IsReadOnly => false;
    public bool IsFinal => false;
    public bool IsVirtual { get; }
    public bool IsIterator => false;
    public IteratorMethodKind IteratorKind => IteratorMethodKind.None;
    public TypeSymbol? IteratorElementType => null;
    public bool SetsRequiredMembers => false;
    public override void Accept(SymbolVisitor visitor) => visitor.VisitMethod(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitMethod(this);
}

internal sealed class NativeParameterSymbol : SymbolBase, ParameterSymbol
{
    internal NativeParameterSymbol(int ordinal, string? name, TypeSymbol type, NativeMethodSymbol method, RefKind refKind, bool isParameterArray, Func<ImmutableArray<AttributeData>> readAttributes, bool hasDefaultValue, object? defaultValue)
        : base(SymbolKind.Parameter, name ?? "$arg" + ordinal, method, null, method.ContainingNamespace, [], []) { Type = type; RefKind = refKind; HasImplicitName = name is null; IsVarParams = isParameterArray; attributes = new(readAttributes); HasExplicitDefaultValue = hasDefaultValue; ExplicitDefaultValue = defaultValue; }
    private readonly Lazy<ImmutableArray<AttributeData>> attributes;
    public override ImmutableArray<AttributeData> GetAttributes() => attributes.Value;
    public TypeSymbol Type { get; }
    public bool HasImplicitName { get; }
    public bool IsVarParams { get; }
    public RefKind RefKind { get; }
    public bool IsMutable => false;
    public bool HasExplicitDefaultValue { get; }
    public object? ExplicitDefaultValue { get; }
    public override ModuleSymbol ContainingModule => ContainingSymbol!.ContainingModule!;
    public override AssemblySymbol ContainingAssembly => ContainingSymbol!.ContainingAssembly!;
    public override void Accept(SymbolVisitor visitor) => visitor.VisitParameter(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitParameter(this);
}
