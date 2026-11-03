using System.Collections.Immutable;

using NeoCLR.Metadata.Experimental.Introspection;
using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Metadata;

namespace Raven.CodeAnalysis.NeoClr;

// Native declarations retain their own generic parameter scopes.
// Keep unsupported categories at the reader boundary rather than manufacturing members.
internal partial class NativeNamedTypeSymbol : SymbolBase, NamedTypeSymbol, IArrayTypeProvider
{
    public override Raven.CodeAnalysis.Documentation.DocumentationComment? GetDocumentationComment() => NativeDocumentation.Get(this);
    private readonly Compilation compilation;
    private readonly Lazy<ImmutableArray<Symbol>> extensionMembers;
    private readonly bool isExtensionContainer;
    private readonly NominalTypeInfo view;
    internal MemoryViewKind? MemoryView => view.MemoryView;
    internal bool IsRuntimeCancellationToken => view.IsValueType && view.GenericArity == 0 &&
        view.FullName == "System.Concurrency.CancellationToken" &&
        compilation.Options.MetadataImportOptions?.AsyncAssemblyName == ContainingAssembly.Name;
    public bool IsRefLikeType => view.MemoryView is MemoryViewKind.Slice or MemoryViewKind.ReadOnlySlice;
    private readonly Lazy<ImmutableArray<NamedTypeSymbol>> interfaces;
    private readonly Lazy<ImmutableArray<NamedTypeSymbol>> allInterfaces;
    private ImmutableArray<Symbol> members;
    private readonly Lazy<ImmutableArray<AttributeData>> attributes;
    internal NativeNamedTypeSymbol(Compilation compilation, NominalTypeInfo view, NativeNamespaceSymbol owner,
        NativeNamedTypeSymbol? declaringType = null)
        : base(SymbolKind.Type, view.GenericArity == 0 ? view.Name : view.Name[..view.Name.LastIndexOf('`')], declaringType ?? (Symbol)owner, declaringType, owner, [], [],
            NativeMetadataAccess.Map(view.Accessibility))
    {
        this.compilation = compilation;
        attributes = new(ReadAttributes);
        this.view = view;
        isExtensionContainer = ReadExtensionContainer();
        // Erased Value has native storage semantics, but no corresponding CLR special
        // type. Preserve its declared identity; target contracts select its ownership.
        SpecialType = view.NativeGrapheme ? SpecialType.System_Char :
            view.NativePrimitive is { } primitive && primitive != PrimitiveType.Value
                ? Enum.Parse<SpecialType>("System_" + primitive) : SpecialType.None;
        if (compilation.Options.TargetPlatform == TargetPlatform.NeoCLR &&
            compilation.Options.MetadataImportOptions?.ObjectAssemblyName == ContainingAssembly.Name &&
            view.FullName == "System.Object")
        {
            if (declaringType is not null || view.GenericArity != 0 || view.IsValueType || view.IsInterface ||
                view.IsStatic || !view.IsAbstract || view.Accessibility != MetadataAccessibility.Public ||
                view.BaseType is not null || view.GetFields().Count != 0)
                throw new InvalidDataException("imported Object ownership requires a public abstract fieldless nongeneric root without a base");
            SpecialType = SpecialType.System_Object;
        }
        // The native primitive marker covers storage types, not their nominal base.
        // Classify nominal ValueType, Enum and Array only in the selected native core.
        if (SpecialType == SpecialType.None &&
            compilation.Options.MetadataImportOptions is { UseNativeMetadata: true } imports &&
            imports.CoreAssemblyName == ContainingAssembly.Name &&
            declaringType is null && view.GenericArity == 0 && !view.IsValueType && !view.IsInterface &&
            view.FullName is "System.ValueType" or "System.Enum" or "System.Array")
            SpecialType = view.FullName switch
            {
                "System.Enum" => SpecialType.System_Enum,
                "System.Array" => SpecialType.System_Array,
                _ => SpecialType.System_ValueType
            };
        if (SpecialType == SpecialType.None && compilation.Options.TargetPlatform == TargetPlatform.NeoCLR &&
            compilation.Options.MetadataImportOptions?.AsyncAssemblyName == ContainingAssembly.Name)
            SpecialType = view.FullName switch
            {
                "System.Runtime.CompilerServices.AsyncStateMachine" => SpecialType.System_Runtime_CompilerServices_IAsyncStateMachine,
                "System.Concurrency.Tasks.Task`1" => SpecialType.System_Threading_Tasks_Task_T,
                "System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1" => SpecialType.System_Runtime_CompilerServices_AsyncTaskMethodBuilder_T,
                _ => SpecialType.None
            };
        TypeParameters = [.. view.GetGenericArguments().Cast<GenericParameterTypeInfo>()
            .Select(parameter => (TypeParameterSymbol)new NativeTypeParameterSymbol(compilation, parameter.Name, parameter.Position, this, getConstraints: () => [.. parameter.GetTypeConstraints().Select(((NativeModuleSymbol)ContainingModule).MapView)], variance: parameter.Variance switch
            {
                NeoCLR.Metadata.Experimental.Model.GenericVariance.Covariant => VarianceKind.Out,
                NeoCLR.Metadata.Experimental.Model.GenericVariance.Contravariant => VarianceKind.In,
                _ => VarianceKind.None
            }, specialConstraints: NativeConstraintMapping.ToSymbol(parameter.SpecialConstraints),
                getAttributes: () => ((NativeModuleSymbol)ContainingModule).MapAttributes(parameter.GetCustomAttributes())))];
        TypeArguments = [.. TypeParameters];
        interfaces = new(() =>
        {
            var module = (NativeModuleSymbol)ContainingModule;
            return [.. view.GetDeclaredInterfaces().Select(view => (NamedTypeSymbol)module.MapView(view))];
        });
        allInterfaces = new(() =>
        {
            var module = (NativeModuleSymbol)ContainingModule;
            // The metadata view expands declared interface edges; class inheritance
            // contributes additional interfaces to the compiler's AllInterfaces contract.
            return [.. view.GetInterfaces().Select(view => (NamedTypeSymbol)module.MapView(view))
                .Concat(BaseType?.AllInterfaces ?? [])
                .Distinct<NamedTypeSymbol>(SymbolEqualityComparer.Default)];
        });
        var methods = view.GetMethods().Concat(view.GetConstructors()).OrderBy(method => method.MetadataToken)
            .Select(method => new NativeMethodSymbol(compilation, method, this)).ToArray();
        var properties = view.GetProperties().Select(property => new NativePropertySymbol(property, this)).ToArray();
        extensionMembers = new(() => BuildExtensionMembers(methods));
        // Accessors remain available through their property and the token map,
        // matching PE import; they are not directly invocable named members.
        // Native instance slots are flattened across the complete base chain.
        // Keep resolution lazy so merely importing a declaration does not force
        // unrelated dependency layouts; static fields remain owner-local.
        var fields = view.GetFields();
        var inheritedFieldCount = new Lazy<int>(() =>
        {
            int count = 0, depth = 0;
            for (var parent = view.BaseType; parent is not null;)
            {
                if (++depth > 64) throw new InvalidDataException("native field base chain exceeds limit");
                var (parentFields, next) = parent switch
                {
                    NominalTypeInfo nominal => (nominal.GetFields(), nominal.BaseType),
                    ConstructedTypeInfo constructed => (constructed.GetFields(), constructed.BaseType),
                    _ => throw new InvalidDataException("native field base must be nominal")
                };
                count = checked(count + parentFields.Count(field => !field.IsStatic));
                parent = next;
            }
            return count;
        });
        members = [.. methods.Where(method => method.AssociatedSymbol is null),
            .. fields.Select((field, ordinal) => (Symbol)new NativeFieldSymbol(field, this,
                () => fields.Take(ordinal).Count(f => f.IsStatic == field.IsStatic) + (field.IsStatic ? 0 : inheritedFieldCount.Value))),
            .. properties];
    }
    public override ImmutableArray<AttributeData> GetAttributes() => attributes.Value;
    private ImmutableArray<AttributeData> ReadAttributes()
    {
        var declared = ((NativeModuleSymbol)ContainingModule).MapAttributes(view.GetCustomAttributes());
        if (!view.IsFlagsEnum) return declared;
        // Flags is a primitive-bootstrap marker, independent of the selected Object owner.
        var coreName = compilation.Options.MetadataImportOptions?.CoreAssemblyName;
        var core = coreName is null
            ? compilation.GetSpecialType(SpecialType.System_Object).ContainingAssembly
            : compilation.ReferencedAssemblySymbols.SingleOrDefault(assembly => assembly.Name == coreName)
                ?? throw new InvalidDataException("The configured primitive core is missing for flags enums.");
        var marker = core.GetTypeByMetadataName("System.FlagsAttribute") as NamedTypeSymbol
            ?? throw new InvalidDataException("The configured core must declare System.FlagsAttribute for flags enums.");
        var constructor = marker.Constructors.SingleOrDefault(m => !m.IsStatic && m.Parameters.IsEmpty && m.DeclaredAccessibility == Accessibility.Public)
            ?? throw new InvalidDataException("The configured core FlagsAttribute requires a public parameterless constructor.");
        return declared.Add(new AttributeData(marker, constructor, [], [], null));
    }
    internal void AddNestedType(NamedTypeSymbol type) => members = members.Add(type);
    internal bool IsExtensionContainer => isExtensionContainer;
    private bool ReadExtensionContainer()
    {
        var markers = view.GetCustomAttributes().Where(a =>
            a.Namespace == "System.Runtime.CompilerServices" && a.Name == "ExtensionAttribute").ToArray();
        if (markers.Length == 0) return false;
        if (markers.Length != 1 || markers[0].GetArguments().Count != 0 || !IsStatic)
            throw new InvalidDataException("invalid native extension container marker");
        return true;
    }
    public override string MetadataName => view.Name;
    public override ModuleSymbol ContainingModule => ContainingNamespace!.ContainingModule!;
    public override AssemblySymbol ContainingAssembly => ContainingNamespace!.ContainingAssembly!;
    public override bool IsStatic => view.IsStatic;
    public bool IsAbstract => view.IsAbstract;
    public bool IsValueType => view.IsValueType;
    public bool IsReferenceType => !IsValueType;
    public bool IsInterface => view.IsInterface;
    public bool IsClosed => view.IsSealed;
    public bool IsSealedHierarchy => view.IsClosedHierarchy;
    public ImmutableArray<NamedTypeSymbol> PermittedDirectSubtypes => [.. view.GetPermittedDirectSubtypes()
        .Select(type => (NamedTypeSymbol)((NativeModuleSymbol)ContainingModule).MapView(type))];
    public bool IsNamespace => false;
    public bool IsType => true;
    public TypeSymbol? EnumUnderlyingType => view.IsEnum ? compilation.GetSpecialType(SpecialType.System_Int32) : null;
    public TypeKind TypeKind => view.IsEnum ? TypeKind.Enum : view.IsInterface ? TypeKind.Interface : view.IsValueType ? TypeKind.Struct : TypeKind.Class;
    public SpecialType SpecialType { get; }
    public NamedTypeSymbol? BaseType => TypeKind == TypeKind.Interface || SpecialType == SpecialType.System_Object ? null : view.BaseType is { } baseType ?
        (NamedTypeSymbol)((NativeModuleSymbol)ContainingModule).MapView(baseType) : compilation.GetSpecialType(view.IsEnum ? SpecialType.System_Enum : view.IsValueType ? SpecialType.System_ValueType : SpecialType.System_Object) as NamedTypeSymbol;
    public TypeSymbol OriginalDefinition => this;
    public TypeSymbol ConstructedFrom => this;
    public int Arity => view.GenericArity;
    public bool IsGenericType => Arity != 0;
    public bool IsUnboundGenericType => false;
    public ImmutableArray<TypeSymbol> TypeArguments { get; }
    public ImmutableArray<TypeParameterSymbol> TypeParameters { get; }
    public ImmutableArray<NamedTypeSymbol> Interfaces => interfaces.Value;
    public ImmutableArray<NamedTypeSymbol> AllInterfaces => allInterfaces.Value;
    public ImmutableArray<MethodSymbol> Constructors => InstanceConstructors;
    public ImmutableArray<MethodSymbol> InstanceConstructors => [.. members.OfType<MethodSymbol>().Where(m => m.MethodKind == MethodKind.Constructor)];
    public MethodSymbol? StaticConstructor => members.OfType<MethodSymbol>().SingleOrDefault(m => m.MethodKind == MethodKind.StaticConstructor);
    public NamedTypeSymbol? UnderlyingTupleType => null;
    public ImmutableArray<FieldSymbol> TupleElements => [];
    public ImmutableArray<Symbol> GetMembers() => IsExtensionContainer ? extensionMembers.Value : members;

    private ImmutableArray<Symbol> BuildExtensionMembers(NativeMethodSymbol[] methods)
    {
        // Read signatures lazily, after the module has registered every type.
        var accessors = methods.Where(m => m.IsStatic && m.AssociatedSymbol is null &&
            (m.Name == "get_Item" && m.Parameters.Length > 1 || m.Name == "set_Item" && m.Parameters.Length > 2));
        var properties = new List<PropertySymbol>();
        foreach (var group in accessors.GroupBy(m =>
            m.TypeParameters.Length + ":" + string.Join(";", (m.Name == "get_Item" ? m.Parameters : m.Parameters.RemoveAt(m.Parameters.Length - 1))
                .Select(p => p.RefKind + ":" + ExtensionSignatureKey(p.Type))) + "->" +
            ExtensionSignatureKey(m.Name == "get_Item" ? m.ReturnType : m.Parameters[^1].Type)))
        {
            var getter = group.SingleOrDefault(m => m.Name == "get_Item");
            var setter = group.SingleOrDefault(m => m.Name == "set_Item");
            var property = new SynthesizedExtensionPropertySymbol(this, [], [], getter, setter, "Item", NativeDocumentation.Get);
            getter?.Associate(property);
            setter?.Associate(property);
            properties.Add(property);
        }
        return [.. members.Where(m => m is not MethodSymbol { AssociatedSymbol: not null }), .. properties];
    }

    private static string ExtensionSignatureKey(TypeSymbol type) => type switch
    {
        TypeParameterSymbol parameter => (parameter.ContainingSymbol is MethodSymbol ? "!!" : "!") + parameter.Ordinal,
        ArrayTypeSymbol array => ExtensionSignatureKey(array.ElementType) + "[" + array.Rank + "]",
        NamedTypeSymbol named => named.ContainingAssembly?.Name + ":" + (named.ContainingType is { } owner ? ExtensionSignatureKey(owner) : named.ContainingNamespace?.ToDisplayString()) + "." + named.MetadataName +
            "<" + string.Join(",", named.TypeArguments.Select(ExtensionSignatureKey)) + ">",
        _ => type.ToDisplayString()
    };
    public ImmutableArray<Symbol> GetMembers(string name) => [.. GetMembers().Where(member => member.Name == name)];
    public TypeSymbol? LookupType(string name) => members.OfType<NamedTypeSymbol>().SingleOrDefault(t => t.Name == name);
    public bool IsMemberDefined(string name, out Symbol? symbol) { symbol = GetMembers().FirstOrDefault(member => member.Name == name); return symbol is not null; }
    public TypeSymbol Construct(params TypeSymbol[] typeArguments) => typeArguments.Length == 0 && Arity == 0 ? this : new ConstructedNamedTypeSymbol(this, [.. typeArguments]);
    public override void Accept(SymbolVisitor visitor) => visitor.VisitNamedType(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitNamedType(this);
}
