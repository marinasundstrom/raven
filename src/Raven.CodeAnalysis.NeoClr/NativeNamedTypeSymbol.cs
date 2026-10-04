using System.Collections.Immutable;

using NeoCLR.Metadata.Experimental.Introspection;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.NeoClr;

// Native declarations retain their own generic parameter scopes.
// Keep unsupported categories at the reader boundary rather than manufacturing members.
internal class NativeNamedTypeSymbol : Symbol, INamedTypeSymbol
{
    private readonly Compilation compilation;
    private readonly NominalTypeInfo view;
    private readonly Lazy<ImmutableArray<INamedTypeSymbol>> interfaces;
    private readonly Lazy<ImmutableArray<INamedTypeSymbol>> allInterfaces;
    private ImmutableArray<ISymbol> members;
    internal NativeNamedTypeSymbol(Compilation compilation, NominalTypeInfo view, NativeNamespaceSymbol owner,
        NativeNamedTypeSymbol? declaringType = null)
        : base(SymbolKind.Type, view.GenericArity == 0 ? view.Name : view.Name[..view.Name.LastIndexOf('`')], declaringType ?? (ISymbol)owner, declaringType, owner, [], [],
            NativeMetadataAccess.Map(view.Accessibility))
    {
        this.compilation = compilation;
        this.view = view;
        SpecialType = view.NativePrimitive is { } primitive ? Enum.Parse<SpecialType>("System_" + primitive) : SpecialType.None;
        TypeParameters = [.. view.GetGenericArguments().Cast<GenericParameterTypeInfo>()
            .Select(parameter => (ITypeParameterSymbol)new NativeTypeParameterSymbol(parameter.Name, parameter.Position, this))];
        TypeArguments = [.. TypeParameters];
        interfaces = new(() =>
        {
            var module = (NativeModuleSymbol)ContainingModule;
            return [.. view.GetDeclaredInterfaces().Select(view => (INamedTypeSymbol)module.MapView(view))];
        });
        allInterfaces = new(() =>
        {
            var module = (NativeModuleSymbol)ContainingModule;
            return [.. view.GetInterfaces().Select(view => (INamedTypeSymbol)module.MapView(view))];
        });
        var methods = view.GetMethods().Concat(view.GetConstructors()).OrderBy(method => method.MetadataToken)
            .Select(method => new NativeMethodSymbol(compilation, method, this)).ToArray();
        members = [.. methods,
            .. view.GetFields().Select((field, ordinal) => (ISymbol)new NativeFieldSymbol(field, this, ordinal)),
            .. view.GetProperties().Select(property => (ISymbol)new NativePropertySymbol(property, this))];
    }
    internal void AddNestedType(INamedTypeSymbol type) => members = members.Add(type);
    internal bool IsExtensionContainer
    {
        get
        {
            var markers = view.GetCustomAttributes().Where(a =>
                a.Namespace == "System.Runtime.CompilerServices" && a.Name == "ExtensionAttribute").ToArray();
            if (markers.Length == 0) return false;
            if (markers.Length != 1 || markers[0].GetArguments().Count != 0 || !IsStatic)
                throw new InvalidDataException("invalid native extension container marker");
            return true;
        }
    }
    public override string MetadataName => view.Name;
    public override IModuleSymbol ContainingModule => ContainingNamespace!.ContainingModule!;
    public override IAssemblySymbol ContainingAssembly => ContainingNamespace!.ContainingAssembly!;
    public override bool IsStatic => view.IsStatic;
    public bool IsAbstract => view.IsAbstract;
    public bool IsValueType => view.IsValueType;
    public bool IsReferenceType => !IsValueType;
    public bool IsInterface => view.IsInterface;
    public bool IsClosed => view.IsSealed;
    public bool IsNamespace => false;
    public bool IsType => true;
    public ITypeSymbol? EnumUnderlyingType => view.IsEnum ? compilation.GetSpecialType(SpecialType.System_Int32) : null;
    public TypeKind TypeKind => view.IsEnum ? TypeKind.Enum : view.IsInterface ? TypeKind.Interface : view.IsValueType ? TypeKind.Struct : TypeKind.Class;
    public SpecialType SpecialType { get; }
    public INamedTypeSymbol? BaseType => TypeKind == TypeKind.Interface ? null : compilation.GetSpecialType(view.IsEnum ? SpecialType.System_Enum : view.IsValueType ? SpecialType.System_ValueType : SpecialType.System_Object) as INamedTypeSymbol;
    public ITypeSymbol OriginalDefinition => this;
    public ITypeSymbol ConstructedFrom => this;
    public int Arity => view.GenericArity;
    public bool IsGenericType => Arity != 0;
    public bool IsUnboundGenericType => false;
    public ImmutableArray<ITypeSymbol> TypeArguments { get; }
    public ImmutableArray<ITypeParameterSymbol> TypeParameters { get; }
    public ImmutableArray<INamedTypeSymbol> Interfaces => interfaces.Value;
    public ImmutableArray<INamedTypeSymbol> AllInterfaces => allInterfaces.Value;
    public ImmutableArray<IMethodSymbol> Constructors => InstanceConstructors;
    public ImmutableArray<IMethodSymbol> InstanceConstructors => [.. members.OfType<IMethodSymbol>().Where(m => m.MethodKind == MethodKind.Constructor)];
    public IMethodSymbol? StaticConstructor => null;
    public INamedTypeSymbol? UnderlyingTupleType => null;
    public ImmutableArray<IFieldSymbol> TupleElements => [];
    public ImmutableArray<ISymbol> GetMembers() => members;
    public ImmutableArray<ISymbol> GetMembers(string name) => [.. GetMembers().Where(member => member.Name == name)];
    public ITypeSymbol? LookupType(string name) => members.OfType<INamedTypeSymbol>().SingleOrDefault(t => t.Name == name);
    public bool IsMemberDefined(string name, out ISymbol? symbol) { symbol = GetMembers().FirstOrDefault(member => member.Name == name); return symbol is not null; }
    public ITypeSymbol Construct(params ITypeSymbol[] typeArguments) => typeArguments.Length == 0 && Arity == 0 ? this : new ConstructedNamedTypeSymbol(this, [.. typeArguments]);
    public override void Accept(SymbolVisitor visitor) => visitor.VisitNamedType(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitNamedType(this);
}
