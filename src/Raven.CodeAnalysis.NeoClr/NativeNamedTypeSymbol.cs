using System.Collections.Immutable;

using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.NeoClr;

// Native declarations retain their own generic parameter scopes.
// Keep unsupported categories at the reader boundary rather than manufacturing members.
internal sealed class NativeNamedTypeSymbol : Symbol, INamedTypeSymbol
{
    private readonly Compilation compilation;
    private readonly NeoCLR.Metadata.Experimental.Introspection.NominalTypeInfo view;
    private readonly Lazy<ImmutableArray<INamedTypeSymbol>> interfaces;
    private readonly Lazy<ImmutableArray<INamedTypeSymbol>> allInterfaces;
    private readonly ImmutableArray<ISymbol> members;
    internal NativeNamedTypeSymbol(Compilation compilation, TypeDefinition definition, NativeNamespaceSymbol owner)
        : base(SymbolKind.Type, definition.GenericArity == 0 ? definition.Name : definition.Name[..definition.Name.LastIndexOf('`')], owner, null, owner, [], [],
            NativeMetadataAccess.Map(((NativeModuleSymbol)owner.ContainingModule).TypeView(definition).Accessibility))
    {
        this.compilation = compilation;
        Definition = definition;
        view = ((NativeModuleSymbol)owner.ContainingModule).TypeView(definition);
        TypeParameters = [.. (definition.GenericParameterNames ?? []).Select((name, i) => (ITypeParameterSymbol)new NativeTypeParameterSymbol(name, i, this))];
        TypeArguments = [.. TypeParameters];
        interfaces = new(() =>
        {
            var module = (NativeModuleSymbol)ContainingModule;
            return [.. module.TypeView(definition).GetDeclaredInterfaces().Select(view => (INamedTypeSymbol)module.MapView(view))];
        });
        allInterfaces = new(() =>
        {
            var module = (NativeModuleSymbol)ContainingModule;
            return [.. module.TypeView(definition).GetInterfaces().Select(view => (INamedTypeSymbol)module.MapView(view))];
        });
        var methods = definition.Methods.ToDictionary(method => method, method => new NativeMethodSymbol(compilation, method, this));
        members = [.. methods.Values,
            .. definition.Fields.Select((field, ordinal) => (ISymbol)new NativeFieldSymbol(compilation, field, this, ordinal)),
            .. definition.Properties.Select(property => (ISymbol)new NativePropertySymbol(property, this, methods))];
    }
    internal TypeDefinition Definition { get; }
    public override string MetadataName => Definition.Name;
    internal ITypeSymbol Map(SignatureType signature) => ((NativeModuleSymbol)ContainingModule).Map(signature, this);
    public override IModuleSymbol ContainingModule => ContainingNamespace!.ContainingModule!;
    public override IAssemblySymbol ContainingAssembly => ContainingNamespace!.ContainingAssembly!;
    public override bool IsStatic => view.IsStatic;
    public bool IsAbstract => view.IsAbstract;
    public bool IsClosed => view.IsSealed;
    public bool IsNamespace => false;
    public bool IsType => true;
    public TypeKind TypeKind => view.IsInterface ? TypeKind.Interface : TypeKind.Class;
    public SpecialType SpecialType => SpecialType.None;
    public INamedTypeSymbol? BaseType => TypeKind == TypeKind.Interface ? null : compilation.GetSpecialType(SpecialType.System_Object) as INamedTypeSymbol;
    public ITypeSymbol OriginalDefinition => this;
    public ITypeSymbol ConstructedFrom => this;
    public int Arity => Definition.GenericArity;
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
    public ImmutableArray<ISymbol> GetMembers(string name) => [.. members.Where(member => member.Name == name)];
    public ITypeSymbol? LookupType(string name) => null;
    public bool IsMemberDefined(string name, out ISymbol? symbol) { symbol = members.FirstOrDefault(member => member.Name == name); return symbol is not null; }
    public ITypeSymbol Construct(params ITypeSymbol[] typeArguments) => typeArguments.Length == 0 && Arity == 0 ? this : new ConstructedNamedTypeSymbol(this, [.. typeArguments]);
    public override void Accept(SymbolVisitor visitor) => visitor.VisitNamedType(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitNamedType(this);
}
