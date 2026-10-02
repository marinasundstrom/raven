using System.Collections.Immutable;
using System.Collections.Concurrent;

using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.NeoClr;

// Native declarations retain their own generic parameter scopes.
// Keep unsupported categories at the reader boundary rather than manufacturing members.
internal sealed class NativeNamedTypeSymbol : Symbol, INamedTypeSymbol
{
    private readonly Compilation compilation;
    private readonly ConcurrentDictionary<SignatureType, ITypeSymbol> genericSignatureTypes = new();
    private readonly Lazy<ImmutableArray<INamedTypeSymbol>> interfaces;
    private readonly Lazy<ImmutableArray<INamedTypeSymbol>> allInterfaces;
    private readonly ImmutableArray<ISymbol> members;
    internal NativeNamedTypeSymbol(Compilation compilation, TypeDefinition definition, NativeNamespaceSymbol owner)
        : base(SymbolKind.Type, definition.GenericArity == 0 ? definition.Name : definition.Name[..definition.Name.LastIndexOf('`')], owner, null, owner, [], [],
            (definition.Attributes & 1) != 0 ? Accessibility.Public : Accessibility.Internal)
    {
        this.compilation = compilation;
        Definition = definition;
        TypeParameters = [.. (definition.GenericParameterNames ?? []).Select((name, i) => (ITypeParameterSymbol)new NativeTypeParameterSymbol(name, i, this))];
        TypeArguments = [.. TypeParameters];
        interfaces = new(() => [.. definition.Interfaces.Select(i => (INamedTypeSymbol)((NativeModuleSymbol)ContainingModule).Resolve(i.InterfaceType))]);
        allInterfaces = new(() =>
        {
            var result = new List<INamedTypeSymbol>();
            var seen = new HashSet<INamedTypeSymbol>(SymbolEqualityComparer.Default);
            void Add(INamedTypeSymbol contract) { if (seen.Add(contract)) { result.Add(contract); foreach (var parent in contract.Interfaces) Add(parent); } }
            foreach (var contract in Interfaces) Add(contract);
            return [.. result];
        });
        var methods = definition.Methods.ToDictionary(method => method, method => new NativeMethodSymbol(compilation, method, this));
        members = [.. methods.Values,
            .. definition.Fields.Select((field, ordinal) => (ISymbol)new NativeFieldSymbol(compilation, field, this, ordinal)),
            .. definition.Properties.Select(property => (ISymbol)new NativePropertySymbol(property, this, methods))];
    }
    internal TypeDefinition Definition { get; }
    public override string MetadataName => Definition.Name;
    internal ITypeSymbol Map(SignatureType signature) => signature.TypeParameterIndex is { } ordinal ? TypeParameters[ordinal]
        : NativeModuleSymbol.HasParameter(signature, method: false)
            ? genericSignatureTypes.GetOrAdd(signature, type => ((NativeModuleSymbol)ContainingModule).ConstructSignature(type, Map))
        : ((NativeModuleSymbol)ContainingModule).Map(signature);
    public override IModuleSymbol ContainingModule => ContainingNamespace!.ContainingModule!;
    public override IAssemblySymbol ContainingAssembly => ContainingNamespace!.ContainingAssembly!;
    public override bool IsStatic => (Definition.Attributes & 0x180) == 0x180;
    public bool IsAbstract => (Definition.Attributes & 0x80) != 0;
    public bool IsClosed => (Definition.Attributes & 0x100) != 0;
    public bool IsNamespace => false;
    public bool IsType => true;
    public TypeKind TypeKind => (Definition.Attributes & 0x20) != 0 ? TypeKind.Interface : TypeKind.Class;
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
