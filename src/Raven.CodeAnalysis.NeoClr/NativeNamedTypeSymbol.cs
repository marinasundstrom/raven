using System.Collections.Immutable;

using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.NeoClr;

// The reader currently admits nongeneric top-level classes with bounded fields and properties.
// Keep unsupported categories at the reader boundary rather than manufacturing members.
internal sealed class NativeNamedTypeSymbol : Symbol, INamedTypeSymbol
{
    private readonly Compilation compilation;
    private readonly ImmutableArray<ISymbol> members;
    internal NativeNamedTypeSymbol(Compilation compilation, TypeDefinition definition, NativeNamespaceSymbol owner)
        : base(SymbolKind.Type, definition.Name, owner, null, owner, [], [],
            (definition.Attributes & 1) != 0 ? Accessibility.Public : Accessibility.Internal)
    {
        this.compilation = compilation;
        Definition = definition;
        var methods = definition.Methods.ToDictionary(method => method, method => new NativeMethodSymbol(compilation, method, this));
        members = [.. methods.Values,
            .. definition.Fields.Select(field => (ISymbol)new NativeFieldSymbol(compilation, field, this)),
            .. definition.Properties.Select(property => (ISymbol)new NativePropertySymbol(property, this, methods))];
    }
    internal TypeDefinition Definition { get; }
    public override IModuleSymbol ContainingModule => ContainingNamespace!.ContainingModule!;
    public override IAssemblySymbol ContainingAssembly => ContainingNamespace!.ContainingAssembly!;
    public override bool IsStatic => (Definition.Attributes & 0x180) == 0x180;
    public bool IsAbstract => (Definition.Attributes & 0x80) != 0;
    public bool IsClosed => (Definition.Attributes & 0x100) != 0;
    public bool IsNamespace => false;
    public bool IsType => true;
    public TypeKind TypeKind => TypeKind.Class;
    public SpecialType SpecialType => SpecialType.None;
    public INamedTypeSymbol? BaseType => compilation.GetSpecialType(SpecialType.System_Object) as INamedTypeSymbol;
    public ITypeSymbol OriginalDefinition => this;
    public ITypeSymbol ConstructedFrom => this;
    public int Arity => 0;
    public bool IsGenericType => false;
    public bool IsUnboundGenericType => false;
    public ImmutableArray<ITypeSymbol> TypeArguments => [];
    public ImmutableArray<ITypeParameterSymbol> TypeParameters => [];
    public ImmutableArray<INamedTypeSymbol> Interfaces => [];
    public ImmutableArray<INamedTypeSymbol> AllInterfaces => [];
    public ImmutableArray<IMethodSymbol> Constructors => InstanceConstructors;
    public ImmutableArray<IMethodSymbol> InstanceConstructors => [.. members.OfType<IMethodSymbol>().Where(m => m.MethodKind == MethodKind.Constructor)];
    public IMethodSymbol? StaticConstructor => null;
    public INamedTypeSymbol? UnderlyingTupleType => null;
    public ImmutableArray<IFieldSymbol> TupleElements => [];
    public ImmutableArray<ISymbol> GetMembers() => members;
    public ImmutableArray<ISymbol> GetMembers(string name) => [.. members.Where(member => member.Name == name)];
    public ITypeSymbol? LookupType(string name) => null;
    public bool IsMemberDefined(string name, out ISymbol? symbol) { symbol = members.FirstOrDefault(member => member.Name == name); return symbol is not null; }
    public ITypeSymbol Construct(params ITypeSymbol[] typeArguments) => typeArguments.Length == 0 ? this : throw new ArgumentException("nongeneric native type");
    public override void Accept(SymbolVisitor visitor) => visitor.VisitNamedType(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitNamedType(this);
}
