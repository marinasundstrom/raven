using System.Collections.Immutable;
using NeoCLR.Metadata.Experimental.Model;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.NeoClr;

// The reader currently admits only fieldless nongeneric top-level static classes.
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
        members = [.. definition.Methods.Select(method => (ISymbol)new NativeMethodSymbol(compilation, method, this))];
    }
    internal TypeDefinition Definition { get; }
    public override IModuleSymbol ContainingModule => ContainingNamespace!.ContainingModule!;
    public override IAssemblySymbol ContainingAssembly => ContainingNamespace!.ContainingAssembly!;
    public override bool IsStatic => true;
    public bool IsAbstract => true;
    public bool IsClosed => true;
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
    public ImmutableArray<IMethodSymbol> Constructors => [];
    public ImmutableArray<IMethodSymbol> InstanceConstructors => [];
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
