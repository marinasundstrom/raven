using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Metadata;

namespace Raven.CodeAnalysis.NeoClr;

internal sealed class NativeFieldSymbol : Symbol, IFieldSymbol, IInstanceFieldLayoutSymbol
{
    private readonly Lazy<ITypeSymbol> type;
    internal NativeFieldSymbol(Compilation compilation, FieldDefinition definition, NativeNamedTypeSymbol owner, int instanceStorageOrdinal)
        : base(SymbolKind.Field, definition.Name, owner, owner, owner.ContainingNamespace, [], [],
            (definition.Attributes & 7) == 6 ? Accessibility.Public : (definition.Attributes & 7) == 3 ? Accessibility.Internal : Accessibility.Private)
    {
        InstanceStorageOrdinal = instanceStorageOrdinal;
        IsReadOnly = (definition.Attributes & 0x20) != 0;
        if (!definition.TryGetSignature(out var signature)) throw new InvalidDataException("unsupported native field signature");
        type = new(() => owner.Map(signature!));
    }
    public ITypeSymbol Type => type.Value;
    public override IModuleSymbol ContainingModule => ContainingType!.ContainingModule!;
    public override IAssemblySymbol ContainingAssembly => ContainingType!.ContainingAssembly!;
    public override bool IsStatic => false;
    public bool IsConst => false;
    public bool IsReadOnly { get; }
    public int InstanceStorageOrdinal { get; }
    public bool IsRequired => false;
    public object? GetConstantValue() => null;
    public override void Accept(SymbolVisitor visitor) => visitor.VisitField(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitField(this);
}
