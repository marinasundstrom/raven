using NeoCLR.Metadata.Experimental.Introspection;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Metadata;

namespace Raven.CodeAnalysis.NeoClr;

internal sealed class NativeFieldSymbol : Symbol, IFieldSymbol, IInstanceFieldLayoutSymbol
{
    private readonly Lazy<ITypeSymbol> type;
    internal NativeFieldSymbol(FieldInfo view, NativeNamedTypeSymbol owner, int instanceStorageOrdinal)
        : base(SymbolKind.Field, view.Name, owner, owner, owner.ContainingNamespace, [], [], NativeMetadataAccess.Map(view.Accessibility))
    {
        InstanceStorageOrdinal = instanceStorageOrdinal;
        IsConst = view.IsLiteral; constant = view.Constant;
        IsReadOnly = view.IsReadOnly;
        IsStatic = view.IsStatic;
        type = new(() => ((NativeModuleSymbol)owner.ContainingModule).MapView(view.FieldType));
    }
    public ITypeSymbol Type => type.Value;
    public override IModuleSymbol ContainingModule => ContainingType!.ContainingModule!;
    public override IAssemblySymbol ContainingAssembly => ContainingType!.ContainingAssembly!;
    public override bool IsStatic { get; }
    private readonly int? constant;
    public bool IsConst { get; }
    public bool IsReadOnly { get; }
    public int InstanceStorageOrdinal { get; }
    public bool IsRequired => false;
    public object? GetConstantValue() => constant;
    public override void Accept(SymbolVisitor visitor) => visitor.VisitField(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitField(this);
}
