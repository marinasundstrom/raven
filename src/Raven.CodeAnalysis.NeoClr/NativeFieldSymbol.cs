using System.Collections.Immutable;

using NeoCLR.Metadata.Experimental.Introspection;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Metadata;

namespace Raven.CodeAnalysis.NeoClr;

internal sealed class NativeFieldSymbol : SymbolBase, FieldSymbol, InstanceFieldLayoutSymbol
{
    public override Raven.CodeAnalysis.Documentation.DocumentationComment? GetDocumentationComment() => NativeDocumentation.Get(this);

    private readonly Lazy<TypeSymbol> type;
    private readonly Lazy<int> instanceStorageOrdinal;
    private readonly Lazy<ImmutableArray<AttributeData>> attributes;
    public override ImmutableArray<AttributeData> GetAttributes() => attributes.Value;
    internal NativeFieldSymbol(FieldInfo view, NativeNamedTypeSymbol owner, Func<int> instanceStorageOrdinal)
        : base(SymbolKind.Field, view.Name, owner, owner, owner.ContainingNamespace, [], [], NativeMetadataAccess.Map(view.Accessibility))
    {
        this.instanceStorageOrdinal = new(instanceStorageOrdinal);
        IsConst = view.IsLiteral; constant = view.Constant;
        IsReadOnly = view.IsReadOnly;
        IsStatic = view.IsStatic;
        attributes = new(() => ((NativeModuleSymbol)owner.ContainingModule).MapAttributes(view.GetCustomAttributes()));
        type = new(() => ((NativeModuleSymbol)owner.ContainingModule).MapView(view.FieldType));
    }
    public TypeSymbol Type => type.Value;
    public override ModuleSymbol ContainingModule => ContainingType!.ContainingModule!;
    public override AssemblySymbol ContainingAssembly => ContainingType!.ContainingAssembly!;
    public override bool IsStatic { get; }
    private readonly object? constant;
    public bool IsConst { get; }
    public bool IsReadOnly { get; }
    public int InstanceStorageOrdinal => instanceStorageOrdinal.Value;
    public bool IsRequired => false;
    public object? GetConstantValue() => constant;
    public override void Accept(SymbolVisitor visitor) => visitor.VisitField(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitField(this);
}
