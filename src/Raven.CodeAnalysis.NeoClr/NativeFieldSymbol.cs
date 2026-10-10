using System.Collections.Immutable;

using NeoCLR.Metadata.Experimental.Introspection;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Metadata;

namespace Raven.CodeAnalysis.NeoClr;

internal sealed class NativeFieldSymbol : Symbol, IFieldSymbol, IInstanceFieldLayoutSymbol
{
    public override Raven.CodeAnalysis.Documentation.DocumentationComment? GetDocumentationComment() => NativeDocumentation.Get(this);

    private readonly Lazy<ITypeSymbol> type;
    private readonly Lazy<ImmutableArray<AttributeData>> attributes;
    public override ImmutableArray<AttributeData> GetAttributes() => attributes.Value;
    internal NativeFieldSymbol(FieldInfo view, NativeNamedTypeSymbol owner, int instanceStorageOrdinal)
        : base(SymbolKind.Field, view.Name, owner, owner, owner.ContainingNamespace, [], [], NativeMetadataAccess.Map(view.Accessibility))
    {
        InstanceStorageOrdinal = instanceStorageOrdinal;
        IsConst = view.IsLiteral; constant = view.Constant;
        IsReadOnly = view.IsReadOnly;
        IsStatic = view.IsStatic;
        attributes = new(() => ((NativeModuleSymbol)owner.ContainingModule).MapAttributes(view.GetCustomAttributes()));
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
