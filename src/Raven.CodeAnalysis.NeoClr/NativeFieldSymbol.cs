using NeoCLR.Metadata.Experimental.Model;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.NeoClr;

internal sealed class NativeFieldSymbol : Symbol, IFieldSymbol
{
    private readonly Compilation compilation;
    private readonly PrimitiveType primitive;
    internal NativeFieldSymbol(Compilation compilation, FieldDefinition definition, NativeNamedTypeSymbol owner)
        : base(SymbolKind.Field, definition.Name, owner, owner, owner.ContainingNamespace, [], [],
            (definition.Attributes & 7) == 6 ? Accessibility.Public : (definition.Attributes & 7) == 3 ? Accessibility.Internal : Accessibility.Private)
    {
        this.compilation = compilation;
        Definition = definition;
        if (!definition.TryGetPrimitiveType(out primitive)) throw new InvalidDataException("unsupported native field signature");
    }
    internal FieldDefinition Definition { get; }
    public ITypeSymbol Type => compilation.GetSpecialType(primitive switch {
        PrimitiveType.Int32 => SpecialType.System_Int32,
        PrimitiveType.Int64 => SpecialType.System_Int64,
        PrimitiveType.Boolean => SpecialType.System_Boolean,
        PrimitiveType.String => SpecialType.System_String,
        _ => throw new InvalidDataException("unsupported native field signature") });
    public override IModuleSymbol ContainingModule => ContainingType!.ContainingModule!;
    public override IAssemblySymbol ContainingAssembly => ContainingType!.ContainingAssembly!;
    public override bool IsStatic => false;
    public bool IsConst => false;
    public bool IsReadOnly => (Definition.Attributes & 0x20) != 0;
    public bool IsRequired => false;
    public object? GetConstantValue() => null;
    public override void Accept(SymbolVisitor visitor) => visitor.VisitField(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitField(this);
}
