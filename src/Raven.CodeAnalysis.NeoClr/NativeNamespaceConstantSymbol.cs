using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.NeoClr;

internal sealed class NativeNamespaceConstantSymbol : Symbol, IFieldSymbol
{
    private readonly double value;
    private readonly Lazy<ITypeSymbol> type;
    internal NativeNamespaceConstantSymbol(NamespaceConstantDefinition constant, INamespaceSymbol owner, Func<ITypeSymbol> type)
        : base(SymbolKind.Field, constant.Name, owner, null, owner, [], [],
            constant.Visibility == MethodVisibility.Public ? Accessibility.Public : Accessibility.Internal)
    { value = constant.Value; this.type = new(type); }
    public ITypeSymbol Type => type.Value;
    public override IModuleSymbol ContainingModule => ContainingNamespace!.ContainingModule!;
    public override IAssemblySymbol ContainingAssembly => ContainingNamespace!.ContainingAssembly!;
    public override bool IsStatic => true;
    public bool IsConst => true;
    public bool IsReadOnly => false;
    public bool IsRequired => false;
    public object GetConstantValue() => value;
    public override void Accept(SymbolVisitor visitor) => visitor.VisitField(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitField(this);
}
