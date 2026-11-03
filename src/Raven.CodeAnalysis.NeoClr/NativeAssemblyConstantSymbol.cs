using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.NeoClr;

internal sealed class NativeAssemblyConstantSymbol : SymbolBase, FieldSymbol
{
    public override Raven.CodeAnalysis.Documentation.DocumentationComment? GetDocumentationComment() => NativeDocumentation.Get(this);

    private readonly double value;
    private readonly Lazy<TypeSymbol> type;
    internal NativeAssemblyConstantSymbol(AssemblyConstantDefinition constant, NamespaceSymbol owner, Func<TypeSymbol> type)
        : base(SymbolKind.Field, constant.Name, owner, null, owner, [], [],
            constant.Visibility == MethodVisibility.Public ? Accessibility.Public : Accessibility.Internal)
    { value = constant.Value; this.type = new(type); }
    public TypeSymbol Type => type.Value;
    public override ModuleSymbol ContainingModule => ContainingNamespace!.ContainingModule!;
    public override AssemblySymbol ContainingAssembly => ContainingNamespace!.ContainingAssembly!;
    public override bool IsStatic => true;
    public bool IsConst => true;
    public bool IsReadOnly => false;
    public bool IsRequired => false;
    public object GetConstantValue() => value;
    public override void Accept(SymbolVisitor visitor) => visitor.VisitField(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitField(this);
}
