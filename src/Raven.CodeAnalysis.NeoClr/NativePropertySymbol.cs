using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.NeoClr;

internal sealed class NativePropertySymbol : Symbol, IPropertySymbol
{
    private readonly Lazy<ITypeSymbol> type;
    internal NativePropertySymbol(PropertyDefinition definition, NativeNamedTypeSymbol owner,
        IReadOnlyDictionary<MethodDefinition, NativeMethodSymbol> methods)
        : base(SymbolKind.Property, definition.Name, owner, owner, owner.ContainingNamespace, [], [], AccessibilityFor(definition))
    {
        if (!definition.TryGetSignature(out var signature, out var isStatic)) throw new InvalidDataException("unsupported native property signature");
        IsStatic = isStatic;
        type = new(() => ((NativeModuleSymbol)ContainingModule).Map(signature!));
        GetMethod = definition.GetMethod is { } getter ? methods[getter] : null;
        SetMethod = definition.SetMethod is { } setter ? methods[setter] : null;
        (GetMethod as NativeMethodSymbol)?.Associate(this);
        (SetMethod as NativeMethodSymbol)?.Associate(this);
    }
    private static Accessibility AccessibilityFor(PropertyDefinition definition)
    {
        var access = new[] { definition.GetMethod, definition.SetMethod }.Where(m => m is not null).Select(m => m!.Attributes & 7).ToArray();
        return access.Contains(6) ? Accessibility.Public : access.Contains(3) ? Accessibility.Internal : Accessibility.Private;
    }
    public ITypeSymbol Type => type.Value;
    public IMethodSymbol? GetMethod { get; }
    public IMethodSymbol? SetMethod { get; }
    public IPropertySymbol OriginalDefinition => this;
    public override bool IsStatic { get; }
    public bool IsIndexer => false;
    public bool IsRequired => false;
    public override IModuleSymbol ContainingModule => ContainingType!.ContainingModule!;
    public override IAssemblySymbol ContainingAssembly => ContainingType!.ContainingAssembly!;
    public override void Accept(SymbolVisitor visitor) => visitor.VisitProperty(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitProperty(this);
}
