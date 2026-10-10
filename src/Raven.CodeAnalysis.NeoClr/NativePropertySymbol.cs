using System.Collections.Immutable;

using NeoCLR.Metadata.Experimental.Introspection;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.NeoClr;

internal sealed class NativePropertySymbol : Symbol, IPropertySymbol
{
    public override Raven.CodeAnalysis.Documentation.DocumentationComment? GetDocumentationComment() => NativeDocumentation.Get(this);

    private readonly Lazy<ITypeSymbol> type;
    private readonly Lazy<ImmutableArray<AttributeData>> attributes;
    public override ImmutableArray<AttributeData> GetAttributes() => attributes.Value;
    private readonly Lazy<ImmutableArray<IParameterSymbol>> parameters;
    internal NativePropertySymbol(PropertyInfo view, NativeNamedTypeSymbol owner)
        : base(SymbolKind.Property, view.Name, owner, owner, owner.ContainingNamespace, [], [], AccessibilityFor(view))
    {
        var module = (NativeModuleSymbol)owner.ContainingModule;
        IsStatic = view.IsStatic;
        attributes = new(() => ((NativeModuleSymbol)owner.ContainingModule).MapAttributes(view.GetCustomAttributes()));
        IsInitOnly = view.IsInitOnly;
        IsIndexer = view.IndexParameterTypes.Count != 0;
        type = new(() => module.MapView(view.PropertyType));
        GetMethod = view.GetMethod is { } getter ? module.GetMethodSymbol(getter.MetadataToken) : null;
        SetMethod = view.SetMethod is { } setter ? module.GetMethodSymbol(setter.MetadataToken) : null;
        parameters = new(() => !IsIndexer ? [] : GetMethod?.Parameters ?? [.. SetMethod!.Parameters.Take(SetMethod.Parameters.Length - 1)]);
        (GetMethod as NativeMethodSymbol)?.Associate(this);
        (SetMethod as NativeMethodSymbol)?.Associate(this);
    }
    private static Accessibility AccessibilityFor(PropertyInfo view)
    {
        var access = new[] { view.GetMethod, view.SetMethod }.Where(m => m is not null)
            .Select(m => NativeMetadataAccess.Map(m!.Accessibility)).ToArray();
        return access.Contains(Accessibility.Public) ? Accessibility.Public : access.Contains(Accessibility.Internal) ? Accessibility.Internal : Accessibility.Private;
    }
    public ITypeSymbol Type => type.Value;
    public IMethodSymbol? GetMethod { get; }
    public IMethodSymbol? SetMethod { get; }
    public IPropertySymbol OriginalDefinition => this;
    public override bool IsStatic { get; }
    public bool IsIndexer { get; }
    public ImmutableArray<IParameterSymbol> Parameters => parameters.Value;
    internal bool IsInitOnly { get; }
    public bool IsRequired => false;
    public override IModuleSymbol ContainingModule => ContainingType!.ContainingModule!;
    public override IAssemblySymbol ContainingAssembly => ContainingType!.ContainingAssembly!;
    public override void Accept(SymbolVisitor visitor) => visitor.VisitProperty(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitProperty(this);
}
