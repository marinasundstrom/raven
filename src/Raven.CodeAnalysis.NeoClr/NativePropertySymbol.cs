using System.Collections.Immutable;

using NeoCLR.Metadata.Experimental.Introspection;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.NeoClr;

internal sealed class NativePropertySymbol : SymbolBase, PropertySymbol
{
    public override Raven.CodeAnalysis.Documentation.DocumentationComment? GetDocumentationComment() => NativeDocumentation.Get(this);

    private readonly Lazy<TypeSymbol> type;
    private readonly Lazy<ImmutableArray<AttributeData>> attributes;
    public override ImmutableArray<AttributeData> GetAttributes() => attributes.Value;
    private readonly Lazy<ImmutableArray<ParameterSymbol>> parameters;
    internal NativePropertySymbol(PropertyInfo view, NativeNamedTypeSymbol owner)
        : base(SymbolKind.Property, view.Name, owner, owner, owner.ContainingNamespace, [], [], AccessibilityFor(view))
    {
        var module = (NativeModuleSymbol)owner.ContainingModule;
        IsStatic = view.IsStatic;
        attributes = new(() => ((NativeModuleSymbol)owner.ContainingModule).MapAttributes(view.GetCustomAttributes()));
        IsInitOnly = view.IsInitOnly;
        IsIndexer = view.IndexParameterTypes.Count != 0;
        // Property rows describe physical storage. Nullable annotations live on
        // accessor signatures; preserve them for ordinary property binding too.
        type = new(() => GetMethod?.ReturnType ?? SetMethod?.Parameters.Last().Type ?? module.MapView(view.PropertyType));
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
    public TypeSymbol Type => type.Value;
    public MethodSymbol? GetMethod { get; }
    public MethodSymbol? SetMethod { get; }
    public PropertySymbol OriginalDefinition => this;
    public override bool IsStatic { get; }
    public bool IsIndexer { get; }
    public ImmutableArray<ParameterSymbol> Parameters => parameters.Value;
    internal bool IsInitOnly { get; }
    public bool IsRequired => false;
    public override ModuleSymbol ContainingModule => ContainingType!.ContainingModule!;
    public override AssemblySymbol ContainingAssembly => ContainingType!.ContainingAssembly!;
    public override void Accept(SymbolVisitor visitor) => visitor.VisitProperty(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitProperty(this);
}
