using System.Collections.Immutable;

using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.NeoClr;

internal sealed class NativePropertySymbol : Symbol, IPropertySymbol
{
    private readonly Lazy<ITypeSymbol> type;
    private readonly Lazy<ImmutableArray<IParameterSymbol>> parameters;
    internal NativePropertySymbol(PropertyDefinition definition, NativeNamedTypeSymbol owner,
        IReadOnlyDictionary<MethodDefinition, NativeMethodSymbol> methods)
        : base(SymbolKind.Property, definition.Name, owner, owner, owner.ContainingNamespace, [], [], AccessibilityFor(definition, owner))
    {
        var module = (NativeModuleSymbol)owner.ContainingModule;
        var view = module.TypeView(owner.Definition).GetProperties().Single(p => p.MetadataToken == definition.MetadataToken);
        IsStatic = view.IsStatic;
        IsIndexer = view.IndexParameterTypes.Count != 0;
        type = new(() => module.MapView(view.PropertyType));
        GetMethod = definition.GetMethod is { } getter ? methods[getter] : null;
        SetMethod = definition.SetMethod is { } setter ? methods[setter] : null;
        parameters = new(() => !IsIndexer ? [] : GetMethod?.Parameters ?? [.. SetMethod!.Parameters.Take(SetMethod.Parameters.Length - 1)]);
        (GetMethod as NativeMethodSymbol)?.Associate(this);
        (SetMethod as NativeMethodSymbol)?.Associate(this);
    }
    private static Accessibility AccessibilityFor(PropertyDefinition definition, NativeNamedTypeSymbol owner)
    {
        var module = (NativeModuleSymbol)owner.ContainingModule;
        var access = new[] { definition.GetMethod, definition.SetMethod }.Where(m => m is not null)
            .Select(m => NativeMetadataAccess.Map(module.MethodView(m!).Accessibility)).ToArray();
        return access.Contains(Accessibility.Public) ? Accessibility.Public : access.Contains(Accessibility.Internal) ? Accessibility.Internal : Accessibility.Private;
    }
    public ITypeSymbol Type => type.Value;
    public IMethodSymbol? GetMethod { get; }
    public IMethodSymbol? SetMethod { get; }
    public IPropertySymbol OriginalDefinition => this;
    public override bool IsStatic { get; }
    public bool IsIndexer { get; }
    public ImmutableArray<IParameterSymbol> Parameters => parameters.Value;
    public bool IsRequired => false;
    public override IModuleSymbol ContainingModule => ContainingType!.ContainingModule!;
    public override IAssemblySymbol ContainingAssembly => ContainingType!.ContainingAssembly!;
    public override void Accept(SymbolVisitor visitor) => visitor.VisitProperty(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitProperty(this);
}
