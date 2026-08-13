using System.Collections.Immutable;

using Raven.CodeAnalysis.Documentation;
using Raven.CodeAnalysis.Macros;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class SynthesizedMacroSymbol : Symbol, IMacroSymbol
{
    public SynthesizedMacroSymbol(
        string name,
        string canonicalName,
        ImmutableArray<string> aliases,
        MacroDefinitionDescriptor descriptor,
        INamespaceSymbol containingNamespace,
        INamedTypeSymbol? implementationType)
        : base(
            SymbolKind.Macro,
            name,
            containingNamespace,
            containingType: null,
            containingNamespace,
            [],
            [],
            Accessibility.Public)
    {
        CanonicalName = canonicalName;
        Aliases = aliases;
        Descriptor = descriptor;
        ImplementationType = implementationType;
    }

    public override string MetadataName => Name;

    public override bool IsStatic => true;

    public override bool CanBeReferencedByName => true;

    public MacroApplicationKind ApplicationKind => Descriptor.ApplicationKind;

    public MacroInvocationTargets InvocationTargets => Descriptor.InvocationTargets;

    public MacroKind MacroKind => MacroFacts.GetKind(Descriptor.Definition);

    public MacroTarget Targets => Descriptor.AttachmentTargets;

    public string CanonicalName { get; }

    public ImmutableArray<string> Aliases { get; }

    internal MacroDefinitionDescriptor Descriptor { get; }

    internal INamedTypeSymbol? ImplementationType { get; }

    public override DocumentationComment? GetDocumentationComment()
    {
        var documentation = Descriptor.Definition.Documentation;
        if (!string.IsNullOrWhiteSpace(documentation))
        {
            return DocumentationComment.Create(
                Descriptor.Definition.DocumentationFormat,
                documentation);
        }

        return ImplementationType?.GetDocumentationComment();
    }

    public override void Accept(SymbolVisitor visitor) => visitor.DefaultVisit(this);

    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.DefaultVisit(this);
}
