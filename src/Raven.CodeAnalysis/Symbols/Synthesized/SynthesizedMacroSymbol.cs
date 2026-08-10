using System.Collections.Immutable;

using Raven.CodeAnalysis.Macros;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class SynthesizedMacroSymbol : Symbol, IMacroSymbol
{
    public SynthesizedMacroSymbol(
        string name,
        string canonicalName,
        ImmutableArray<string> aliases,
        IMacroDefinition definition,
        INamespaceSymbol containingNamespace)
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
        Definition = definition;
    }

    public override string MetadataName => Name;

    public override bool IsStatic => true;

    public override bool CanBeReferencedByName => true;

    public MacroApplicationKind ApplicationKind => MacroFacts.GetApplicationKind(Definition);

    public MacroInvocationTargets InvocationTargets => MacroFacts.GetInvocationTargets(Definition);

    public MacroKind MacroKind => MacroFacts.GetKind(Definition);

    public MacroTarget Targets => Definition is IAttachedDeclarationMacro attached
        ? attached.Targets
        : MacroTarget.None;

    public string CanonicalName { get; }

    public ImmutableArray<string> Aliases { get; }

    internal IMacroDefinition Definition { get; }

    public override void Accept(SymbolVisitor visitor) => visitor.DefaultVisit(this);

    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.DefaultVisit(this);
}
