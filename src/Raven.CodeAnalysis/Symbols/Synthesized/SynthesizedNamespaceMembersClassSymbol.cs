using System.Collections.Immutable;

using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.Symbols;

sealed partial class SynthesizedNamespaceMembersClassSymbol : SourceNamedTypeSymbol, TypeSymbol
{
    public const string MetadataTypeName = "NamespaceMembers";

    public SynthesizedNamespaceMembersClassSymbol(
        Compilation compilation,
        NamespaceSymbol @namespace,
        Location[] locations,
        SyntaxReference[] syntaxReferences)
        : base(
            MetadataTypeName,
            @namespace,
            null,
            @namespace,
            locations,
            syntaxReferences,
            isStatic: true,
            declaredAccessibility: Accessibility.Public)
    {
        Compilation = compilation;
    }

    public Compilation Compilation { get; }

    public override AssemblySymbol ContainingAssembly => ContainingSymbol!.ContainingAssembly!;

    public override ModuleSymbol ContainingModule => ContainingSymbol!.ContainingModule!;

    public override bool IsImplicitlyDeclared => true;

    public override ImmutableArray<AttributeData> GetAttributes()
        => base.GetAttributes();
}
