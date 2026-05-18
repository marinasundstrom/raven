using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.Symbols;

sealed partial class SynthesizedNamespaceMembersClassSymbol : SourceNamedTypeSymbol, ITypeSymbol
{
    public const string MetadataTypeName = "NamespaceMembers";

    public SynthesizedNamespaceMembersClassSymbol(
        Compilation compilation,
        INamespaceSymbol @namespace,
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

    public override IAssemblySymbol ContainingAssembly => ContainingSymbol!.ContainingAssembly!;

    public override IModuleSymbol ContainingModule => ContainingSymbol!.ContainingModule!;

    public override bool IsImplicitlyDeclared => true;
}
