using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.Symbols;

sealed partial class SynthesizedProgramClassSymbol : SourceNamedTypeSymbol, TypeSymbol
{

    public SynthesizedProgramClassSymbol(Compilation compilation, NamespaceSymbol @namespace, Location[] location, SyntaxReference[] syntaxReferences)
        : base("Program", @namespace, null, @namespace, location, syntaxReferences, declaredAccessibility: Accessibility.Internal)
    {
        Compilation = compilation;

    }

    public Compilation Compilation { get; }

    public override AssemblySymbol ContainingAssembly => ContainingSymbol!.ContainingAssembly!;

    public override ModuleSymbol ContainingModule => ContainingSymbol!.ContainingModule!;

    public override bool IsStatic => true;

    public override bool IsImplicitlyDeclared => true;

}
