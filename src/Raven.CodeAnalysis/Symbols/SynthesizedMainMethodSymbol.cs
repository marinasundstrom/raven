using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

sealed partial class SynthesizedMainMethodSymbol : SourceMethodSymbol, IMethodSymbol
{

    public SynthesizedMainMethodSymbol(SynthesizedProgramClassSymbol type, Location[] location, SyntaxReference[] syntaxReferences) : base("Main",
               returnType: type.ContainingAssembly.GetTypeByMetadataName("System.Void"),
               parameters: [], type, type, type.ContainingNamespace, location, syntaxReferences)
    {

    }

    public override bool IsStatic => true;

    public override bool IsImplicitlyDeclared => true;
}

sealed partial class SynthesizedProgramClassSymbol : SourceNamedTypeSymbol, ITypeSymbol
{

    public SynthesizedProgramClassSymbol(Compilation compilation, INamespaceSymbol @namespace, Location[] location, SyntaxReference[] syntaxReferences)
        : base("Program", @namespace, null, @namespace, location, syntaxReferences)
    {

    }

    public override IAssemblySymbol ContainingAssembly => ContainingSymbol!.ContainingAssembly!;

    public override IModuleSymbol ContainingModule => ContainingSymbol!.ContainingModule!;

    public override bool IsStatic => true;

    public override bool IsImplicitlyDeclared => true;
}