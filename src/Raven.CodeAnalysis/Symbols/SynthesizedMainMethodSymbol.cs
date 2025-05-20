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
