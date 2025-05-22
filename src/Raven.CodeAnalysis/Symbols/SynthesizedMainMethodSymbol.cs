using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

sealed partial class SynthesizedMainMethodSymbol : SourceMethodSymbol, IMethodSymbol
{

    public SynthesizedMainMethodSymbol(SynthesizedProgramClassSymbol type, Location[] location, SyntaxReference[] declaringSyntaxReferences) : base("Main",
               returnType: type.ContainingAssembly.GetTypeByMetadataName("System.Void"),
               parameters: [], type, type, type.ContainingNamespace, location, declaringSyntaxReferences)
    {
        var arrayType = type.ContainingAssembly.GetTypeByMetadataName("System.Array");
        var stringType = type.ContainingAssembly.GetTypeByMetadataName("System.String");
        var stringArrayType = new ArrayTypeSymbol(arrayType, stringType, arrayType.ContainingSymbol, null, arrayType.ContainingNamespace, []);

        Parameters = [new SourceParameterSymbol("args", stringArrayType, this, type, type.ContainingNamespace, location, declaringSyntaxReferences)];
    }

    public override bool IsStatic => true;

    public override bool IsImplicitlyDeclared => true;
}
