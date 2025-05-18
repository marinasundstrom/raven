using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

sealed partial class SynthesizedMainMethodSymbol : SourceMethodSymbol, IMethodSymbol
{

    public SynthesizedMainMethodSymbol(Compilation compilation, INamespaceSymbol @namespace) : base("Main",
               returnType: compilation.GetSpecialType(SpecialType.System_Void),
               parameters: ImmutableArray<IParameterSymbol>.Empty, null, null, @namespace, [], [])
    {

    }

    public override bool IsStatic => true;

    public override bool IsImplicitlyDeclared => true;
}