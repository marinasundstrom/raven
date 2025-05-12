using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

class TopLevelBinder : BlockBinder
{
    public TopLevelBinder(Binder parent) : base(parent) { }
}

sealed class SynthesizedMainMethodSymbol : SourceMethodSymbol, IMethodSymbol
{

    public SynthesizedMainMethodSymbol(Compilation compilation) : base("Main",
               returnType: compilation.GetSpecialType(SpecialType.System_Void),
               parameters: ImmutableArray<IParameterSymbol>.Empty, null, null, compilation.GlobalNamespace, [], [])
    {

    }

    public override bool IsStatic => true;

    public override bool IsImplicitlyDeclared => true;
}