using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen;

internal class BaseGenerator : Generator
{
    private readonly MethodBodyGenerator _methodBodyGenerator;

    public BaseGenerator(MethodBodyGenerator methodBodyGenerator) : base(null)
    {
        _methodBodyGenerator = methodBodyGenerator;
    }

    public override MethodBodyGenerator MethodBodyGenerator => _methodBodyGenerator;
}