using Raven.CodeAnalysis.Targets;

namespace Raven.CodeAnalysis;

public partial class Compilation
{
    internal RuntimeTypeOfBinding? ResolveRuntimeTypeOfContract()
        => _target.RuntimeContract.ResolveTypeOf(this);
}
