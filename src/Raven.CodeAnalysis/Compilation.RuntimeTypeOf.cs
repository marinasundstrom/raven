using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Targets;

namespace Raven.CodeAnalysis;

public partial class Compilation
{
    private readonly System.Collections.Generic.Dictionary<ITypeSymbol, ImmutableArray<ISymbol>> _functionContractMembers =
        new(System.Collections.Generic.ReferenceEqualityComparer.Instance);
    internal RuntimeTypeOfBinding? ResolveRuntimeTypeOfContract()
        => _target.RuntimeContract.ResolveTypeOf(this);
    internal ImmutableArray<ISymbol> GetFunctionContractMembers(ITypeSymbol owner)
    {
        if (!UsesStructuralFunctions || !SemanticFacts.SatisfiesFunctionTypeConstraint(owner)
            || _target.RuntimeContract.ResolveFunctionInfo(this) is not { } binding)
            return [];
        lock (_functionContractMembers)
        {
            if (!_functionContractMembers.TryGetValue(owner, out var members))
                _functionContractMembers.Add(owner, members = [new StructuralFunctionPropertySymbol(owner, binding)]);
            return members;
        }
    }
}
