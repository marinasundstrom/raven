using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public partial class Compilation
{
    private readonly Dictionary<DelegateSignature, SynthesizedDelegateTypeSymbol> _synthesizedDelegates = new(new DelegateSignatureComparer());
    private readonly Dictionary<SourceMethodSymbol, SynthesizedAsyncStateMachineTypeSymbol> _synthesizedAsyncStateMachines = new(ReferenceEqualityComparer.Instance);
    private readonly Dictionary<SourceMethodSymbol, SynthesizedIteratorTypeSymbol> _synthesizedIterators = new(ReferenceEqualityComparer.Instance);
    private int _synthesizedDelegateOrdinal;
    private int _synthesizedAsyncStateMachineOrdinal;
    private int _synthesizedIteratorOrdinal;

    internal INamedTypeSymbol GetMethodReferenceDelegate(IMethodSymbol methodSymbol)
    {
        var parameterTypes = methodSymbol.Parameters
            .Select(p => p.Type)
            .ToImmutableArray();
        var refKinds = methodSymbol.Parameters
            .Select(p => p.RefKind)
            .ToImmutableArray();

        return GetMethodReferenceDelegate(parameterTypes, refKinds, methodSymbol.ReturnType);
    }

    internal INamedTypeSymbol GetMethodReferenceDelegate(
        ImmutableArray<ITypeSymbol> parameterTypes,
        ImmutableArray<RefKind> refKinds,
        ITypeSymbol returnType)
    {
        if (refKinds.All(static refKind => refKind == RefKind.None))
        {
            var functionType = CreateFunctionTypeSymbol(parameterTypes.ToArray(), returnType);
            if (functionType is INamedTypeSymbol namedDelegate && namedDelegate.TypeKind == TypeKind.Delegate)
                return namedDelegate;
        }

        var signature = new DelegateSignature(parameterTypes, refKinds, returnType);
        if (_synthesizedDelegates.TryGetValue(signature, out var existing))
            return existing;

        var delegateName = $"<>f__Delegate{_synthesizedDelegateOrdinal++}";
        var containingNamespace = SourceGlobalNamespace;
        var synthesized = new SynthesizedDelegateTypeSymbol(
            this,
            delegateName,
            parameterTypes,
            refKinds,
            returnType,
            containingNamespace);

        _synthesizedDelegates[signature] = synthesized;
        return synthesized;
    }

    internal IEnumerable<INamedTypeSymbol> GetSynthesizedDelegateTypes()
        => _synthesizedDelegates.Values;

    internal SynthesizedAsyncStateMachineTypeSymbol CreateAsyncStateMachine(SourceMethodSymbol method)
    {
        if (_synthesizedAsyncStateMachines.TryGetValue(method, out var existing))
            return existing;

        var name = $"<>c__AsyncStateMachine{_synthesizedAsyncStateMachineOrdinal++}";
        var stateMachine = new SynthesizedAsyncStateMachineTypeSymbol(this, method, name);
        _synthesizedAsyncStateMachines[method] = stateMachine;
        return stateMachine;
    }

    internal IEnumerable<SynthesizedAsyncStateMachineTypeSymbol> GetSynthesizedAsyncStateMachineTypes()
        => _synthesizedAsyncStateMachines.Values;

    internal SynthesizedIteratorTypeSymbol CreateIteratorStateMachine(SourceMethodSymbol method, IteratorMethodKind iteratorKind, ITypeSymbol elementType)
    {
        if (_synthesizedIterators.TryGetValue(method, out var existing))
            return existing;

        var name = $"<>c__Iterator{_synthesizedIteratorOrdinal++}";
        var stateMachine = new SynthesizedIteratorTypeSymbol(this, method, name, iteratorKind, elementType);
        _synthesizedIterators[method] = stateMachine;
        return stateMachine;
    }

    internal IEnumerable<SynthesizedIteratorTypeSymbol> GetSynthesizedIteratorTypes()
        => _synthesizedIterators.Values;

    private readonly struct DelegateSignature
    {
        public DelegateSignature(ImmutableArray<ITypeSymbol> parameterTypes, ImmutableArray<RefKind> refKinds, ITypeSymbol returnType)
        {
            ParameterTypes = parameterTypes;
            RefKinds = refKinds;
            ReturnType = returnType;
        }

        public ImmutableArray<ITypeSymbol> ParameterTypes { get; }

        public ImmutableArray<RefKind> RefKinds { get; }

        public ITypeSymbol ReturnType { get; }
    }

    private sealed class DelegateSignatureComparer : IEqualityComparer<DelegateSignature>
    {
        public bool Equals(DelegateSignature x, DelegateSignature y)
        {
            if (!SymbolEqualityComparer.Default.Equals(x.ReturnType, y.ReturnType))
                return false;

            if (x.ParameterTypes.Length != y.ParameterTypes.Length || x.RefKinds.Length != y.RefKinds.Length)
                return false;

            for (var i = 0; i < x.ParameterTypes.Length; i++)
            {
                if (x.RefKinds[i] != y.RefKinds[i])
                    return false;

                if (!SymbolEqualityComparer.Default.Equals(x.ParameterTypes[i], y.ParameterTypes[i]))
                    return false;
            }

            return true;
        }

        public int GetHashCode(DelegateSignature obj)
        {
            var hash = SymbolEqualityComparer.Default.GetHashCode(obj.ReturnType);

            for (var i = 0; i < obj.ParameterTypes.Length; i++)
            {
                hash = HashCode.Combine(
                    hash,
                    SymbolEqualityComparer.Default.GetHashCode(obj.ParameterTypes[i]),
                    obj.RefKinds[i]);
            }

            return hash;
        }
    }
}
