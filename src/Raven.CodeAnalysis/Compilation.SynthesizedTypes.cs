using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public partial class Compilation
{
    private readonly Dictionary<DelegateSignature, SynthesizedDelegateTypeSymbol> _synthesizedDelegates = new(new DelegateSignatureComparer());
    private readonly Dictionary<MethodSymbol, SynthesizedAsyncStateMachineTypeSymbol> _synthesizedAsyncStateMachines = new(SymbolEqualityComparer.Default);
    private readonly Dictionary<MethodSymbol, SynthesizedIteratorTypeSymbol> _synthesizedIterators = new(SymbolEqualityComparer.Default);
    private int _synthesizedDelegateOrdinal;
    private int _synthesizedAsyncStateMachineOrdinal;
    private int _synthesizedIteratorOrdinal;

    internal NamedTypeSymbol GetMethodReferenceDelegate(MethodSymbol methodSymbol)
    {
        var parameterTypes = methodSymbol.Parameters
            .Select(p => p.Type)
            .ToImmutableArray();
        var refKinds = methodSymbol.Parameters
            .Select(p => p.RefKind)
            .ToImmutableArray();

        return GetMethodReferenceDelegate(parameterTypes, refKinds, methodSymbol.ReturnType);
    }

    internal NamedTypeSymbol GetMethodReferenceDelegate(
        ImmutableArray<TypeSymbol> parameterTypes,
        ImmutableArray<RefKind> refKinds,
        TypeSymbol returnType)
    {
        if (refKinds.All(static refKind => refKind == RefKind.None))
        {
            var functionType = CreateFunctionTypeSymbol(parameterTypes.ToArray(), returnType);
            if (functionType is NamedTypeSymbol namedDelegate && namedDelegate.TypeKind == TypeKind.Delegate)
                return namedDelegate;
        }

        return GetOrAddSynthesizedDelegate(parameterTypes, refKinds, returnType);
    }

    internal IEnumerable<NamedTypeSymbol> GetSynthesizedDelegateTypes()
        => _synthesizedDelegates.Values;

    internal SynthesizedAsyncStateMachineTypeSymbol CreateAsyncStateMachine(
        MethodSymbol method,
        TypeSymbol? selfType = null)
    {
        if (_synthesizedAsyncStateMachines.TryGetValue(method, out var existing))
            return existing;

        existing = _synthesizedAsyncStateMachines.Values
            .FirstOrDefault(machine => SymbolEqualityComparer.Default.Equals(machine.AsyncMethod, method));

        if (existing is not null)
        {
            _synthesizedAsyncStateMachines[method] = existing;
            return existing;
        }

        var syntaxReference = method.DeclaringSyntaxReferences.FirstOrDefault();
        if (syntaxReference is not null)
        {
            existing = _synthesizedAsyncStateMachines.Values
                .FirstOrDefault(machine => machine.AsyncMethod.DeclaringSyntaxReferences
                    .Any(reference => reference.SyntaxTree == syntaxReference.SyntaxTree && reference.Span == syntaxReference.Span));

            if (existing is not null)
            {
                _synthesizedAsyncStateMachines[method] = existing;
                return existing;
            }
        }

        var name = $"<>c__AsyncStateMachine{_synthesizedAsyncStateMachineOrdinal++}";
        var stateMachine = new SynthesizedAsyncStateMachineTypeSymbol(this, method, name, selfType);
        _synthesizedAsyncStateMachines[method] = stateMachine;
        return stateMachine;
    }

    internal IEnumerable<SynthesizedAsyncStateMachineTypeSymbol> GetSynthesizedAsyncStateMachineTypes()
        => _synthesizedAsyncStateMachines.Values;

    internal SynthesizedIteratorTypeSymbol CreateIteratorStateMachine(
        MethodSymbol method,
        IteratorMethodKind iteratorKind,
        TypeSymbol elementType,
        TypeSymbol? selfType = null)
    {
        if (_synthesizedIterators.TryGetValue(method, out var existing) &&
            HasCompatibleIteratorSelfType(existing, selfType))
        {
            return existing;
        }

        existing = _synthesizedIterators.Values
            .FirstOrDefault(machine => SymbolEqualityComparer.Default.Equals(machine.IteratorMethod, method) &&
                                       HasCompatibleIteratorSelfType(machine, selfType));

        if (existing is not null)
        {
            _synthesizedIterators[method] = existing;
            return existing;
        }

        var syntaxReference = method.DeclaringSyntaxReferences.FirstOrDefault();
        if (syntaxReference is not null)
        {
            existing = _synthesizedIterators.Values
                .FirstOrDefault(machine => machine.IteratorMethod.DeclaringSyntaxReferences
                    .Any(reference => reference.SyntaxTree == syntaxReference.SyntaxTree && reference.Span == syntaxReference.Span) &&
                    HasCompatibleIteratorSelfType(machine, selfType));

            if (existing is not null)
            {
                _synthesizedIterators[method] = existing;
                return existing;
            }
        }

        var name = $"<>c__Iterator{_synthesizedIteratorOrdinal++}";
        var stateMachine = new SynthesizedIteratorTypeSymbol(this, method, name, iteratorKind, elementType, selfType);
        _synthesizedIterators[method] = stateMachine;
        return stateMachine;
    }

    internal IEnumerable<SynthesizedIteratorTypeSymbol> GetSynthesizedIteratorTypes()
        => _synthesizedIterators.Values;

    private static bool HasCompatibleIteratorSelfType(SynthesizedIteratorTypeSymbol iterator, TypeSymbol? selfType)
        => selfType is null ||
           iterator.ThisField is null ||
           SymbolEqualityComparer.Default.Equals(iterator.ThisField.Type, selfType);

    private NamedTypeSymbol GetOrAddSynthesizedDelegate(
        ImmutableArray<TypeSymbol> parameterTypes,
        ImmutableArray<RefKind> refKinds,
        TypeSymbol returnType)
    {
        if (_target.RuntimeContract.UsesStructuralFunctions)
            return new StructuralFunctionTypeSymbol(parameterTypes, refKinds, returnType,
                (NamedTypeSymbol)GetSpecialType(SpecialType.System_Object), this);

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

    private readonly struct DelegateSignature
    {
        public DelegateSignature(ImmutableArray<TypeSymbol> parameterTypes, ImmutableArray<RefKind> refKinds, TypeSymbol returnType)
        {
            ParameterTypes = parameterTypes;
            RefKinds = refKinds;
            ReturnType = returnType;
        }

        public ImmutableArray<TypeSymbol> ParameterTypes { get; }

        public ImmutableArray<RefKind> RefKinds { get; }

        public TypeSymbol ReturnType { get; }
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
