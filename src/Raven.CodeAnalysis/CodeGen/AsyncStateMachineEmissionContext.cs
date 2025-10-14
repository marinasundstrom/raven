using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Reflection;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen;

internal readonly struct AsyncStateMachineEmissionContext
{
    private readonly ImmutableDictionary<SourceSymbol, ISymbol> _constructedMembers;

    public AsyncStateMachineEmissionContext(
        SourceMethodSymbol asyncMethod,
        SynthesizedAsyncStateMachineTypeSymbol.ConstructedStateMachine constructed)
    {
        AsyncMethod = asyncMethod ?? throw new ArgumentNullException(nameof(asyncMethod));
        Constructed = constructed;

        if (constructed.Definition is null)
            throw new ArgumentException("Constructed async state machine must reference its definition.", nameof(constructed));

        Definition = constructed.Definition;

        var map = ImmutableDictionary.CreateBuilder<SourceSymbol, ISymbol>(ReferenceEqualityComparer.Instance);

        map[Definition.StateField] = constructed.StateField;

        if (Definition.ThisField is { } thisField && constructed.ThisField is not null)
            map[thisField] = constructed.ThisField;

        map[Definition.BuilderField] = constructed.BuilderField;

        foreach (var parameter in Definition.AsyncMethod.Parameters)
        {
            if (!Definition.ParameterFieldMap.TryGetValue(parameter, out var sourceField))
                continue;

            if (!constructed.ParameterFieldMap.TryGetValue(parameter, out var constructedField))
                continue;

            map[sourceField] = constructedField;
        }

        map[Definition.Constructor] = constructed.Constructor;
        map[Definition.MoveNextMethod] = constructed.MoveNextMethod;
        map[Definition.SetStateMachineMethod] = constructed.SetStateMachineMethod;

        _constructedMembers = map.ToImmutable();
    }

    public SourceMethodSymbol AsyncMethod { get; }

    public SynthesizedAsyncStateMachineTypeSymbol Definition { get; }

    public SynthesizedAsyncStateMachineTypeSymbol.ConstructedStateMachine Constructed { get; }

    public ConstructedNamedTypeSymbol? ConstructedType => Constructed.Type as ConstructedNamedTypeSymbol;

    public ImmutableDictionary<ITypeParameterSymbol, ITypeParameterSymbol> MethodTypeParameterMap
        => Definition.MethodTypeParameterMap;

    public bool TryGetConstructedMember(SourceSymbol sourceSymbol, out ISymbol constructedSymbol)
    {
        if (sourceSymbol is null)
            throw new ArgumentNullException(nameof(sourceSymbol));

        return _constructedMembers.TryGetValue(sourceSymbol, out constructedSymbol);
    }

    public bool TryGetConstructedMemberInfo(SourceSymbol sourceSymbol, CodeGenerator codeGen, out MemberInfo? memberInfo)
    {
        memberInfo = null;

        if (!TryGetConstructedMember(sourceSymbol, out var constructed))
            return false;

        memberInfo = constructed switch
        {
            IFieldSymbol field => field.GetFieldInfo(codeGen),
            IMethodSymbol method when method.IsConstructor => method.GetClrConstructorInfo(codeGen),
            IMethodSymbol method => method.GetClrMethodInfo(codeGen),
            _ => null
        };

        return memberInfo is not null;
    }
}
