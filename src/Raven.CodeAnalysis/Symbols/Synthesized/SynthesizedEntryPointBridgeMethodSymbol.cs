using System;
using System.Collections.Immutable;

using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.Symbols;

sealed class SynthesizedEntryPointBridgeMethodSymbol : SourceMethodSymbol, IMethodSymbol
{
    public SynthesizedEntryPointBridgeMethodSymbol(
        SourceNamedTypeSymbol containingType,
        Location[] locations,
        SyntaxReference[] declaringSyntaxReferences,
        ITypeSymbol returnType,
        IMethodSymbol asyncImplementation)
        : base(
            name: "<Main>_EntryPoint",
            returnType: returnType,
            parameters: ImmutableArray<SourceParameterSymbol>.Empty,
            containingType,
            containingType,
            containingType.ContainingNamespace,
            locations,
            declaringSyntaxReferences,
            isStatic: true,
            methodKind: MethodKind.Ordinary)
    {
        AsyncImplementation = asyncImplementation;

        if (!asyncImplementation.Parameters.IsDefaultOrEmpty)
        {
            var parameters = ImmutableArray.CreateBuilder<SourceParameterSymbol>(asyncImplementation.Parameters.Length);
            foreach (var parameter in asyncImplementation.Parameters)
            {
                parameters.Add(new SourceParameterSymbol(
                    parameter.Name,
                    parameter.Type,
                    this,
                    containingType,
                    containingType.ContainingNamespace,
                    locations,
                    declaringSyntaxReferences,
                    parameter.RefKind));
            }

            SetParameters(parameters);
        }
    }

    public override bool IsStatic => true;

    public override bool IsImplicitlyDeclared => true;

    public IMethodSymbol AsyncImplementation { get; }
}
